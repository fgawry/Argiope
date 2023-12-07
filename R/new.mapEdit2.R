#' Interactively Edit a Map
#'
#' This is exactly the editMap function, but changed to allow loops. Solution by https://github.com/r-spatial/mapedit/issues/83
#'
#' @param x \code{leaflet} or \code{mapview} map to edit
#' @param ... other arguments for \code{leafem::addFeatures()} when
#'          using \code{editMap.NULL} or \code{selectFeatures}
#'
#' @return \code{sf} simple features or \code{GeoJSON}
#'
#' @export
editMap2 <- function(x, ...) {
  UseMethod("editMap2")
}

#' @name editMap2
#' @param targetLayerId \code{string} name of the map layer group to use with edit
#' @param sf \code{logical} return simple features.  The default is \code{TRUE}.
#'          If \code{sf = FALSE}, \code{GeoJSON} will be returned.
#' @param ns \code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
#'          is unlikely to require a change.
#' @param record \code{logical} to record all edits for future playback.
#' @param viewer \code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
#'          NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
#'          open the app in the default browser, the browser window will automatically
#'          close when closing the app (by pressing "done" or "cancel") in most browsers.
#'          Firefox is an exception. See Details for instructions on how to enable this
#'          behaviour in Firefox.
#' @param crs see \code{\link[sf]{st_crs}}.
#' @param title \code{string} to customize the title of the UI window.  The default
#'          is "Edit Map".
#' @param editor \code{character} either "leaflet.extras" or "leafpm"
#'
#' @details
#'   When setting \code{viewer = browserViewer(browser = getOption("browser"))} and
#'   the systems default browser is Firefox, the browser window will likely not
#'   automatically close when the app is closed (by pressing "done" or "cancel").
#'   To enable automatic closing of tabs/windows in Firefox try the following:
#'   \itemize{
#'     \item{input "about:config " to your firefox address bar and hit enter}
#'     \item{make sure your "dom.allow_scripts_to_close_windows" is true}
#'   }
#'
#' @export
editMap2.leaflet <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "leaflet"))

  stopifnot(
    requireNamespace("leaflet"),
    requireNamespace("leaflet.extras"),
    requireNamespace("shiny"),
    requireNamespace("miniUI")
  )

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      editModUI(id = ns, height="97%"),
      height=NULL, width=NULL
    ),
    miniUI::gadgetTitleBar(
      title = title,
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    tags$script(HTML(
      "
// close browser window on session end
$(document).on('shiny:disconnected', function() {
  // check to make sure that button was pressed
  //  to avoid websocket disconnect caused by some other reason than close
  if(
    Shiny.shinyapp.$inputValues['cancel:shiny.action'] ||
    Shiny.shinyapp.$inputValues['done:shiny.action']
  ) {
    window.close()
  }
})
"
    ))
  )

  server <- function(input, output, session) {
    crud <- callModule(
      editMod,
      ns,
      x,
      targetLayerId = targetLayerId,
      sf = sf,
      record = record,
      crs = crs,
      editor = editor
    )





    observe({crud()})

    shiny::observeEvent(input$done, {
      crud()
      session$close()
    })

    #shiny::observeEvent(input$done, {
    #  shiny::stopApp(
    #    crud()
    #  )
    #})

    # if browser viewer and user closes tab/window
    #  then Shiny does not stop so we will stopApp
    #  when a session ends.  This works fine unless a user might
    #  have two sessions open.  Closing one will also close the
    #  other.
    session$onSessionEnded(function() {
      # should this be a cancel where we send NULL
      #  or a done where we send crud()
      shiny::stopApp(isolate(crud()))
    })

    shiny::observeEvent(input$cancel, { shiny::stopApp (NULL) })
  }

  shiny::runGadget(
    ui,
    server,
    viewer =  viewer,
    stopOnCancel = FALSE
  )
}

#' @name editMap2
#' @export
editMap2.mapview <- function(
  x = NULL, targetLayerId = NULL, sf = TRUE,
  ns = "mapedit-edit", record = FALSE, viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
) {
  stopifnot(!is.null(x), inherits(x, "mapview"), inherits(x@map, "leaflet"))

  editMap2.leaflet(
    x@map, targetLayerId = targetLayerId, sf = sf,
    ns = ns, viewer = viewer, record = TRUE, crs = crs,
    title = title,
    editor = editor
  )
}

#' @name editMap2
#' @export
editMap2.NULL = function(x, editor = c("leaflet.extras", "leafpm"), ...) {
  m = mapview::mapview()@map
  m = leaflet::fitBounds(
    m,
    lng1 = -180, #as.numeric(sf::st_bbox(x)[1]),
    lat1 = -90, #as.numeric(sf::st_bbox(x)[2]),
    lng2 = 180, #as.numeric(sf::st_bbox(x)[3]),
    lat2 = 90 #as.numeric(sf::st_bbox(x)[4])
  )
  ed = editMap2(m, record = TRUE, editor = editor)
  ed_record <- ed$finished
  attr(ed_record, "recorder") <- attr(ed, "recorder", exact = TRUE)
  ed_record
}
