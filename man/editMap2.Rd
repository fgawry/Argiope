% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new.mapEdit2.R
\name{editMap2}
\alias{editMap2}
\alias{editMap2.leaflet}
\alias{editMap2.mapview}
\alias{editMap2.NULL}
\title{Interactively Edit a Map}
\usage{
editMap2(x, ...)

\method{editMap2}{leaflet}(
  x = NULL,
  targetLayerId = NULL,
  sf = TRUE,
  ns = "mapedit-edit",
  record = FALSE,
  viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
)

\method{editMap2}{mapview}(
  x = NULL,
  targetLayerId = NULL,
  sf = TRUE,
  ns = "mapedit-edit",
  record = FALSE,
  viewer = shiny::paneViewer(),
  crs = 4326,
  title = "Edit Map",
  editor = c("leaflet.extras", "leafpm"),
  ...
)

\method{editMap2}{`NULL`}(x, editor = c("leaflet.extras", "leafpm"), ...)
}
\arguments{
\item{x}{\code{leaflet} or \code{mapview} map to edit}

\item{...}{other arguments for \code{leafem::addFeatures()} when
using \code{editMap.NULL} or \code{selectFeatures}}

\item{targetLayerId}{\code{string} name of the map layer group to use with edit}

\item{sf}{\code{logical} return simple features.  The default is \code{TRUE}.
If \code{sf = FALSE}, \code{GeoJSON} will be returned.}

\item{ns}{\code{string} name for the Shiny \code{namespace} to use.  The \code{ns}
is unlikely to require a change.}

\item{record}{\code{logical} to record all edits for future playback.}

\item{viewer}{\code{function} for the viewer.  See Shiny \code{\link[shiny]{viewer}}.
NOTE: when using \code{browserViewer(browser = getOption("browser"))} to
open the app in the default browser, the browser window will automatically
close when closing the app (by pressing "done" or "cancel") in most browsers.
Firefox is an exception. See Details for instructions on how to enable this
behaviour in Firefox.}

\item{crs}{see \code{\link[sf]{st_crs}}.}

\item{title}{\code{string} to customize the title of the UI window.  The default
is "Edit Map".}

\item{editor}{\code{character} either "leaflet.extras" or "leafpm"}
}
\value{
\code{sf} simple features or \code{GeoJSON}
}
\description{
This is exactly the editMap function, but changed to allow loops. Solution by https://github.com/r-spatial/mapedit/issues/83
}
\details{
When setting \code{viewer = browserViewer(browser = getOption("browser"))} and
  the systems default browser is Firefox, the browser window will likely not
  automatically close when the app is closed (by pressing "done" or "cancel").
  To enable automatic closing of tabs/windows in Firefox try the following:
  \itemize{
    \item{input "about:config " to your firefox address bar and hit enter}
    \item{make sure your "dom.allow_scripts_to_close_windows" is true}
  }
}
