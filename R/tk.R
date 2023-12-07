#' Ask user
#'
#' Function to ask item name to user. This function is used internally. Based on https://stackoverflow.com/questions/16847621/get-data-out-of-a-tcltk-function
#'
#' @return Prompt user and return value.
#' @import tcltk
#' @export
item.name.fun <- function(){

  xvar <- tclVar("")

  tt <- tktoplevel()
  tkwm.title(tt,"Input Numbers")
  x.entry <- tkentry(tt, textvariable=xvar)

  reset <- function()
  {
    tclvalue(xvar)<-""
  }

  reset.but <- tkbutton(tt, text="Reset", command=reset)

  submit <- function() {
    x <- (tclvalue(xvar))
    e <- parent.env(environment())
    e$x <- x
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)

  tkgrid(tklabel(tt,text="Enter item name:"), x.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)

  tkwait.window(tt)
  return(c(x))
}


