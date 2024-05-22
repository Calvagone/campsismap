#_______________________________________________________________________________
#----                          shiny_input class                            ----
#_______________________________________________________________________________

#' Shiny input class.
#' 
#' @export
setClass(
  "shiny_input",
  representation(
    id="character",
    value="ANY",
    transient="logical",
    ns="function"
  ),
  contains="pmx_element"
)

#' 
#' Create a new shiny input.
#' 
#' @param id input identifier
#' @param value input default value
#' @param transient transient input (i.e. not persisted)
#' @param ns namespace
#' @return a shiny input
#' @export
shinyInput <- function(id, value, transient=FALSE, ns=function(x){x}) {
  return(new("shiny_input", id=id, value=value, transient=transient, ns=ns))
}

#_______________________________________________________________________________
#----                               getName                                 ----
#_______________________________________________________________________________

setMethod("getName", signature=c("shiny_input"), definition=function(x) {
  return(x@id)
})

#_______________________________________________________________________________
#----                               getID                                   ----
#_______________________________________________________________________________

#' @rdname getID
setMethod("getID", signature=c("shiny_input"), definition=function(object) {
  return(object@id)
})

