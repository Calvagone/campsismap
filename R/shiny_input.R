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
    ns="character" # Namespace, string form
  ),
  contains="pmx_element"
)

#' 
#' Create a new shiny input.
#' 
#' @param id input identifier
#' @param value input default value
#' @param transient transient input (i.e. not persisted)
#' @return a shiny input
#' @export
shinyInput <- function(id, value, transient=FALSE) {
  return(new("shiny_input", id=id, value=value, transient=transient))
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

