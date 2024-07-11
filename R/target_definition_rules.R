#_______________________________________________________________________________
#----                     target_definition_rules class                     ----
#_______________________________________________________________________________

#' 
#' Target definition rules class.
#' 
#' @export
setClass(
  "target_definition_rules",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="target_definition_rule")
)

#' Create a 'Rules' object.
#' 
#' @param ... any rules
#' @export
Rules <- function(...) {
  extraArgs <- list(...)
  retValue <- new("target_definition_rules") %>%
    add(extraArgs)
  return(retValue)
}
