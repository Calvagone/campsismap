#_______________________________________________________________________________
#----                      dose_adaptation_rules class                      ----
#_______________________________________________________________________________

#' 
#' Dose adaptation rules class.
#' 
#' @export
setClass(
  "dose_adaptation_rules",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="dose_adaptation_rule")
)

#' Create a 'Rules' object.
#' 
#' @param ... any rules
#' @export
Rules <- function(...) {
  extraArgs <- list(...)
  retValue <- new("dose_adaptation_rules") %>%
    add(extraArgs)
  return(retValue)
}
