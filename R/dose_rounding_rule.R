#_______________________________________________________________________________
#----                       dose_rounding_rule class                        ----
#_______________________________________________________________________________

#' Dose rounding rule class.
#' 
#' @export
setClass(
  "dose_rounding_rule",
  representation(
    fun = "function"
  ),
  contains="dose_adaptation_rule"
)

#' Create a dose rounding rule.
#' 
#' @param fun purrr-style lambda function used for rounding doses, first argument is the dose amount to round
#' @importFrom rlang as_function
#' @export
DoseRoundingRule <- function(fun=~round(.x)) {
  fun <- rlang::as_function(fun)
  class(fun) <- "function" # Cast needed to work with S4 class system
  return(new("dose_rounding_rule", fun=fun))
}

#_______________________________________________________________________________
#----                               getName                                 ----
#_______________________________________________________________________________

setMethod("getName", signature=c("dose_rounding_rule"), definition=function(x) {
  return("Dose rounding rule")
})
