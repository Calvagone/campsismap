
#_______________________________________________________________________________
#----                           computeSd                                   ----
#_______________________________________________________________________________

#' Compute standard deviation of error model.
#' 
#' @param object error model
#' @param ipred individual predicted values
#' @param ... extra arguments
#' @return the standard deviation
#' @export
#' @rdname computeSd
computeSd <- function(object, ipred, ...) {
  stop("No default function is provided")
}

setGeneric("computeSd", function(object, ipred, ...) {
  standardGeneric("computeSd")
})
