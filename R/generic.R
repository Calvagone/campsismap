
#_______________________________________________________________________________
#----                           computeSd                                   ----
#_______________________________________________________________________________

#' Compute standard deviation of error model.
#' 
#' @param object error model
#' @param x predicted values, numeric vector
#' @param ... extra arguments
#' @return the standard deviation
#' @export
#' @rdname computeSd
computeSd <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("computeSd", function(object, x, ...) {
  standardGeneric("computeSd")
})
