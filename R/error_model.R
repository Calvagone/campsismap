
#_______________________________________________________________________________
#----                        error_model class                              ----
#_______________________________________________________________________________

#' Error model class.
#' 
#' @slot fun function to compute the standard deviation of the model
#' @export
setClass(
  "error_model",
  representation(
    fun="function"
  )
)

#_______________________________________________________________________________
#----                    proportional_error_model class                     ----
#_______________________________________________________________________________

#' Proportional error model class.
#' 
#' @slot prop standard deviation of proportional error, numeric
#' @export
setClass(
  "proportional_error_model",
  representation(
    prop="numeric"
  ),
  contains="error_model"
)

#' Proportional error model.
#' 
#' @slot prop standard deviation of proportional error, numeric
#' @export
ProportionalErrorModel <- function(prop) {
  fun <- function(ipred, prop) {
    return(abs(prop*ipred))
  }
  return(new("proportional_error_model", fun=fun, prop=prop))
}

#' @rdname computeSd
setMethod("computeSd", signature("proportional_error_model", "numeric"), function(object, ipred) {
  return(object@fun(ipred=ipred, prop=object@prop))
})
