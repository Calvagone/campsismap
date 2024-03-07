
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
#' @param prop standard deviation of proportional error, numeric
#' @return an error model
#' @export
ProportionalErrorModel <- function(prop) {
  fun <- function(x, prop) {
    return(abs(prop*x))
  }
  return(new("proportional_error_model", fun=fun, prop=prop))
}

#' @rdname computeSd
setMethod("computeSd", signature("proportional_error_model", "numeric"), function(object, x) {
  return(object@fun(x=x, prop=object@prop))
})

#_______________________________________________________________________________
#----                      combined_error_model class                       ----
#_______________________________________________________________________________

#' Combined error model class.
#' 
#' @slot prop standard deviation of proportional error, numeric
#' @slot add standard deviation of additive error, same unit as observed variable, numeric
#' @export
setClass(
  "combined_error_model",
  representation(
    prop="numeric",
    add="numeric"
  ),
  contains="error_model"
)

#' Combined error model.
#' 
#' @param prop standard deviation of proportional error, numeric
#' @param add standard deviation of additive error, same unit as observed variable, numeric
#' @return an error model
#' @export
CombinedErrorModel <- function(prop, add) {
  fun <- function(x, prop, add) {
    return(sqrt(((prop*x)^2) + add^2))
  }
  return(new("combined_error_model", fun=fun, prop=prop, add=add))
}

#' @rdname computeSd
setMethod("computeSd", signature("combined_error_model", "numeric"), function(object, x) {
  return(object@fun(x=x, prop=object@prop, add=object@add))
})
