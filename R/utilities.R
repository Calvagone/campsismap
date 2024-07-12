#' Preprocess the simulation settings.
#' 
#' @param settings simulation settings
#' @param dest destination engine
#' @return updated simulation settings
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessSettings <- function(settings, dest) {
  # Use default settings if not specified
  if (is.null(settings)) {
    settings <- Settings()
  }
  
  # Check if NOCB is specified
  enable <- settings@nocb@enable
  if (is.na(enable)) {
    if (dest=="mrgsolve") {
      enable <- TRUE
    } else {
      enable <- FALSE
    }
  }
  # Assign final value
  settings@nocb@enable <- enable
  
  # Preprocess slice_size
  settings@hardware@slice_size <- as.integer(1)
  
  return(settings)
}

#' Test equality between two S4 objects.
#' 
#' @param x first object
#' @param y second object
#' @export
s4Eq <- function(x, y) {
  return(isTRUE(all.equal(x, y)))
}

checkModelReady <- function(object, check_error_model=TRUE, raise_error=TRUE) {
  # Check error model (if requested)
  if (check_error_model) {
    if (is(object@error, class(UndefinedErrorModel()))) {
      if (raise_error) {
        stop("No error model configured. Please add one.")
      } else {
        return(FALSE)
      }
    }
  }
  
  # Check if model is cached
  if (is.null(object@model_cache@mod)) {
    if (raise_error) {
      stop("Please setup your model first. See ?setup.")
    } else {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Initialise the ETA vector based on the Campsismap model.
#' ETA vector is initialised only if it has length 0.
#' 
#' @param etas given ETA vector
#' @param model Campsismap model
#' @return initiated ETA vector
#' @importFrom assertthat assert_that
initialiseEtaVector <- function(etas, model) {
  assertthat::assert_that(is(model, "campsismap_model"), msg="model is not a campsismap model")
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  return(etas)
}
