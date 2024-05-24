
#_______________________________________________________________________________
#----                      campsismap_model class                           ----
#_______________________________________________________________________________

#' Campsismap model class.
#' 
#' @slot model Campsis model ready for estimation
#' @slot omega pre-computed OMEGA matrix
#' @slot eta_names ETA names in model
#' @slot variable variable name representing the predictions in model
#' @slot error error model
#' @slot model_cache model cache
#' @slot settings simulation settings
#' @slot dest destination engine
#' @export
setClass(
  "campsismap_model",
  representation(
    model="campsis_model",
    omega="matrix",
    eta_names="character",
    variable="character",
    error="error_model",
    model_cache="model_cache",
    settings="simulation_settings",
    dest="simulation_engine"
  ),
  prototype=prototype(error=UndefinedErrorModel())
)

#' Create a new Campsismap model.
#' 
#' @param model Campsis model
#' @param variable variable of the concentration in model
#' @return a Campsismap mode
#' @export
CampsismapModel <- function(model, variable) {
  # Derive OMEGA matrix
  omega <- rxodeMatrix(model)
  
  # Store ETA names once for all and delete names in OMEGA matrix
  eta_names <- colnames(omega)
  colnames(omega) <- NULL
  rownames(omega) <- NULL

  concEquation <- model %>%
    find(Equation(variable))
  if (is.null(concEquation)) {
    stop(sprintf("Could not find equation '%s' in model code"), variable)
  }
  
  model <- model %>%
    delete(ErrorRecord()) %>%
    delete(concEquation)
  
  # Append concentration equation to the ODE record 
  model <- model %>%
    add(concEquation, pos=campsismod::Position(OdeRecord()))
  
  # Discard omegas and sigmas
  model@parameters@list <- model@parameters@list %>%
    purrr::discard(~is(.x, "double_array_parameter"))

  return(new("campsismap_model", model=model, omega=omega, eta_names=eta_names, variable=variable))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("campsismap_model", "error_model"), definition = function(object, x) {
  object@error <- x 
  return(object)
})

#_______________________________________________________________________________
#----                             setup                                     ----
#_______________________________________________________________________________

#' @rdname setup
setMethod("setup", signature=c("campsismap_model", "character", "simulation_settings"), definition = function(object, dest, settings) {
  
  # Settings
  settings <- preprocessSettings(settings, dest=dest)
  
  if (dest=="mrgsolve") {
    dest_ <- new("mrgsolve_engine")
    model_cache <- MrgsolveModelCache(model=object@model, variable=object@variable, eta_names=object@eta_names, settings=settings)
  } else if (dest=="rxode2") {
    dest_ <- new("rxode_engine")
    model_cache <- Rxode2ModelCache(model=object@model, variable=object@variable, eta_names=object@eta_names, settings=settings)
  } else {
    stop("Engine not supported")
  }
  
  object@dest <- dest_
  object@model_cache <- model_cache
  object@settings <- settings
  
  return(object)
})

#_______________________________________________________________________________
#----                           predict                               ----
#_______________________________________________________________________________

#' @rdname predict
setMethod("predict", signature("campsismap_model", "dataset", "numeric", "simulation_settings"), function(object, dataset, etas, settings, ...) {
  datasetTbl <- dataset %>%
    export(dest=object@dest, seed=1, model=NULL, settings=settings)
  
  return(predict(object=object@model_cache, dataset=datasetTbl, etas=etas, settings=settings, ...))
})


checkModelReady <- function(object, raise_error=TRUE) {
  if (is.null(object@model_cache@mod)) {
    if (raise_error) {
      stop("Please setup your model first. See ?setup.")
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

