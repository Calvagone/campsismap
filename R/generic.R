
#_______________________________________________________________________________
#----                              addDV                                    ----
#_______________________________________________________________________________

#' Add information related to the dependent variable (DV), i.e. add the observed samples.
#' 
#' @param object generic object
#' @param x data frame with 2 columns: TIME and DV
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname addDV
addDV <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("addDV", function(object, x, ...) {
  standardGeneric("addDV")
})

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

#_______________________________________________________________________________
#----                            estimate                                   ----
#_______________________________________________________________________________

#' Estimate.
#' 
#' @param model Campsismap model
#' @param dataset predicted values, numeric vector
#' @param etas initial etas vector, default will be all etas 0
#' @param ... extra arguments
#' @return estimated etas, numeric vector
#' @export
#' @rdname estimate
estimate <- function(model, dataset, etas, ...) {
  stop("No default function is provided")
}

setGeneric("estimate", function(model, dataset, etas=NULL, ...) {
  if (is.null(etas)) etas <- numeric()
  standardGeneric("estimate")
})

#_______________________________________________________________________________
#----                              getDV                                    ----
#_______________________________________________________________________________

#' Get information related to the dependent variable (DV), i.e. add the observed samples.
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return tibble with the samples (TIME and DV)
#' @export
#' @rdname getDV
getDV <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getDV", function(object, ...) {
  standardGeneric("getDV")
})

#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' Quickly visualise the individual predictions with regard to the observed data.
#' 
#' @param model Campsismap or Campsis model
#' @param dataset Campsis dataset
#' @param etas individual parameters
#' @param pop add population reference
#' @param ... extra arguments
#' @return a ggplot2 object
#' @export
#' @rdname quickPlot
quickPlot <- function(model, dataset, etas, pop, ...) {
  stop("No default function is provided")
}

setGeneric("quickPlot", function(model, dataset, etas=NULL, pop=NULL, ...) {
  if (is.null(pop)) pop <- TRUE
  if (is.null(etas)) {
    pop <- FALSE
    etas <- numeric()
  } 
  standardGeneric("quickPlot")
})

#_______________________________________________________________________________
#----                           initCache                                  ----
#_______________________________________________________________________________

#' Init the model cache.
#' 
#' @param object model cache
#' @param model original model
#' @param settings simulation settings
#' @param ... extra arguments
#' @return updated model
#' @export
#' @rdname initCache
initCache <- function(object, model, settings, ...) {
  stop("No default function is provided")
}

setGeneric("initCache", function(object, model, settings, ...) {
  standardGeneric("initCache")
})

#_______________________________________________________________________________
#----                             setup                                     ----
#_______________________________________________________________________________

#' Setup campsismap object by caching the model.
#' 
#' @param object campsismap object
#' @param dest destination engine
#' @param settings simulation settings
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname setup
setup <- function(object, dest, settings, ...) {
  stop("No default function is provided")
}

setGeneric("setup", function(object, dest, settings=NULL, ...) {
  if (is.null(settings)) settings <- Settings()
  standardGeneric("setup")
})

#_______________________________________________________________________________
#----                           simulateModel                               ----
#_______________________________________________________________________________

#' Simulate model.
#' 
#' @param object model cache
#' @param dataset dataset
#' @param etas individual parameters to simulate
#' @param settings simulation settings
#' @param ... extra arguments
#' @return updated model
#' @export
#' @rdname simulateModel
simulateModel <- function(object, dataset, etas, settings, ...) {
  stop("No default function is provided")
}

setGeneric("simulateModel", function(object, dataset, etas=NULL, settings=NULL, ...) {
  if (is.null(etas)) etas <- numeric()
  if (is.null(settings) && is(object, "campsismap_model")) settings <- object@settings
  standardGeneric("simulateModel")
})
