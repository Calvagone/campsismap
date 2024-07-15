
#_______________________________________________________________________________
#----                           addSamples                                  ----
#_______________________________________________________________________________

#' Add the observed samples.
#' 
#' @param object generic object
#' @param x data frame with 2 columns: TIME and DV
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname addSamples
addSamples <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("addSamples", function(object, x, ...) {
  standardGeneric("addSamples")
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
#----                           getSamples                                  ----
#_______________________________________________________________________________

#' Get the observed samples.
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return tibble with the samples (TIME and DV)
#' @export
#' @rdname getSamples
getSamples <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getSamples", function(object, ...) {
  standardGeneric("getSamples")
})

#_______________________________________________________________________________
#----                       getSimulationTimes                              ----
#_______________________________________________________________________________

#' Get the simulation times (by opposition to the samples).
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return a numeric vector with the simulation times
#' @export
#' @rdname getSimulationTimes
getSimulationTimes <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getSimulationTimes", function(object, ...) {
  standardGeneric("getSimulationTimes")
})

#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' Quickly visualise the individual predictions with regard to the observed data.
#' 
#' @param model Campsismap or Campsis model
#' @param dataset Campsis dataset
#' @param etas individual parameters
#' @param plot plot type
#' @param options plot options
#' @param ... extra arguments
#' @return a ggplot2 object
#' @export
#' @rdname quickPlot
quickPlot <- function(model, dataset, etas, plot, options, ...) {
  stop("No default function is provided")
}

setGeneric("quickPlot", function(model, dataset=NULL, etas=NULL, plot=NULL, options=NULL, ...) {
  if (is.null(etas)) {
    etas <- numeric()
  }
  if (is.null(options)) {
    options <- PlotDisplayOptions()
  }
  args <- list(...)
  if (is.null(dataset) && !is.null(args$recommendation)) {
    dataset <- args$recommendation@original_dataset
  }
  if (is.null(plot)) {
    if (is.null(args$recommendation)) {
      plot <- IndividualFitPlotType()
    } else {
      plot <- RecommendationPlotType()
    }
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
#----                                 read                                  ----
#_______________________________________________________________________________

#' Generic load method to load data from a file or a folder.
#' 
#' @param object object to load with something 
#' @param file path to the file or folder to be read
#' @param ... extra arguments
#' @return the loaded object
#' @export
load <- function(object, file, ...) {
  stop("No default function is provided")
}

setGeneric("load", function(object, file, ...) {
  standardGeneric("load")
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
#----                              predict                                  ----
#_______________________________________________________________________________

#' Predict.
#' 
#' @param object campsismap object or model cache
#' @param dataset dataset
#' @param etas individual parameters to simulate
#' @param settings simulation settings
#' @param ... extra arguments
#' @return updated model
#' @export
#' @rdname predict
predict <- function(object, dataset, etas, settings, ...) {
  stop("No default function is provided")
}

setGeneric("predict", function(object, dataset, etas=NULL, settings=NULL, ...) {
  if (is.null(etas)) etas <- numeric()
  if (is.null(settings) && is(object, "campsismap_model")) settings <- object@settings
  standardGeneric("predict")
})

#_______________________________________________________________________________
#----                             recommend                                 ----
#_______________________________________________________________________________

#' Recommend.
#' 
#' @param object campsismap object or model cache
#' @param dataset dataset
#' @param etas individual parameters to simulate
#' @param target target definition
#' @param now defines what the future or the past is, default is 0 (all doses adapted)
#' @param rules dose adaptation rules
#' @param settings simulation settings
#' @param ... extra arguments
#' @return recommendations
#' @export
#' @rdname recommend
recommend <- function(object, dataset, etas, target, now, rules, settings, ...) {
  stop("No default function is provided")
}

setGeneric("recommend", function(object, dataset, etas=NULL, target=NULL, now=NULL, rules=NULL, settings=NULL, ...) {
  if (is.null(etas)) etas <- numeric()
  if (is.null(rules)) rules <- Rules()
  if (is.null(settings) && is(object, "campsismap_model")) settings <- object@settings
  if (is.null(now)) now <- as.numeric(0)
  standardGeneric("recommend")
})

#_______________________________________________________________________________
#----                       updateDoseAmount                                ----
#_______________________________________________________________________________

#' Update dose amount.
#' 
#' @param object generic object (e.g. dataset)
#' @param amount updated dose amount
#' @param dose_number corresponding dose number
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname updateDoseAmount
updateDoseAmount <- function(object, amount, dose_number, ...) {
  stop("No default function is provided")
}

setGeneric("updateDoseAmount", function(object, amount, dose_number, ...) {
  dose_number <- as.integer(dose_number)
  standardGeneric("updateDoseAmount")
})

#_______________________________________________________________________________
#----                     retrieveDoseAmount                                ----
#_______________________________________________________________________________

#' Retrieve dose amount.
#' 
#' @param object generic object (e.g. dataset)
#' @param dose_number corresponding dose number
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname retrieveDoseAmount
retrieveDoseAmount <- function(object, dose_number, ...) {
  stop("No default function is provided")
}

setGeneric("retrieveDoseAmount", function(object, dose_number, ...) {
  dose_number <- as.integer(dose_number)
  standardGeneric("retrieveDoseAmount")
})
