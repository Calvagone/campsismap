
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
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' Quickly visualise the individual predictions with regard to the observed data.
#' 
#' @param model Campsismap or Campsis model
#' @param dataset Campsis dataset
#' @param etas individual parameters
#' @param ... extra arguments
#' @return a ggplot2 object
#' @export
#' @rdname quickPlot
quickPlot <- function(model, dataset, etas, ...) {
  stop("No default function is provided")
}

setGeneric("quickPlot", function(model, dataset, etas=NULL, ...) {
  if (is.null(etas)) etas <- numeric()
  standardGeneric("quickPlot")
})
