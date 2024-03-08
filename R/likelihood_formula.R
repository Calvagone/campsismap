
#' Population likelihood (how far the individual parameters deviate from 0).
#' 
#' @param model Campsismap model
#' @param etas unnamed numeric vector
#' @return population likelihood
#' @export
populationLikelihood <- function(model, etas) {
  retValue <- mvtnorm::dmvnorm(x=etas, mean=rep(0, length(etas)), sigma=model@omega, log=TRUE)
  return(sum(retValue))
}

#' Individual likelihood (how far the individual concentrations deviate from the observations, weighted by the error model).
#' 
#' @param model Campsismap model
#' @param dataset exported dataset
#' @param samples individual samples
#' @param etas unnamed numeric vector
#' @return population likelihood
#' @export
individualLikelihood <- function(model, dataset, samples, etas) {
  # Retrieve error model
  error <- model@error
  if (is(error, class(UndefinedErrorModel()))) {
    stop("No error model configured. Please add one.")
  }

  # Simulate
  if (length(samples) > 0) {
    results <- individualPrediction(model=model, dataset=dataset, etas=etas)
    
    ipred <- results %>% dplyr::pull(model@variable)
    ipredTimes <- results$TIME
    dv <- samples$DV
    dvTimes <- samples$TIME
    
    # Make sure times match...
    assertthat::assert_that(length(dv)==nrow(results), msg="dv and results do not have the same number of observations")
    assertthat::assert_that(all(abs(ipredTimes-dvTimes) < 1e-6), msg="times in dv and results do not match")
    
    sd <- error %>% computeSd(x=ipred)
    return(dnorm(x=ipred, mean=dv, sd=sd, log=TRUE))
  } else {
    return(0)
  }
}


