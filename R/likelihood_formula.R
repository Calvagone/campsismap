
#' Population likelihood (how far the individual parameters deviate from 0).
#' 
#' @param model Campsismap model
#' @param etas unnamed numeric vector
#' @return population likelihood
#' @importFrom mvtnorm dmvnorm
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
  # Simulate
  if (length(samples) > 0) {
    results <- predict(object=model@model_cache, dataset=dataset, etas=etas, settings=model@settings)
    
    ipred <- results %>% dplyr::pull(model@variable)
    ipredTimes <- results$TIME
    dv <- samples$DV
    dvTimes <- samples$TIME
    
    # Make sure times match...
    assertthat::assert_that(length(dv)==nrow(results), msg="dv and results do not have the same number of observations")
    assertthat::assert_that(all(abs(ipredTimes-dvTimes) < 1e-6), msg="times in dv and results do not match")
    
    sd <- model@error %>% computeSd(x=ipred)
    return(sum(dnorm(x=ipred, mean=dv, sd=sd, log=TRUE)))
  } else {
    return(0)
  }
}


