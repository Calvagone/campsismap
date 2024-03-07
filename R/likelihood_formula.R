
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
#' @param dataset Campsis dataset
#' @param etas unnamed numeric vector
#' @return population likelihood
#' @export
individualLikelihood <- function(model, dataset, etas) {
  times <- getObservationTimes(dataset)
  etaNames <- model@eta_names
  assertthat::are_equal(length(etaNames), length(etas))
  
  if (length(times) > 0) {
    for (index in seq_along(etaNames)) {
      dataset <- dataset %>%
        add(Covariate(etaNames[index], etas[index]))
    }
    results <- simulate(model=model@model, dataset=dataset, dest="mrgsolve", seed=0, outvars=c(model@variable, "DV"))
    observations <- results %>%
      obsOnly() %>%
      dplyr::transmute(dplyr::all_of(c("TIME", model@variable, "DV")))
    assertthat::are_equal(length(times), nrow(observations))
  } else {
    observations <- tibble::tibble(TIME=numeric(0), !!model@variable:=numeric(0), DV=numeric(0))
  }

  return(observations)
}

getObservationTimes <- function(dataset) {
  return(dataset@arms@list[[1]]@protocol@observations %>% getTimes())
}
