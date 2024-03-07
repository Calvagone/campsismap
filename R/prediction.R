
#' Individual predictions calculation.
#' 
#' @param model Campsismap model
#' @param dataset Campsis dataset
#' @param etas unnamed numeric vector
#' @return a dataframe with 3 columns: TIME, <VARIABLE>, DV
#' @importFrom dplyr all_of transmute
#' @importFrom tibble tibble
#' @export
individualPrediction <- function(model, dataset, etas) {
  times <- getObservationTimes(dataset)
  etaNames <- model@eta_names
  assertthat::are_equal(length(etaNames), length(etas))
  
  if (length(times) > 0) {
    for (index in seq_along(etaNames)) {
      dataset <- dataset %>%
        add(Covariate(etaNames[index], etas[index]))
    }
    results <- simulate(model=model@model, dataset=dataset, dest="mrgsolve", seed=0,
                        outvars=c(model@variable, "DV"), settings=Settings(Declare("DV")))
    observations <- results %>%
      obsOnly() %>%
      dplyr::select(dplyr::all_of(c("TIME", model@variable, "DV")))
    assertthat::are_equal(length(times), nrow(observations))
  } else {
    observations <- tibble::tibble(TIME=numeric(0), !!model@variable:=numeric(0), DV=numeric(0))
  }
  return(observations)
}
