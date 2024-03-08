
#' Individual predictions calculation.
#' 
#' @param model Campsismap model
#' @param dataset exported Campsis dataset
#' @param etas unnamed numeric vector
#' @return a dataframe with 3 columns: TIME, <VARIABLE>, DV
#' @importFrom dplyr all_of transmute
#' @importFrom tibble tibble
#' @export
individualPrediction <- function(model, dataset, etas) {
  times <- 1 # TO DO
  etaNames <- model@eta_names
  assertthat::are_equal(length(etaNames), length(etas))
  modelCache <- model@model_cache
  
  if (length(times) > 0) {
    results <- simulateModel(object=modelCache, dataset=dataset, settings=model@settings)
    observations <- results %>%
      dplyr::select(dplyr::all_of(c("TIME", model@variable, "DV")))
    assertthat::are_equal(length(times), nrow(observations))
  } else {
    observations <- tibble::tibble(TIME=numeric(0), !!model@variable:=numeric(0), DV=numeric(0))
  }
  return(observations)
}
