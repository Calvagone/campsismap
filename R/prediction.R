
#' Individual predictions calculation.
#' 
#' @param model Campsismap model
#' @param dataset exported Campsis dataset
#' @param etas unnamed numeric vector
#' @return a dataframe with 2 columns: TIME, <VARIABLE>
#' @importFrom dplyr all_of transmute
#' @importFrom tibble tibble
#' @export
individualPrediction <- function(model, dataset, etas) {
  results <- simulateModel(object=model@model_cache, dataset=dataset, etas=etas, settings=model@settings)
  observations <- results %>%
    dplyr::select(dplyr::all_of(c("TIME", model@variable)))
  return(observations)
}
