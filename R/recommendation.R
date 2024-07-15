
#_______________________________________________________________________________
#----                           recommendation                              ----
#_______________________________________________________________________________

#' Recommendation.
#' 
#' @export
setClass(
  "recommendation",
  representation(
    original_dataset = "dataset",
    recommended_dataset = "dataset",
    original_target = "target_definition",
    effective_target = "target_definition_effective",
    rules = "dose_adaptation_rules",
    now = "numeric"
  )
)

#' Make a recommendation object.
#'
#' @param dataset original dataset
#' @param target target definition
#' @param rules dose adaptation rules
#' @param now numeric value
#' @return recommendation object
#' @export 
Recommendation <- function(dataset, target, rules, now=0) {
  
  datasetTbl <- dataset %>%
    export(dest="RxODE", seed=1, model=NULL)
  
  dosing <- datasetTbl %>% 
    dplyr::filter(EVID==1) %>%
    dplyr::select(TIME, AMT)
  
  targetEffective <- target %>%
    export(dest=TargetDefinitionEffective(), dosing=dosing, rules=rules)
  
  return(new("recommendation", original_dataset=dataset, original_target=target, effective_target=targetEffective, rules=rules, now=now))
}

#_______________________________________________________________________________
#----                               add                                     ----
#_______________________________________________________________________________

#' Add observations (for simulation) to both the original and recommended datasets.
#' @param object recommendation
#' @param x observations
#' @return updated Campsismap model
setMethod("add", signature = c("recommendation", "observations"), definition = function(object, x) {
  object@original_dataset <- object@original_dataset %>%
    add(x)
  object@recommended_dataset <- object@recommended_dataset %>%
    add(x)
  return(object)
})

#_______________________________________________________________________________
#----                     retrieveDoseAmount                                ----
#_______________________________________________________________________________

#' @rdname retrieveDoseAmount
setMethod("retrieveDoseAmount", signature("recommendation", "integer"), function(object, dose_number) {
  return(object@recommended_dataset %>% retrieveDoseAmount(dose_number=dose_number))
})
