#_______________________________________________________________________________
#----                            recommend                                  ----
#_______________________________________________________________________________

#' @rdname recommend
#' @importFrom optimx optimr
setMethod("recommend", signature("campsismap_model", "dataset", "numeric", "target_definition", "numeric", "dose_adaptation_rules", "simulation_settings"),
          function(object, dataset, etas, target, now, rules, settings, ...) {
  
  model <- object
  
  # Check inputs
  checkModelReady(model, check_error_model=FALSE)
  etas <- initialiseEtaVector(etas, model=model)
  
  # Prepare recommendation object
  retValue <- Recommendation(dataset=dataset, target=target, rules=rules, now=now)
  
  targetTbl <- retValue@effective_target@table %>%
    dplyr::mutate(ADAPTABLE_DOSE=LAST_DOSE_TIME > now) %>%
    dplyr::filter(ADAPTABLE_DOSE)
  
  doseRoundingRule <- rules@list %>% purrr::detect(~(.x %>% getName())==(DoseRoundingRule() %>% getName()))
  if (is.null(doseRoundingRule)) {
    doseRoundingRule <- DoseRoundingRule() # Default
    warning("No rule detected for rounding the doses. Default rule will apply.")
  }
  
  # Dataset copy and clear initial samples
  recommendedDataset <- dataset
  recommendedDataset@arms@list[[1]]@protocol@observations@list <- list()
  
  for (index in seq_len(nrow(targetTbl))) {
    currentTarget <- targetTbl[index, ]
    doseno <- currentTarget$LAST_DOSENO
    targetValue <- tibble::tibble(TIME=currentTarget$TIME, DV=currentTarget$VALUE)
    
    dataset_ <- recommendedDataset %>%
      addSamples(targetValue)
    datasetTbl_ <- dataset_ %>%
      export(dest=model@dest, seed=1, model=NULL, settings=model@settings)
    
    doseIndex <- which(datasetTbl_$EVID==1 & datasetTbl_$DOSENO==doseno)
    initDose <- datasetTbl_$AMT[doseIndex]
    
    res <- optimx::optimr(par=initDose, fn=recommendOptimisationFun, hessian=FALSE, method="L-BFGS-B",
                          model=model, etas=etas, dataset=datasetTbl_, targetValue=targetValue, doseIndex=doseIndex)
    recommendedDose <- res$par
    if (recommendedDose < 0) {
      recommendedDose <- 0
    }
    # Round recommended dose
    recommendedDose <- doseRoundingRule@fun(recommendedDose)
    
    recommendedDataset <- updateDoseAmount(object=recommendedDataset, amount=recommendedDose, dose_number=doseno)
  }
  
  # Store recommended dataset
  retValue@recommended_dataset <- recommendedDataset
  
  # Store ETAs
  retValue@etas <- etas

  return(retValue)
})

recommendOptimisationFun <- function(par, model, etas, dataset, targetValue=targetValue, doseIndex=doseIndex) {
  dataset[doseIndex, "AMT"] <- par
  results <- predict(object=model@model_cache, dataset=dataset, etas=etas, settings=model@settings)
  summary <- summarisePredictions(results=results, samples=targetValue, model=model)
  return((summary$IPRED-summary$DV)^2)
}

