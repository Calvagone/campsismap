#_______________________________________________________________________________
#----                            recommend                                  ----
#_______________________________________________________________________________

#' @rdname recommend
#' @importFrom optimx optimr
setMethod("recommend", signature("campsismap_model", "dataset", "numeric", "target_definition", "numeric", "simulation_settings"),
          function(object, dataset, etas, target, now, settings, ...) {
  
  model <- object

  # Check error model
  if (is(model@error, class(UndefinedErrorModel()))) {
    stop("No error model configured. Please add one.")
  }
  
  # Check model is ready
  checkModelReady(model)
  
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  
  datasetTbl <- dataset %>%
    export(dest=model@dest, seed=1, model=NULL, settings=model@settings)
  
  dosing <- datasetTbl %>% 
    dplyr::filter(EVID==1) %>%
    dplyr::select(TIME, AMT)
  
  rules <- Rules(TroughTimeRule(ii=12, use_next_dose=TRUE))
  
  targetPerDose <- target %>%
    export(dest=TargetDefinitionPerDose(), dosing=dosing)
  
  targetEffective <- targetPerDose %>%
    export(dest=TargetDefinitionEffective(), dosing=dosing, rules=rules)
  
  targetTbl <- targetEffective@table %>%
    dplyr::mutate(ADAPTABLE_DOSE=LAST_DOSE_TIME > now) %>%
    dplyr::filter(ADAPTABLE_DOSE)
  
  for (index in seq_len(nrow(targetTbl))) {
    currentTarget <- targetTbl[index, ]
    doseno <- currentTarget$LAST_DOSENO
    targetValue <- tibble::tibble(TIME=currentTarget$TIME, DV=currentTarget$VALUE)
    
    dataset_ <- dataset %>%
      addSamples(targetValue)
    datasetTbl_ <- dataset_ %>%
      export(dest=model@dest, seed=1, model=NULL, settings=model@settings)
    
    doseIndex <- which(datasetTbl_$EVID == 1 & datasetTbl_$DOSENO==doseno)
    initDose <- datasetTbl_$AMT[doseIndex]
    
    res <- optimx::optimr(par=initDose, fn=recommendOptimisationFun, hessian=FALSE, method="L-BFGS-B",
                          model=model, etas=etas, dataset=datasetTbl_, targetValue=targetValue, doseIndex=doseIndex)
    recommendation <- res$par
    if (recommendation < 0) {
      recommendation <- 0
    }
    dataset <- updateDoseAmount(object=dataset, amount=recommendation, dose_number=doseno)
  }

  return(dataset)
})

recommendOptimisationFun <- function(par, model, etas, dataset, targetValue=targetValue, doseIndex=doseIndex) {
  dataset[doseIndex, "AMT"] <- par
  results <- predict(object=model@model_cache, dataset=dataset, etas=etas, settings=model@settings)
  
  ipred <- results %>% dplyr::pull(model@variable)
  ipredTimes <- results$TIME
  dv <- targetValue$DV
  dvTimes <- targetValue$TIME
  
  # Make sure times match...
  assertthat::assert_that(length(dv)==nrow(results), msg="dv and results do not have the same number of observations")
  assertthat::assert_that(all(abs(ipredTimes-dvTimes) < 1e-6), msg="times in dv and results do not match")
  
  return((ipred-dv)^2)
}

