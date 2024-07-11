#_______________________________________________________________________________
#----                            recommend                                  ----
#_______________________________________________________________________________

#' @rdname recommend
#' @importFrom optimx optimr
setMethod("recommend", signature("campsismap_model", "dataset", "numeric", "target_definition", "POSIXct", "simulation_settings"),
          function(object, dataset, etas, target, now, settings, ...) {
  
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
  
  rules <- Rules(TroughTimeRule(ii=12, use_next_dose=TRUE))
  
  targetEffective <- target %>%
    export(dest=TargetDefinitionEffective(), dosing="TODO", rules="TODO")
  
  optimisationFun <- function(par, model, dataset, target, now) {
    # TODO
    # Predict samples
    # Compare to target
    return(0)
  }
  
  retValue <- optimx::optimr(par=etas, fn=optimisationFun, hessian=FALSE, method="L-BFGS-B", model=model, dataset=datasetTbl, target=targetEffective, now=now)
  return(TRUE)
})