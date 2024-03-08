
#_______________________________________________________________________________
#----                            estimate                                   ----
#_______________________________________________________________________________

#' @rdname estimate
#' @importFrom optimx optimr
setMethod("estimate", signature("campsismap_model", "dataset", "numeric"), function(model, dataset, etas, ...) {
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  
  datasetTbl <- dataset %>%
    export(dest=dest)
  
  likelihoodFun <- function(par, model, dataset) {
    popLL <- populationLikelihood(model=model, etas=par)
    indLL <- individualLikelihood(model=model, dataset=dataset, etas=par)
    return(-2*(popLL + indLL))
  }
  
  retValue <- optimx::optimr(par=etas, fn=likelihoodFun, hessian=FALSE, method="L-BFGS-B", model=model, dataset=datasetTbl)
  return(retValue)
})

