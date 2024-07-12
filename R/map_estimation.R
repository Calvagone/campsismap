
#_______________________________________________________________________________
#----                            estimate                                   ----
#_______________________________________________________________________________

#' @rdname estimate
#' @importFrom optimx optimr
setMethod("estimate", signature("campsismap_model", "dataset", "numeric"), function(model, dataset, etas, ...) {
  
  # Check inputs
  checkModelReady(model)
  etas <- initialiseEtaVector(etas, model=model)

  datasetTbl <- dataset %>%
    export(dest=model@dest, seed=1, model=NULL, settings=model@settings)
  
  samples <- dataset %>%
    getSamples()
  
  likelihoodFun <- function(par, model, dataset, samples) {
    popLL <- populationLikelihood(model=model, etas=par)
    indLL <- individualLikelihood(model=model, dataset=dataset, samples=samples, etas=par)
    return(-2*(popLL + indLL))
  }
  
  retValue <- optimx::optimr(par=etas, fn=likelihoodFun, hessian=FALSE, method="L-BFGS-B", model=model, dataset=datasetTbl, samples=samples)
  return(retValue)
})
