#_______________________________________________________________________________
#----                            recommend                                  ----
#_______________________________________________________________________________

#' @rdname recommend
#' @importFrom optimx optimr
setMethod("recommend", signature("campsismap_model", "dataset", "numeric", "target_definition", "POSIXct", "simulation_settings"),
          function(object, dataset, etas, target, now, settings, ...) {
  
  print("Calculating recommendations")
  
  return(TRUE)
})