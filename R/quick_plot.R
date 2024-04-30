
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "logical"), function(model, dataset, etas, pop, suggestedYLim=NULL) {
  
  # Check model is ready
  if (!checkModelReady(model, raise_error=FALSE)) {
    model <- model %>%
      setup(dest="mrgsolve")
  }
  
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  
  # Add simulation times to the dataset
  times <- getObservationTimes(dataset)
  if (length(times)==0) times=0
  maxTime <- max(times) + 24
  simulatedTimes <- seq(0, maxTime, length.out=1000)
  dataset <- dataset %>%
    add(Observations(simulatedTimes))

  # Retrieve DV
  dv <- dataset %>%
    getSamples()
  
  # Simulate
  results <- predict(object=model, dataset=dataset, etas=etas)
  if (pop) {
    resultsPop <- predict(object=model, dataset=dataset, etas=rep(0, length(model@eta_names)))
  }

  plot <- ggplot2::ggplot(data=results, mapping=ggplot2::aes(x=TIME, y=.data[[model@variable]])) +
    ggplot2::geom_line(linewidth=1, alpha=0.6, color="#B90E1E")
  
  # Take max
  maxYValue <- max(results[[model@variable]])
  
  if (pop) {
    plot <- plot +
      ggplot2::geom_line(data=resultsPop, linewidth=1, alpha=0.6, color="#6196B4")
    
    # Update max value
    maxYValuePop <- max(resultsPop[[model@variable]])
    if (maxYValue < maxYValuePop) {
      maxYValue <- maxYValuePop
    }
  }
  
  if (nrow(dv) > 0) {
    plot <- plot +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
    
    # Update max value
    maxYValueDV <- max(dv$DV)
    if (maxYValue < maxYValueDV) {
      maxYValue <- maxYValueDV
    }
  }

  # Use suggested Y limit  
  if (!is.null(suggestedYLim)) {
    if (maxYValue > suggestedYLim) {
      suggestedYLim <- maxYValue
    }
    plot <- plot + 
      ggplot2::ylim(limits=c(0, suggestedYLim))
  }
  
  return(plot + ggplot2::theme_bw())
})
