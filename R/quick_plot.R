
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "logical"), function(model, dataset, etas, pop) {
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
    getDV()
  
  # Simulate
  results <- simulateModel(object=model, dataset=dataset, etas=etas)
  if (pop) {
    resultsPop <- simulateModel(object=model, dataset=dataset, etas=rep(0, length(model@eta_names)))
  }

  plot <- ggplot2::ggplot(data=results, mapping=ggplot2::aes(x=TIME, y=.data[[model@variable]])) +
    ggplot2::geom_line(linewidth=1, alpha=0.6, color="#B90E1E")
  
  if (pop) {
    plot <- plot +
      ggplot2::geom_line(data=resultsPop, linewidth=1, alpha=0.6, color="#6196B4")
  }
  
  if (nrow(dv) > 0) {
    plot <- plot +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
  }
  
  return(plot + ggplot2::theme_bw())
})
