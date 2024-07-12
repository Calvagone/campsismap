
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "logical", "plot_options"), function(model, dataset, etas, pop, options) {

  # Check model is ready
  if (!checkModelReady(model, check_error_model=FALSE, raise_error=FALSE)) {
    model <- model %>%
      setup(dest="mrgsolve")
  }
  
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  
  # Add simulation times to the dataset
  times <- getSimulationTimes(dataset)
  dv <- dataset %>%
    getSamples()
  if (length(times)==0) {
    if (nrow(dv)==0) {
      maxTime <- 0
    } else {
      maxTime <- max(dv$TIME)
    }
    maxTime <- maxTime + 24
    simulatedTimes <- seq(0, maxTime, length.out=1000)
    dataset <- dataset %>%
      add(Observations(simulatedTimes))
  } else {
    # Don't do anything
    # Simulation times are provided
  }

  # Simulate
  results <- predict(object=model, dataset=dataset, etas=etas)
  if (pop) {
    resultsPop <- predict(object=model, dataset=dataset, etas=rep(0, length(model@eta_names)))
  }

  plot <- ggplot2::ggplot(data=results, mapping=ggplot2::aes(x=TIME, y=.data[[model@variable]])) +
    ggplot2::geom_line(mapping=ggplot2::aes(color="Individual fit"), linewidth=1, alpha=0.6)

  if (pop) {
    plot <- plot +
      ggplot2::geom_line(mapping=ggplot2::aes(color="Typical profile"), data=resultsPop, linewidth=1, alpha=0.6)
  }
  
  # Scale color manual to add a defaut legend
  plot <- plot +
    ggplot2::scale_color_manual(name=options@legend_title, values=c("Individual fit"="#B90E1E", "Typical profile"="#6196B4"))
  
  # Add the observations
  if (nrow(dv) > 0) {
    plot <- plot +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
  }

  # Add options
  plot <- plot %>% add(options, variable=model@variable)

  return(plot + ggplot2::theme_bw())
})
