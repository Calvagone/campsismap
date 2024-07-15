
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "individual_fit_plot_type", "plot_display_options"), function(model, dataset, etas, plot, options) {

  # Check model is ready
  if (!checkModelReady(model, check_error_model=FALSE, raise_error=FALSE)) {
    model <- model %>%
      setup(dest="mrgsolve")
  }
  
  # If etas not provided, they are all 0
  if (length(etas)==0) {
    etas <- rep(0, length(model@eta_names))
  }
  
  # Add sampling if not there (1000 points by default)
  dataset <- dataset %>%
    addSampling()

  # Simulate
  results <- predict(object=model, dataset=dataset, etas=etas)
  if (plot@show_pop) {
    resultsPop <- predict(object=model, dataset=dataset, etas=rep(0, length(model@eta_names)))
  }

  retValue <- ggplot2::ggplot(data=results, mapping=ggplot2::aes(x=TIME, y=.data[[model@variable]])) +
    ggplot2::geom_line(mapping=ggplot2::aes(color="Individual fit"), linewidth=1, alpha=0.6)

  if (plot@show_pop) {
    retValue <- retValue +
      ggplot2::geom_line(mapping=ggplot2::aes(color="Typical profile"), data=resultsPop, linewidth=1, alpha=0.6)
  }
  
  # Scale color manual to add a defaut legend
  retValue <- retValue +
    ggplot2::scale_color_manual(name=options@legend_title, values=c("Individual fit"="#B90E1E", "Typical profile"="#6196B4"))
  
  # Retrieve DV
  dv <- dataset %>%
    getSamples()
  
  # Add the observations
  if (nrow(dv) > 0) {
    retValue <- retValue +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
  }

  # Add options
  retValue <- retValue %>% add(options, variable=model@variable)

  return(retValue + ggplot2::theme_bw())
})

addSampling <- function(dataset, length.out=1000) {
  # Retrieve DV
  dv <- dataset %>%
    getSamples()
  
  # Add simulation times to the dataset
  times <- getSimulationTimes(dataset)
  
  if (length(times)==0) {
    if (nrow(dv)==0) {
      maxTime <- 0
    } else {
      maxTime <- max(dv$TIME)
    }
    maxTime <- maxTime + 24
    simulatedTimes <- seq(0, maxTime, length.out=length.out)
    dataset <- dataset %>%
      add(Observations(simulatedTimes))
  } else {
    # Don't do anything
    # Simulation times are provided
  }
  return(dataset)
}
