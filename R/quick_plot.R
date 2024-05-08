
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "logical", "plot_options"), function(model, dataset, etas, pop, options) {

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

  # Time reference
  timeref <- options@timeref
  
  if (!is.na(timeref)) {
    results <- results %>%
      dplyr::mutate(TIME=timeref + lubridate::dhours(TIME))
    if (pop) {
      resultsPop <- resultsPop %>%
        dplyr::mutate(TIME=timeref + lubridate::dhours(TIME))
    }
    if (nrow(dv) > 0) {
      dv <- dv %>%
        dplyr::mutate(TIME=timeref + lubridate::dhours(TIME))
    }
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
  suggestedYLim <- options@ylim
  
  if (!is.na(suggestedYLim)) {
    if (maxYValue > suggestedYLim) {
      suggestedYLim <- maxYValue
    }
    plot <- plot + 
      ggplot2::ylim(limits=c(0, suggestedYLim))
  }
  
  # Datetime label
  if (!is.na(timeref)) {
    timelabel <- options@timelabel
    breaksInterval <- options@minor_breaks_interval

    # Set locale to English
    invisible(Sys.setlocale("LC_ALL", "English.utf8"))
    
    initTime <- toDateTime(date=as.character(as.Date(timeref)), time="00:00")
    maxTime <- timeref + lubridate::dhours(max(dataset %>% getSimulationTimes()))
    intervalDuration <- (lubridate::interval(initTime, maxTime) %>% as.numeric())/3600
    noOfBreaks <- intervalDuration/breaksInterval
    minor_breaks <- seq_len(noOfBreaks) %>%
      purrr::map_dbl(~initTime + lubridate::dhours(.x*breaksInterval)) %>%
      as.POSIXct()

    plot <- plot +
      ggplot2::scale_x_datetime(date_breaks="1 day", minor_breaks=minor_breaks, date_labels=timelabel)
  }

  return(plot + ggplot2::theme_bw())
})


