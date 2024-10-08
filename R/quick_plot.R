
#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
#' @param target effective target to draw, if provided. NULL by default.
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "individual_fit_plot_type", "plot_display_options"),
          function(model, dataset, etas, plot, options, target=NULL) {

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
    ggplot2::scale_color_manual(name=options@legend_title, values=c("Individual fit"=options@individual_fit_profile_colour,
                                                                    "Typical profile"=options@typical_profile_colour))
  
  # Retrieve DV
  dv <- dataset %>%
    getSamples()
  
  # Add the observations
  if (nrow(dv) > 0) {
    retValue <- retValue +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
  }
  
  # Draw target profile if provided
  if (!is.null(target)) {
    retValue <- drawTarget(x=retValue, target=target, colour=options@target_profile_colour)
  }
  
  # Add display options
  retValue <- retValue %>% add(options, variable=model@variable)

  return(retValue)
})

#' @param recommendation Campsismap recommendation
#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "recommendation_plot_type", "plot_display_options"),
          function(model, dataset, etas, plot, options, recommendation) {
  
  # Check model is ready
  if (!checkModelReady(model, check_error_model=FALSE, raise_error=FALSE)) {
    model <- model %>%
      setup(dest="mrgsolve")
  }
  
  # If etas not provided, they are all 0
  if (length(etas) == 0) {
    etas <- recommendation@etas
  } else {
    warning("ETAs specified in quickPlot call, ETAs contained in recommendation are discarded")
  }
  
  # Add sampling if not there (1000 points by default)
  dataset <- dataset %>%
    addSampling()
  
  # Simulate
  resultsA <- predict(object=model, dataset=dataset, etas=etas) %>%
    dplyr::mutate(COLOR="Individual fit")
  
  resultsB <- predict(object=model, dataset=recommendation@recommended_dataset, etas=etas) %>%
    dplyr::mutate(COLOR="Recommendation")

  results <- dplyr::bind_rows(resultsA, resultsB)
  
  retValue <- ggplot2::ggplot(data=results, mapping=ggplot2::aes(x=TIME, y=.data[[model@variable]], colour=COLOR)) +
    ggplot2::geom_line(linewidth=1, alpha=0.6)

  # Scale color manual to add a defaut legend
  retValue <- retValue +
    ggplot2::scale_color_manual(name=options@legend_title, values=c("Individual fit"=options@individual_fit_profile_colour,
                                                                    "Recommendation"=options@recommendation_profile_colour))
  
  # Retrieve DV
  dv <- recommendation@samples
  
  # Add the observations
  if (nrow(dv) > 0) {
    retValue <- retValue +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="black")
  }
  
  # Now vertical line
  now <- recommendation@now
  retValue <- retValue +
    ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=TIME), data=tibble::tibble(TIME=now), color="black", linetype="dotted")
  
  # Draw target profile
  retValue <- drawTarget(x=retValue, target=recommendation@effective_target, colour=options@target_profile_colour)
  
  # Add display options
  retValue <- retValue %>% add(options, variable=model@variable)
  
  return(retValue)
})

#'
#' Draw target profile.
#' 
#' @param x ggplot object
#' @param target target effective definition, an object of class 'target_definition_effective'
#' @param colour colour of the target profile
#' @param linetype linetype of the target profile, default is 'dashed'
#' @return updated ggplot object
#' @importFrom ggplot2 geom_step
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
#' 
drawTarget <- function(x, target, colour, linetype="dashed") {
  if (!is(target, "target_definition_effective")) {
    stop("target must be an object of class 'target_definition_effective'")
  }

  # Fill in target profile on the right-hand side
  table <- target@table
  lastValue <- table$VALUE[length(table$VALUE)]
  maxTime <- maxValueInPlot(plot=x, variable="TIME")
  table_ <- dplyr::bind_rows(table, tibble::tibble(TIME=maxTime, VALUE=lastValue))
  
  # Fill in target profile on the left-hand side
  firstValue <- table$VALUE[1]
  firstTime <- table$TIME[1]
  if (firstTime > 0) {
    table_ <- dplyr::bind_rows(tibble::tibble(TIME=0, VALUE=firstValue), table_)
  }
  
  x <- x +
    ggplot2::geom_step(data=table_, mapping=ggplot2::aes(x=TIME, y=VALUE), direction="vh", colour=colour, linetype=linetype)
  
  return(x)
}

#' @param recommendation Campsismap recommendation
#' @importFrom ggplot2 geom_col geom_text geom_vline guides scale_fill_manual theme_bw
#' @importFrom tidyr pivot_longer
#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "recommendation_bar_plot_type", "plot_display_options"),
          function(model, dataset, etas, plot, options, recommendation) {
            
  # Retrieve summary          
  summary <- recommendation %>% export(dest="summary") %>%
    dplyr::rename(Original=ORIGINAL, Recommendation=RECOMMENDATION)
  
  # Deriving position dodge width automatically
  diffTime <- diff(summary$TIME)
  hasTimeref <- !is.na(options@timeref)
  
  if (length(diffTime)==0) {
    diffTime <- ifelse(hasTimeref, 24, 1)
  }
  position_dodge_width <- ifelse(hasTimeref, 3600*min(diffTime)*0.9, min(diffTime)*0.9)

  # Pivot longer
  summary <- summary %>%
    tidyr::pivot_longer(cols=c("Original", "Recommendation"), names_to="AMT")
  
  # Data passed to geom_col due to bug with POSIXct if 1 bar (and not in ggplot constructor)
  # See method plotToPOSIXct and timeToPOSIXct
  retValue <- ggplot2::ggplot() +
      ggplot2::geom_col(mapping=ggplot2::aes(x=TIME, y=value, fill=AMT), data=summary, position="dodge")
  
  if (options@bar_plot_value_mode=="within") {
    retValue <- retValue +
      ggplot2::geom_text(ggplot2::aes(x=TIME, y=value, label=value, group=AMT), data=summary, vjust=1.6, color="white",
                         position = ggplot2::position_dodge(width=position_dodge_width), size=3.5, fontface="bold")
    
  } else if (options@bar_plot_value_mode=="above") {
    retValue <- retValue +
      ggplot2::geom_text(ggplot2::aes(x=TIME, y=value, label=value, group=AMT), data=summary, vjust=-0.5, color="black",
                         position = ggplot2::position_dodge(width=position_dodge_width), size=3.5, fontface="bold")
  }
  
  retValue <- retValue + 
    ggplot2::scale_fill_manual(values=c("Original"=options@individual_fit_bar_colour, "Recommendation"=options@recommendation_bar_colour))
    
  if (nrow(summary) == 0) {
    # Otherwise, x-axis is shifted by 1 day (after conversion to POSIXct), seems a bug in ggplot2
    # If this point is there, it seems to work well
    retValue <- retValue +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=VALUE), data=tibble::tibble(TIME=c(recommendation@now), VALUE=0), color="black", alpha=0)
  }
  retValue <- retValue +
    ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=TIME), data=tibble::tibble(TIME=c(recommendation@now)), color="black", linetype="dotted")
  
  # Add display options
  retValue <- retValue %>%
    add(options, variable="value")
  
  retValue <- retValue +
    ggplot2::guides(fill="none")
  
  return(retValue)
})


#' @param recommendation Campsismap recommendation
#' @importFrom patchwork plot_layout
#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric", "recommendation_full_plot_type", "plot_display_options"),
          function(model, dataset, etas, plot, options, recommendation) {
            
  plotA <- quickPlot(model=model, recommendation=recommendation, plot=RecommendationPlotType(), options=options)
  options_ <- options
  limitsPlotA <- c(minValueInPlot(plotA, "TIME"), maxValueInPlot(plotA, "TIME"))
  
  if (!is.na(options_@timeref)) {
    options_@date_limits <- limitsPlotA
  }

  plotB <- quickPlot(model=model, recommendation=recommendation, plot=RecommendationBarPlotType(), options=options_)
  if (is.na(options_@timeref)) {
    # Not working well
    # plotB <- plotB +
    #   ggplot2::scale_x_continuous(limits=limitsPlotA)
  }
  
  retValue <- plotA + plotB + patchwork::plot_layout(ncol = 1, heights = c(0.75, 0.25))

  return(retValue)
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
