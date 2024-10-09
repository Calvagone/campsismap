#_______________________________________________________________________________
#----                       plot_display_options class                              ----
#_______________________________________________________________________________

#' Plot options class.
#' 
#' @slot ylim suggested Y-axis limit
#' @slot ylim_bar_plot suggested Y-axis limit in bar plot
#' @slot timeref POSIXct time corresponding to TIME=0
#' @slot date_labels label of datetime on X-axis
#' @slot date_breaks data breaks, e.g. '1 day'
#' @slot minor_breaks_interval minor breaks interval in hours
#' @slot date_limits date limits, vector of 2 POSIXct values with the limits
#' @slot date_n_dodge number of dodges for date
#' @slot show_legend show a legend
#' @slot legend_title legend title
#' @slot legend_position legend position
#' @slot x_axis_label label
#' @slot y_axis_label label
#' @slot x_axis_bar_plot_label label
#' @slot y_axis_bar_plot_label label
#' @slot bar_plot_value_mode bar plot value mode: 'within' or 'above'
#' @slot typical_profile_colour colour of typical profile
#' @slot individual_fit_profile_colour colour of individual fit profile
#' @slot recommendation_profile_colour colour of recommendation profile
#' @slot individual_fit_bar_colour colour of individual fit bar
#' @slot recommendation_bar_colour colour of recommendation bar
#' @slot target_profile_colour colour of target profile
#' @export
setClass(
  "plot_display_options",
  representation(
    ylim="numeric",
    ylim_bar_plot="numeric",
    timeref="POSIXct",
    date_labels="character",
    date_breaks="character", # E.g. '1 day'
    minor_breaks_interval="integer", # E.g. 6
    date_limits="POSIXct",
    date_n_dodge="integer", # E.g. 2L
    show_legend="logical",
    legend_title="character",
    legend_position="character",
    x_axis_label="character",
    y_axis_label="character",
    x_axis_bar_plot_label="character",
    y_axis_bar_plot_label="character",
    bar_plot_value_mode="character",
    typical_profile_colour="character",
    individual_fit_profile_colour="character",
    recommendation_profile_colour="character",
    individual_fit_bar_colour="character",
    recommendation_bar_colour="character",
    target_profile_colour="character"
  )
)

#' Plot display options.
#' 
#' @param ylim suggested Y-axis limit
#' @param ylim_bar_plot suggested Y-axis limit in bar plot
#' @param timeref POSIXct time corresponding to TIME=0 in simulation
#' @param date_labels label of datetime on X-axis
#' @param date_breaks data breaks, e.g. '1 day'
#' @param minor_breaks_interval minor breaks interval in hours
#' @param date_limits date limits, vector of 2 POSIXct values with the limits
#' @param date_n_dodge number of dodges for date
#' @param show_legend show a legend
#' @param legend_title legend title
#' @param legend_position legend position
#' @param x_axis_label label
#' @param y_axis_label label
#' @param x_axis_bar_plot_label label
#' @param y_axis_bar_plot_label label
#' @param bar_plot_value_mode bar plot value mode: 'within' or 'above'
#' @param typical_profile_colour colour of typical profile
#' @param individual_fit_profile_colour colour of individual fit profile
#' @param recommendation_profile_colour colour of recommendation profile
#' @param individual_fit_bar_colour colour of individual fit bar
#' @param recommendation_bar_colour colour of recommendation bar
#' @param target_profile_colour colour of target profile
#' @return an object
#' @export
PlotDisplayOptions <- function(ylim=NULL, ylim_bar_plot=NULL, timeref=NULL, date_labels="%b %d", date_breaks="1 day",
                               minor_breaks_interval=6, date_limits=.POSIXct(character(0)), date_n_dodge=1L,
                               show_legend=FALSE, legend_title="Legend", legend_position="right",
                               x_axis_label="Time", y_axis_label="Concentration", x_axis_bar_plot_label="Time", y_axis_bar_plot_label="Dose",
                               bar_plot_value_mode="within",
                               typical_profile_colour="#6196B4", individual_fit_profile_colour="#B90E1E", recommendation_profile_colour="#B2B2B2",
                               individual_fit_bar_colour="#B90E1E", recommendation_bar_colour="#B2B2B2", target_profile_colour="palegreen1") {
  if (is.null(ylim)) {
    ylim <- NA
  }
  if (is.null(ylim_bar_plot)) {
    ylim_bar_plot <- NA
  }
  if (is.null(timeref)) {
    timeref <- NA
  }
  return(new("plot_display_options", ylim=as.numeric(ylim), ylim_bar_plot=as.numeric(ylim_bar_plot),
             timeref=as.POSIXct(timeref), date_labels=date_labels, date_breaks=date_breaks,
             minor_breaks_interval=as.integer(minor_breaks_interval), date_limits=date_limits, date_n_dodge=as.integer(date_n_dodge),
             show_legend=show_legend, legend_title=legend_title, legend_position=legend_position,
             x_axis_label=x_axis_label, y_axis_label=y_axis_label, x_axis_bar_plot_label=x_axis_bar_plot_label, y_axis_bar_plot_label=y_axis_bar_plot_label,
             bar_plot_value_mode=bar_plot_value_mode, 
             typical_profile_colour=typical_profile_colour, individual_fit_profile_colour=individual_fit_profile_colour, recommendation_profile_colour=recommendation_profile_colour,
             individual_fit_bar_colour=individual_fit_bar_colour, recommendation_bar_colour=recommendation_bar_colour, target_profile_colour=target_profile_colour))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

#' Apply plot options to plot.
#' 
#' @param object plot
#' @param x options
#' @param variable variable of interest
#' @return updated plot
#' @importFrom ggplot2 guides scale_x_datetime ylim
setMethod("add", signature = c("ANY", "plot_display_options"), definition = function(object, x, variable) {
  options <- x
  plot <- object
  bar_plot <- isBarPlot(plot)
  
  # Use suggested Y limit
  if (bar_plot) {
    suggestedYLim <- options@ylim_bar_plot
  } else {
    suggestedYLim <- options@ylim
  }

  maxYValue <- maxValueInPlot(plot=plot, variable=variable)
  
  if (!is.na(suggestedYLim)) {
    if (is.finite(maxYValue) && maxYValue > suggestedYLim) {
      suggestedYLim <- maxYValue
    }
    # Special empirical handling when value is above
    if (bar_plot && options@bar_plot_value_mode=="above") {
      suggestedYLim <- suggestedYLim * (1 + 0.2)
    }
    plot <- plot + 
      ggplot2::ylim(limits=c(0, suggestedYLim))
  }
  
  timeref <- options@timeref
  
  # Datetime label
  if (is.na(timeref)) {
    # Limits can't be defined using date_limits (POSIXct vector)
  } else {
    plot <- plotToPOSIXct(plot=plot, timeref=timeref)
    
    fun <- function(limits) {
      return(minorBreaksCustom(limits=limits, breaksInterval=options@minor_breaks_interval))
    }
    
    if (length(options@date_limits) > 0) {
      plot <- plot +
        ggplot2::scale_x_datetime(date_breaks=options@date_breaks, minor_breaks=fun,
                                  date_labels=options@date_labels, limits=options@date_limits,
                                  guide=ggplot2::guide_axis(n.dodge=options@date_n_dodge))
    } else {
      min <- as.POSIXct(minValueInPlot(plot, "TIME"))
      max <- as.POSIXct(maxValueInPlot(plot, "TIME"))
      plot <- plot +
        ggplot2::scale_x_datetime(date_breaks=options@date_breaks, minor_breaks=fun,
                                  date_labels=options@date_labels, limits=c(min, max),
                                  guide=ggplot2::guide_axis(n.dodge=options@date_n_dodge))
    }
  }
  
  if (bar_plot) {
    x_axis_label <- options@x_axis_bar_plot_label
    y_axis_label <- options@y_axis_bar_plot_label
  } else {
    x_axis_label <- options@x_axis_label
    y_axis_label <- options@y_axis_label
  }
  if (x_axis_label=="") {
    x_axis_label <- NULL
  }
  if (y_axis_label=="") {
    y_axis_label <- NULL
  }
  
  plot <- plot +
    ggplot2::xlab(x_axis_label)
  
  plot <- plot +
    ggplot2::ylab(y_axis_label)

  if (!options@show_legend) {
    plot <- plot +
      ggplot2::guides(color="none")
  }
  
  plot <- plot +
    ggplot2::theme_bw()
  
  plot <- plot +
    ggplot2::theme(legend.position=options@legend_position)

  return(plot)
})


#' Custom minor breaks based on limits.
#' 
#' @param limits limits, vector of 2 POSIXct values
#' @param breaksInterval minor breaks interval, in hours
#' @importFrom lubridate dhours interval
#' @export
minorBreaksCustom <- function(limits, breaksInterval) {
  initTime <- toDateTime(date=as.character(as.Date(limits[1])), time="00:00")
  maxTime <- limits[2]
  intervalDuration <- (lubridate::interval(initTime, maxTime) %>% as.numeric())/3600
  noOfBreaks <- intervalDuration/breaksInterval
  minor_breaks <- seq_len(noOfBreaks) %>%
    purrr::map_dbl(~initTime + lubridate::dhours(.x*breaksInterval)) %>%
    as.POSIXct()
  return(minor_breaks)
}

#' Automatically convert all TIME columns found in plot to POSIXct given the time reference.
#' 
#' @param plot ggplot2 plot
#' @param timeref time reference
#' @return updated plot
#' @export
plotToPOSIXct <- function(plot, timeref) {
  plot$data <- timeToPOSIXct(plot$data, timeref=timeref)
  plot$layers <- plot$layers %>% purrr::map(.f=function(layer) {
    constructor <- layer$constructor[1] %>% as.character()
    layer$data <- timeToPOSIXct(layer$data, timeref=timeref, constructor=constructor)
    return(layer)
  })
  
  return(plot)
}

#' Automatically get the max value from the plot.
#' 
#' @param plot ggplot2 plot
#' @param variable variable of interest
#' @return max value in all data layers
#' @export
maxValueInPlot <- function(plot, variable) {
  return(minMaxInPlot(plot=plot, variable=variable, fun=max))
}

#' Automatically get the min value from the plot.
#' 
#' @param plot ggplot2 plot
#' @param variable variable of interest
#' @return min value in all data layers
#' @export
minValueInPlot <- function(plot, variable) {
  return(minMaxInPlot(plot=plot, variable=variable, fun=min))
}

minMaxInPlot <- function(plot, variable, fun) {
  if (variable %in% colnames(plot$data)) {
    value1 <- suppressWarnings(fun(plot$data %>% dplyr::pull(variable)))
  } else {
    value1 <- NULL
  }
  value2 <- plot$layers %>% purrr::map(.f=function(layer) {
    if (variable %in% colnames(layer$data)) {
      return(suppressWarnings(fun(layer$data %>% dplyr::pull(variable))))
    } else {
      return(NULL)
    }
    return(layer)
  }) %>% purrr::discard(~is.null(.x)) %>%
    purrr::flatten_dbl()
  retValue <- suppressWarnings(fun(c(value1, value2)))
  return(retValue)
}

timeToPOSIXct <- function(x, timeref, constructor=NULL) {
  if ("TIME" %in% colnames(x)) {
    x <- x %>%
      dplyr::mutate(TIME=timeref + lubridate::dhours(TIME))
    if (!is.null(constructor) && grepl(pattern="geom_col", x=constructor)) {
      if (length(unique(x$TIME)) == 1) {
        # print(sprintf("Duplicating rows in layer %s", constructor))
        x <- dplyr::bind_rows(x, x %>% dplyr::mutate(value=0, TIME=TIME + lubridate::dhours(24)))
      }
    }
  }
  return(x)
}

isBarPlot <- function(plot) {
  values <- plot$layers %>% purrr::map_lgl(.f=function(layer) {
    constructor <- layer$constructor[1] %>% as.character()
    if (!is.null(constructor) && grepl(pattern="geom_col", x=constructor)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  return(any(values))
}
