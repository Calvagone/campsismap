#_______________________________________________________________________________
#----                       plot_options class                              ----
#_______________________________________________________________________________

#' Plot options class.
#' 
#' @slot ylim suggested Y-axis limit
#' @slot timeref POSIXct time corresponding to TIME=0
#' @slot date_labels label of datetime on X-axis
#' @slot date_breaks data breaks, e.g. '1 day'
#' @slot minor_breaks_interval minor breaks interval in hours
#' @slot show_legend show a legend
#' @slot legend_title legend title
#' @export
setClass(
  "plot_options",
  representation(
    ylim="numeric",
    timeref="POSIXct",
    date_labels="character",
    date_breaks="character", # E.g. '1 day'
    minor_breaks_interval="integer", # E.g. 6
    show_legend="logical",
    legend_title="character"
  )
)

#' Plot options.
#' 
#' @param ylim suggested Y-axis limit
#' @param timeref POSIXct time corresponding to TIME=0 in simulation
#' @param date_labels label of datetime on X-axis
#' @param date_breaks data breaks, e.g. '1 day'
#' @param minor_breaks_interval minor breaks interval in hours
#' @param show_legend show a legend
#' @param legend_title legend title
#' @return an object
#' @export
PlotOptions <- function(ylim=NULL, timeref=NULL, date_labels="%b %d", date_breaks="1 day", minor_breaks_interval=6, show_legend=FALSE, legend_title="Legend") {
  if (is.null(ylim)) {
    ylim <- NA
  }
  if (is.null(timeref)) {
    timeref <- NA
  }
  return(new("plot_options", ylim=as.numeric(ylim), timeref=as.POSIXct(timeref), date_labels=date_labels,
             date_breaks=date_breaks, minor_breaks_interval=as.integer(minor_breaks_interval),
             show_legend=show_legend, legend_title=legend_title))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("ANY", "plot_options"), definition = function(object, x, variable) {
  options <- x
  plot <- object
  
  # Use suggested Y limit
  suggestedYLim <- options@ylim
  
  maxYValue <- maxValueInPlot(plot=plot, variable=variable)
  
  if (!is.na(suggestedYLim)) {
    if (maxYValue > suggestedYLim) {
      suggestedYLim <- maxYValue
    }
    plot <- plot + 
      ggplot2::ylim(limits=c(0, suggestedYLim))
  }
  
  timeref <- options@timeref
  
  # Datetime label
  if (!is.na(timeref)) {
    plot <- plotToPOSIXct(plot=plot, timeref=timeref)
    
    fun <- function(limits) {
      return(minorBreaksCustom(limits=limits, breaksInterval=options@minor_breaks_interval))
    }
    plot <- plot +
      ggplot2::scale_x_datetime(date_breaks=options@date_breaks, minor_breaks=fun, date_labels=options@date_labels)
  }
  
  if (!options@show_legend) {
    plot <- plot +
      ggplot2::guides(color="none")
  }

  return(plot)
})


#' Custom minor breaks based on limits.
#' 
#' @param limits limits, vector of 2 POSIXct values
#' @param ii minor breaks interval, in hours
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
    layer$data <- timeToPOSIXct(layer$data, timeref=timeref)
    return(layer)
  })
  
  return(plot)
}

#' Automatically get the max value from the plot.
#' 
#' @param plot ggplot2 plot
#' @return max value in all data layers
#' @export
maxValueInPlot <- function(plot, variable) {
  if (variable %in% colnames(plot$data)) {
    maxValue1 <- max(plot$data[, variable])
  } else {
    maxValue1 <- NULL
  }
  maxValue2 <- plot$layers %>% purrr::map(.f=function(layer) {
    if (variable %in% colnames(layer$data)) {
      return(max(layer$data[, variable]))
    } else {
      return(NULL)
    }
    return(layer)
  }) %>% purrr::discard(~is.null(.x)) %>%
    purrr::flatten_dbl() %>%
    max()
  maxValue <- max(c(maxValue1, maxValue2))
  return(maxValue)
}

timeToPOSIXct <- function(x, timeref) {
  if ("TIME" %in% colnames(x)) {
    x <- x %>%
      dplyr::mutate(TIME=timeref + lubridate::dhours(TIME))
  }
  return(x)
} 
