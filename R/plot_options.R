#_______________________________________________________________________________
#----                       plot_options class                              ----
#_______________________________________________________________________________

#' Plot options class.
#' 
#' @slot ylim suggested Y-axis limit
#' @slot timeref POSIXct time corresponding to TIME=0
#' @slot timelabel label of datetime on X-axis
#' @slot minor_breaks_interval minor breaks interval in hours
#' @export
setClass(
  "plot_options",
  representation(
    ylim="numeric",
    timeref="POSIXct",
    timelabel="character",
    minor_breaks_interval="integer"
  )
)

#' Plot options.
#' 
#' @param ylim suggested Y-axis limit
#' @param timeref POSIXct time corresponding to TIME=0 in simulation
#' @param timelabel label of datetime on X-axis
#' @param minor_breaks_interval minor breaks interval in hours
#' @return an object
#' @export
PlotOptions <- function(ylim=NULL, timeref=NULL, timelabel="%b %d", minor_breaks_interval=6) {
  if (is.null(ylim)) {
    ylim <- NA
  }
  if (is.null(timeref)) {
    timeref <- NA
  }
  return(new("plot_options", ylim=as.numeric(ylim), timeref=as.POSIXct(timeref), timelabel=timelabel,
             minor_breaks_interval=as.integer(minor_breaks_interval)))
}
