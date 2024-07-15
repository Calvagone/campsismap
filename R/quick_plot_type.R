#_______________________________________________________________________________
#----                        quick_plot_type class                          ----
#_______________________________________________________________________________

#' 
#' Quick plot type.
#' 
#' @export
setClass(
  "quick_plot_type",
  representation(
  )
)

#_______________________________________________________________________________
#----                   individual_fit_plot_type class                      ----
#_______________________________________________________________________________

#' 
#' Individual fit plot type.
#' 
#' @export
setClass(
  "individual_fit_plot_type",
  representation(
    show_pop = "logical"
  ),
  contains="quick_plot_type"
)

#' Individual fit plot type.
#' 
#' @param show_pop show population (typical profile)
#' @return an individual fit plot type
#' @export
IndividualFitPlotType <- function(show_pop=TRUE) {
  return(new("individual_fit_plot_type", show_pop=show_pop))
}
