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
#' @return individual fit plot type
#' @export
IndividualFitPlotType <- function(show_pop=TRUE) {
  return(new("individual_fit_plot_type", show_pop=show_pop))
}

#_______________________________________________________________________________
#----                   recommendation_plot_type class                      ----
#_______________________________________________________________________________

#' 
#' Recommendation plot type.
#' 
#' @export
setClass(
  "recommendation_plot_type",
  representation(
  ),
  contains="quick_plot_type"
)

#' Recommendation plot type.
#' 
#' @return recommendation plot type
#' @export
RecommendationPlotType <- function() {
  return(new("recommendation_plot_type"))
}

#_______________________________________________________________________________
#----                 recommendation_bar_plot_type class                    ----
#_______________________________________________________________________________

#' 
#' Recommendation bar plot type.
#' 
#' @export
setClass(
  "recommendation_bar_plot_type",
  representation(
  ),
  contains="quick_plot_type"
)

#' Recommendation bar plot type.
#' 
#' @return recommendation bar plot type
#' @export
RecommendationBarPlotType <- function() {
  return(new("recommendation_bar_plot_type"))
}

#_______________________________________________________________________________
#----               recommendation_full_plot_type class                     ----
#_______________________________________________________________________________

#' 
#' Recommendation full plot type (profile + bar plot).
#' 
#' @export
setClass(
  "recommendation_full_plot_type",
  representation(
  ),
  contains="quick_plot_type"
)

#' Recommendation full plot type.
#' 
#' @return recommendation full plot type
#' @export
RecommendationFullPlotType <- function() {
  return(new("recommendation_full_plot_type"))
}
