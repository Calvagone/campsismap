
#_______________________________________________________________________________
#----                      campsismap_model class                           ----
#_______________________________________________________________________________

#' Campsismap model class.
#' 
#' @slot model Campsis model ready for estimation
#' @slot omega pre-computed OMEGA matrix
#' @slot eta_names ETA names in model
#' @export
setClass(
  "campsismap_model",
  representation(
    model="campsis_model",
    omega="matrix",
    eta_names="character",
    variable="character"
  )
)

#' Create a new Campsismap model.
#' 
#' @param model Campsis model
#' @param variable variable of the concentration in model
#' @return a Campsismap mode
#' @export
CampsismapModel <- function(model, variable) {
  # Derive OMEGA matrix
  omega <- rxodeMatrix(model)
  
  # Store ETA names once for all and delete names in OMEGA matrix
  eta_names <- colnames(omega)
  colnames(omega) <- NULL
  rownames(omega) <- NULL

  concEquation <- model %>%
    find(Equation(variable))
  if (is.null(concEquation)) {
    stop(sprintf("Could not find equation '%s' in model code"), variable)
  }
  
  model <- model %>%
    delete(ErrorRecord()) %>%
    delete(concEquation)
  
  # Append concentration equation to the ODE record 
  model <- model %>%
    add(concEquation, pos=campsismod::Position(OdeRecord()))
  
  # Discard omegas and sigmas
  model@parameters@list <- model@parameters@list %>%
    purrr::discard(~is(.x, "double_array_parameter"))
  
  return(new("campsismap_model", model=model, omega=omega, eta_names=eta_names, variable=variable))
}

#_______________________________________________________________________________
#----                           quickPlot                                   ----
#_______________________________________________________________________________

#' @rdname quickPlot
setMethod("quickPlot", signature("campsismap_model", "dataset", "numeric"), function(model, dataset, etas) {
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
  
  # Simulate
  results <- individualPrediction(model=model, dataset=dataset, etas=etas)
  
  # Retrieve DV
  dv <- results %>%
    dplyr::filter(DV > 0)
  
  plot <- spaghettiPlot(results %>% dplyr::mutate(ID=1), output=model@variable)
  if (nrow(dv) > 0) {
    plot <- plot +
      ggplot2::geom_point(mapping=ggplot2::aes(x=TIME, y=DV, group=NULL), data=dv, color="blue")
  }
  
  return(plot)
})
