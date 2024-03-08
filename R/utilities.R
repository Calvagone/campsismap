#' Preprocess the simulation settings.
#' 
#' @param settings simulation settings
#' @param dest destination engine
#' @return updated simulation settings
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessSettings <- function(settings, dest) {
  # Use default settings if not specified
  if (is.null(settings)) {
    settings <- Settings()
  }
  
  # Check if NOCB is specified
  enable <- settings@nocb@enable
  if (is.na(enable)) {
    if (dest=="mrgsolve") {
      enable <- TRUE
    } else {
      enable <- FALSE
    }
  }
  # Assign final value
  settings@nocb@enable <- enable
  
  # Preprocess slice_size
  settings@hardware@slice_size <- as.integer(1)
  
  return(settings)
}
