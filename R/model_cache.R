#_______________________________________________________________________________
#----                        model_cache class                              ----
#_______________________________________________________________________________

#' Model cache class.
#' 
#' @slot mod rxode2 or mrgsolve model (in cache)
#' @export
setClass(
  "model_cache",
  representation(
    mod="ANY"
  )
)

#_______________________________________________________________________________
#----                     rxode2_model_cache class                          ----
#_______________________________________________________________________________

#' Rxode2 model cache class.
#' 
#' @export
setClass(
  "rxode2_model_cache",
  representation(
  ),
  contains="model_cache"
)

Rxode2ModelCache <- function(model, variable, eta_names) {
  config <- commonConfiguration(model, variable, eta_names)
  rxmod <- config$engineModel
  mod <- rxode2::rxode2(paste0(rxmod@code, collapse="\n"))
  return(new("rxode2_model_cache", mod=mod))
}

#_______________________________________________________________________________
#----                   mrgsolve_model_cache class                          ----
#_______________________________________________________________________________

#' Mrgsolve model cache class.
#' 
#' @export
setClass(
  "mrgsolve_model_cache",
  representation(
  ),
  contains="model_cache"
)

MrgsolveModelCache <- function(model, variable, eta_names) {
  common <- commonConfiguration(model, variable, eta_names)
  mrgmod <- config$engineModel
  mrgmodCode <- mrgmod %>% toString()
  mrgmodHash <- digest::sha1(mrgmodCode)
  mod <- mrgsolve::mcode_cache(model=paste0("mod_", mrgmodHash), code=mrgmodCode, quiet=TRUE)
  return(new("mrgsolve_model_cache", mod=mod))
}


#_______________________________________________________________________________
#----                             Utilities                                 ----
#_______________________________________________________________________________


commonConfiguration <- function(model, variable, eta_names) {
  # Export to RxODE / rxode2
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% export(dest="RxODE")
    
  # Export to mrgsolve
  } else if (is(dest, "mrgsolve_engine")) {
    
    # Export structural model (all THETA's to 0, all OMEGA's to 0)
    structuralModel <- model
    structuralModel@parameters@list <- structuralModel@parameters@list %>% purrr::map(.f=function(parameter) {
      if (is(parameter, "theta") || is(parameter, "omega")) {
        parameter@value <- 0
      }
      return(parameter)
    })
    
    engineModel <- structuralModel %>% export(dest="mrgsolve", outvars=variable, extra_params=c(eta_names))
    
    # Disable IIV in mrgsolve model
    engineModel@omega <- character(0) # IIV managed by CAMPSIS
  }
  
  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~.x %>% toString())
  
  return(list(engineModel=engineModel, cmtNames=cmtNames))
}

