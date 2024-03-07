#_______________________________________________________________________________
#----                        model_cache class                              ----
#_______________________________________________________________________________

#' Error model class.
#' 
#' @slot mod rxode2 or mrgsolve model (in cache)
#' @slot settings Campsis settings
#' @export
setClass(
  "model_cache",
  representation(
    mod="ANY"
  )
)

commonConfiguration <- function(model, settings) {
  # Extra argument declare (for mrgsolve only)
  user_declare <- settings@declare@variables
  declare <- unique(settings@declare@variables)  
                      
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
    
    # Set ETA's as extra parameters in mrgsolve
    etaNames <- (model@parameters %>% select("omega"))@list %>%
      purrr::keep(~isDiag(.x)) %>%
      purrr::map_chr(~getNameInModel(.x))
    
    # Extra care to additional outputs which need to be explicitly declared with mrgsolve 
    outvars_ <- outvars[!(outvars %in% dropOthers())]
    outvars_ <- unique(c(outvars_, "ARM", "EVENT_RELATED"))
    if (dosing) {
      # These variables are not output by default in mrgsolve when dosing is TRUE
      outvars_ <- unique(c(outvars_, "EVID", "CMT", "AMT"))
    }
    engineModel <- structuralModel %>% export(dest="mrgsolve", outvars=outvars_, extra_params=c(etaNames, declare))
    
    # Disable IIV in mrgsolve model
    engineModel@omega <- character(0) # IIV managed by CAMPSIS
  }
  
  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~.x %>% toString())
  
  return(list(declare=declare, engineModel=engineModel, cmtNames=cmtNames))
}

