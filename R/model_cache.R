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
    mod="ANY",
    eta_names="character",
    variable="character"
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
    thetas="numeric"
  ),
  contains="model_cache"
)

Rxode2ModelCache <- function(model, variable, eta_names, settings) {
  config <- commonConfiguration(model, variable, eta_names, dest=new("rxode_engine"))
  rxmod <- config$engineModel
  mod <- rxode2::rxode2(paste0(rxmod@code, collapse="\n"))
  
  retValue <- new("rxode2_model_cache", mod=mod, eta_names=eta_names, variable=variable) %>%
    setupModel(model=model, settings=settings)
  return(retValue)
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

MrgsolveModelCache <- function(model, variable, eta_names, settings) {
  config <- commonConfiguration(model, variable, eta_names, dest=new("mrgsolve_engine"))
  mrgmod <- config$engineModel
  mrgmodCode <- mrgmod %>% campsismod::toString()
  mrgmodHash <- digest::sha1(mrgmodCode)
  mod <- mrgsolve::mcode_cache(model=paste0("mod_", mrgmodHash), code=mrgmodCode, quiet=TRUE)
  
  retValue <- new("mrgsolve_model_cache", mod=mod, eta_names=eta_names, variable=variable) %>%
    setupModel(model=model, settings=settings)
  return(retValue)
}

#_______________________________________________________________________________
#----                           setupModel                                  ----
#_______________________________________________________________________________

#' @rdname setupModel
setMethod("setupModel", signature("rxode2_model_cache", "campsis_model", "simulation_settings"), function(object, model, settings, ...) {
  object@thetas <- rxodeParams(model)
  return(object)
})

#' @rdname setupModel
setMethod("setupModel", signature("mrgsolve_model_cache", "campsis_model", "simulation_settings"), function(object, model, settings, ...) {
  mod <- object@mod
  
  # Retrieve THETA's
  thetas <- model@parameters %>% campsismod::select("theta")
  thetaParams <- thetas@list %>%
    purrr::set_names(thetas@list %>% purrr::map_chr(~.x %>% getNameInModel)) %>%
    purrr::map(~.x@value)
  
  # Apply simulation settings once for all
  solver <- settings@solver
  mod <- mod %>% mrgsolve::update(atol=solver@atol, rtol=solver@rtol, hmax=solver@hmax, maxsteps=solver@maxsteps)
  
  # Update thetas once for all
  if (length(thetaParams) > 0) {
    mod <- mod %>% mrgsolve::update(param=thetaParams)
  }
  
  object@mod <- mod
  return(object)
})

#_______________________________________________________________________________
#----                           simulateModel                               ----
#_______________________________________________________________________________

#' @rdname simulateModel
setMethod("simulateModel", signature("rxode2_model_cache", "tbl_df", "numeric", "simulation_settings"), function(object, dataset, etas, settings, ...) {
  mod <- object@mod
  solver <- settings@solver
  nocb <- settings@nocb@enable
  keep <- object@variable
  
  names(etas) <- object@eta_names
  params <- object@thetas %>%
    append(etas)
  
  results <- rxode2::rxSolve(object=mod, params=params, omega=FALSE, sigma=NULL, events=dataset, returnType="tibble",
                         atol=solver@atol, rtol=solver@rtol, hmax=solver@hmax, maxsteps=solver@maxsteps, method=solver@method,
                         keep=keep, inits=NULL, covsInterpolation=ifelse(nocb, "nocb", "locf"), addDosing=FALSE, addCov=FALSE, cores=1) %>%
    dplyr::rename(TIME=time)
  return(results)
})

#' @rdname simulateModel
setMethod("simulateModel", signature("mrgsolve_model_cache", "tbl_df", "numeric", "simulation_settings"), function(object, dataset, etas, settings, ...) {
  
  nocb <- settings@nocb@enable
  names(etas) <- object@eta_names
  
  object@mod <- object@mod %>%
    mrgsolve::update(param=etas)
  
  results <- object@mod %>%
    mrgsolve::data_set(data=dataset) %>%
    mrgsolve::mrgsim(obsonly=TRUE, output="df", nocb=nocb) %>%
    tibble::as_tibble()
  
  return(results)
})


#_______________________________________________________________________________
#----                             Utilities                                 ----
#_______________________________________________________________________________


commonConfiguration <- function(model, variable, eta_names, dest) {
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
    
    engineModel <- structuralModel %>% export(dest="mrgsolve", outvars=variable, extra_params=eta_names)
    
    # Disable IIV in mrgsolve model
    engineModel@omega <- character(0) # IIV managed by CAMPSIS
  }
  
  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~.x %>% campsismod::toString())
  
  return(list(engineModel=engineModel, cmtNames=cmtNames))
}

