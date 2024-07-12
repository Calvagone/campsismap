
#_______________________________________________________________________________
#----                           addSamples                                  ----
#_______________________________________________________________________________

#' @rdname addSamples
setMethod("addSamples", signature("dataset", "tbl_df"), function(object, x) {
  return(addSamplesDelegate(object, x))
})

#' @rdname addSamples
setMethod("addSamples", signature("dataset", "data.frame"), function(object, x) {
  return(addSamplesDelegate(object, x))
})

addSamplesDelegate <- function(object, x) {
  assertthat::assert_that(all(c("TIME", "DV") %in% colnames(x)), msg="x must contain 2 columns: TIME and DV")
  
  obs <- Observations(times=x$TIME)
  obs@dv <- x$DV # Hidden slot in Campsis
  
  object <- object %>%
    add(obs)
  
  return(object)
}

#_______________________________________________________________________________
#----                           getSamples                                  ----
#_______________________________________________________________________________

#' @rdname getSamples
setMethod("getSamples", signature("dataset"), function(object) {
  observations <- object@arms@list[[1]]@protocol@observations
  retValue <- observations@list %>%
    purrr::map_df(.f=function(x) {
      times <- x@times
      dv <- x@dv
      if (length(dv) > 0) {
        return(tibble::tibble(TIME=times, DV=dv))
      } else {
        return(tibble::tibble(TIME=numeric(), DV=numeric()))
      }
    })
  return(retValue)
})

#_______________________________________________________________________________
#----                           getSimulationTimes                          ----
#_______________________________________________________________________________

#' @rdname getSimulationTimes
setMethod("getSimulationTimes", signature("dataset"), function(object) {
  observations <- object@arms@list[[1]]@protocol@observations
  retValue <- observations@list %>%
    purrr::map(.f=function(x) {
      times <- x@times
      dv <- x@dv
      if (length(dv) == 0) {
        return(times)
      } else {
        return(numeric())
      }
    }) %>%
    purrr::flatten_dbl()
  return(retValue)
})

#_______________________________________________________________________________
#----                     retrieveDoseAmount                                ----
#_______________________________________________________________________________

#' @rdname retrieveDoseAmount
setMethod("retrieveDoseAmount", signature("dataset", "integer"), function(object, dose_number) {
  object <- checkAssignedDoseNumbers(object)
  retValue <- object@arms@list[[1]]@protocol@treatment@list %>%
    purrr::keep(~.x@dose_number==dose_number) %>%
    purrr::map_dbl(~.x@amount)
  return(retValue)
})

#_______________________________________________________________________________
#----                       updateDoseAmount                                ----
#_______________________________________________________________________________

checkAssignedDoseNumbers <- function(dataset) {
  doseNumbers <- dataset@arms@list[[1]]@protocol@treatment@list %>%
    purrr::map_int(~.x@dose_number)
  
  if (any(is.na(doseNumbers))) {
    # Call to internal method assignDoseNumber in Campsis
    dataset@arms@list[[1]]@protocol@treatment <- dataset@arms@list[[1]]@protocol@treatment %>%
      campsis:::assignDoseNumber()
  } else {
    # Don't do anything
  }
  
  return(dataset)
}

#' @rdname updateDoseAmount
setMethod("updateDoseAmount", signature("dataset", "numeric", "integer"), function(object, amount, dose_number) {
  object <- checkAssignedDoseNumbers(object)
  object@arms@list[[1]]@protocol@treatment@list <- object@arms@list[[1]]@protocol@treatment@list %>%
    purrr::map(.f=function(admin) {
      if (admin@dose_number==dose_number) {
        admin@amount <- amount
      }
      return(admin)
    })
  return(object)
})
