
#_______________________________________________________________________________
#----                              addDV                                    ----
#_______________________________________________________________________________

#' @rdname addDV
setMethod("addDV", signature("dataset", "tbl_df"), function(object, x) {
  return(addDVDelegate(object, x))
})

#' @rdname addDV
setMethod("addDV", signature("dataset", "data.frame"), function(object, x) {
  return(addDVDelegate(object, x))
})

addDVDelegate <- function(object, x) {
  assertthat::assert_that(all(c("TIME", "DV") %in% colnames(x)), msg="x must contain 2 columns: TIME and DV")
  
  obs <- Observations(times=x$TIME)
  obs@dv <- x$DV # Hidden slot in Campsis
  
  object <- object %>%
    add(obs)
  
  return(object)
}

#_______________________________________________________________________________
#----                              getDV                                    ----
#_______________________________________________________________________________

#' @rdname getDV
setMethod("getDV", signature("dataset"), function(object) {
  observations <- dataset@arms@list[[1]]@protocol@observations
  retValue <- observations@list %>%
    purrr::map_df(~tibble::tibble(TIME=.x@times, DV=.x@dv)) %>%
    dplyr::filter(!is.na(DV))
  return(retValue)
})

getObservationTimes <- function(dataset) {
  return(dataset@arms@list[[1]]@protocol@observations %>% getTimes())
}