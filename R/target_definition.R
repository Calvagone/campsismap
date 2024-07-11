
#_______________________________________________________________________________
#----                         target_definition                             ----
#_______________________________________________________________________________

#' Target definition (abstract)
#' 
#' @export
setClass(
  "target_definition",
  representation(
  )
)

#_______________________________________________________________________________
#----                     target_definition_per_window                      ----
#_______________________________________________________________________________

#' Target definition per window.
#' 
#' @slot table table
#' @export
setClass(
  "target_definition_per_window",
  representation(
    table = "data.frame"
  ),
  contains="target_definition"
)

#' Target definition per window.
#' TIME VALUE
#' 0    X   Target of X from 0 to 24 (0 or negative value must be provided)
#' 24   Y   Target of Y from 24 to Inf, etc.
#' 
#' @param table data frame with TIME and VALUE columns
#' @export
TargetDefinitionPerWindow <- function(table=data.frame(TIME=numeric(0), VALUE=numeric(0))) {
  return(new("target_definition_per_window", table=table))
}

#_______________________________________________________________________________
#----                     target_definition_per_dose                        ----
#_______________________________________________________________________________

#' Target definition per dose.
#' DOSENO VALUE
#' 1      X   Dose 1 should attain a target of X
#' 2      Y   Dose 2 should attain a target of Y, etc.
#' 
#' @slot table table
#' @export
setClass(
  "target_definition_per_dose",
  representation(
    table = "data.frame"
  ),
  contains="target_definition"
)

#' Target definition per dose.
#' 
#' @param table data frame with DOSENO and VALUE columns
#' @export
TargetDefinitionPerDose <- function(table=data.frame(DOSENO=integer(0), VALUE=numeric(0))) {
  return(new("target_definition_per_dose", table=table))
}

#_______________________________________________________________________________
#----                     target_definition_effective                       ----
#_______________________________________________________________________________

#' Target definition (effective).
#' TIME   VALUE
#' 24     X   Predose 2 should attain a target of X
#' 48     Y   Predose 3 should attain a target of Y
#' 
#' @slot table table
#' @export
setClass(
  "target_definition_effective",
  representation(
    table = "data.frame"
  ),
  contains="target_definition"
)

#' Target definition (effective).
#' 
#' @param table data frame with (effective) TIME and VALUE columns
#' @export
TargetDefinitionEffective <- function(table=data.frame(TIME=numeric(0), VALUE=numeric(0))) {
  return(new("target_definition_effective", table=table))
}

#_______________________________________________________________________________
#----                               export                                  ----
#_______________________________________________________________________________

setMethod("export", signature=c("target_definition_per_window", "target_definition_per_dose"), definition=function(object, dest, dosing) {
  dosingTimes <- dosing$TIME
  if (any(duplicated(dosingTimes))) {
    stop("Some dosing times are duplicated")
  }
  table <- object@table
  
  updatedTable <- purrr::map2_df(.x=seq_along(dosingTimes), .y=dosingTimes, .f=function(doseno, time) {
    tmp <- table %>%
      dplyr::mutate(CONDITION=time >= .data$TIME) %>%
      dplyr::filter(CONDITION)
    return(tibble::tibble(DOSENO=doseno, VALUE=tmp$VALUE[length(tmp$VALUE)]))
  })
  
  dest@table <- updatedTable
  return(dest)
})

setMethod("export", signature=c("target_definition_per_window", "target_definition_effective"), definition=function(object, dest, dosing) {
  targetDose <- object %>% export(dest=TargetDefinitionPerDose(), dosing=dosing)
  
  
  dest@table <- updatedTable
  return(dest)
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the number of rows contained in the target definition.
#' 
#' @param x target definition
#' @return a number
setMethod("length", signature=c("target_definition"), definition=function(x) {
  return(nrow(x@table))
})

