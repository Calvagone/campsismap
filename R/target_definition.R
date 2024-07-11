
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
TargetDefinitionPerWindow <- function(table) {
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
TargetDefinitionPerDose <- function(table) {
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
TargetDefinitionEffective <- function(table) {
  return(new("target_definition_effective", table=table))
}
