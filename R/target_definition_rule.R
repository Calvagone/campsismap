#_______________________________________________________________________________
#----                   target_definition_rule class                        ----
#_______________________________________________________________________________

#' Target definition rule class.
#' 
#' @export
setClass(
  "target_definition_rule",
  representation(
  ),
  contains="pmx_element"
)

#_______________________________________________________________________________
#----                        trough_time_rule class                         ----
#_______________________________________________________________________________

#' Trough time rule class.
#' 
#' @export
setClass(
  "trough_time_rule",
  representation(
    ii = "numeric",
    use_next_dose = "logical"
  ),
  contains="target_definition_rule"
)

#' Rule to identity when the trough time is.
#' 
#' @param ii dosing interval
#' @param use_next_dose if TRUE next dose will be used as trough time, if FALSE dose + ii is used as trough time
#' @export
TroughTimeRule <- function(ii=24, use_next_dose=TRUE) {
  return(new("trough_time_rule", ii=as.numeric(ii), use_next_dose=use_next_dose))
}

#_______________________________________________________________________________
#----                               getName                                 ----
#_______________________________________________________________________________

setMethod("getName", signature=c("trough_time_rule"), definition=function(x) {
  return("Trough time rule")
})

