
#' Dosing table editor class.
#'
#' @export
setClass(
  "dosing_table_editor",
  representation(
  ),
  contains="datetime_table_editor"
)

DosingTableEditor <- function(tableReact=NULL, ns=shiny::NS("dosing_table"), fun=NULL) {
  editor <- new("dosing_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Dose"))
  if (is.null(tableReact)) {
    tableReact <- reactiveVal(editor %>% getInitialTable(init_dose=100, init_time="08:00"))
    editor@tableReact <- tableReact
  }
  return(editor)
}

#_______________________________________________________________________________
#----                                getInitialTable                                  ----
#_______________________________________________________________________________

#' @rdname getInitialTable
setMethod("getInitialTable", signature=c("dosing_table_editor"), definition=function(object, init_dose, init_time) {
  dates <- c(Sys.Date())
  times <- c(init_time)
  doses <- c(init_dose)
  tibble <- tibble::tibble(Date=dates, Time=times, Dose=doses)
  return(tibble)
})
