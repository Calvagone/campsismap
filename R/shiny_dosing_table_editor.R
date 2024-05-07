
#' Dosing table editor class.
#'
#' @export
setClass(
  "dosing_table_editor",
  representation(
  ),
  contains="datetime_table_editor"
)

#' Dosing table editor.
#' 
#' @param tableReact reactive table
#' @param ns shiny namespace
#' @param fun conversion function (table -> anything)
#' @param initialDose first dose in table, only if tableReact is NULL
#' @param initialTime first dose time in table, only if tableReact is NULL
#' @export
DosingTableEditor <- function(tableReact=NULL, ns=shiny::NS("dosing_table"), fun=NULL,
                              initialDose=100, initialTime="08:00") {
  editor <- new("dosing_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Dose"))
  if (is.null(tableReact)) {
    tableReact <- reactiveVal(editor %>% getInitialTable(init_dose=initialDose, init_time=initialTime))
    editor@tableReact <- tableReact
  }
  editor@default_time <- initialTime
  editor@default_value <- initialDose
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
