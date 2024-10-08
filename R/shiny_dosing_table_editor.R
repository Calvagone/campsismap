
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
#' @param greyOutPast grey out the rows in past, logical value
#' @param dateOnly show only date in editor, logical value
#' @export
DosingTableEditor <- function(tableReact=NULL, ns=shiny::NS("dosing_table"), fun=NULL,
                              initialDose=100, initialTime="08:00",
                              greyOutPast=FALSE, dateOnly=FALSE) {
  editor <- new("dosing_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Dose"),
                grey_out_past=greyOutPast, date_only=dateOnly)
  if (is.null(tableReact)) {
    tableReact <- shiny::reactiveVal(editor %>% getInitialTable(init_dose=initialDose, init_time=initialTime))
    editor@tableReact <- tableReact
  }
  editor@default_time <- initialTime
  editor@default_value <- initialDose
  return(editor)
}

#_______________________________________________________________________________
#----                                getInitialTable                                  ----
#_______________________________________________________________________________

#' @param init_dose initial dose in table
#' @param init_time initial time in table
#' @rdname getInitialTable
setMethod("getInitialTable", signature=c("dosing_table_editor"), definition=function(object, init_dose, init_time) {
  dates <- c(Sys.Date())
  times <- c(init_time)
  doses <- c(init_dose)
  tibble <- tibble::tibble(Date=dates, Time=times, Dose=doses)
  return(tibble)
})
