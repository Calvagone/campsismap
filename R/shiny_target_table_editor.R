
#' Target table editor class.
#'
#' @export
setClass(
  "target_table_editor",
  representation(
  ),
  contains="datetime_table_editor"
)

#' Target table editor.
#' 
#' @param tableReact reactive table
#' @param ns shiny namespace
#' @param fun conversion function (table -> anything)
#' @param initialTarget first target value in table, only if tableReact is NULL
#' @param initialTime first target time in table, only if tableReact is NULL
#' @param greyOutPast grey out the rows in past, logical value
#' @param dateOnly show only date in editor, logical value
#' @importFrom shiny reactiveVal
#' @export
TargetTableEditor <- function(tableReact=NULL, ns=shiny::NS("target_table"), fun=NULL,
                              initialTarget=100, initialTime="00:00",
                              greyOutPast=FALSE, dateOnly=TRUE) {
  editor <- new("target_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Target"),
                grey_out_past=greyOutPast, date_only=dateOnly)
  if (is.null(tableReact)) {
    tableReact <- shiny::reactiveVal(editor %>% getInitialTable(init_target=initialTarget, init_time=initialTime))
    editor@tableReact <- tableReact
  }
  editor@default_time <- initialTime
  editor@default_value <- initialTarget
  return(editor)
}

#_______________________________________________________________________________
#----                                getInitialTable                                  ----
#_______________________________________________________________________________

#' @param init_target initial dose in table
#' @param init_time initial time in table
#' @importFrom tibble tibble
#' @rdname getInitialTable
setMethod("getInitialTable", signature=c("target_table_editor"), definition=function(object, init_target, init_time) {
  dates <- c(Sys.Date())
  times <- c(init_time)
  target <- c(init_target)
  tibble <- tibble::tibble(Date=dates, Time=times, Target=target)
  return(tibble)
})
