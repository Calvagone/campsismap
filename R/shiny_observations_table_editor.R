
#' Observations table editor class.
#'
#' @export
setClass(
  "observations_table_editor",
  representation(
  ),
  contains="datetime_table_editor"
)

#' Observations table editor.
#' 
#' @param tableReact reactive table
#' @param ns shiny namespace
#' @param fun conversion function (table -> anything)
#' @param defaultObs first observation in table, only if tableReact is NULL
#' @param defaultTime first observation time in table, only if tableReact is NULL
#' @param greyOutPast grey out the rows in past, logical value
#' @export
ObservationsTableEditor <- function(tableReact=NULL, ns=shiny::NS("observations_table"), fun=NULL,
                                    defaultObs=100, defaultTime="06:00", greyOutPast=FALSE) {
  editor <- new("observations_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Observation"), grey_out_past=greyOutPast)
  if (is.null(tableReact)) {
    tableReact <- reactiveVal(editor %>% getInitialTable())
    editor@tableReact <- tableReact
  }
  editor@default_time <- defaultTime
  editor@default_value <- defaultObs
  return(editor)
}

#_______________________________________________________________________________
#----                                getInitialTable                                  ----
#_______________________________________________________________________________

#' @rdname getInitialTable
setMethod("getInitialTable", signature=c("observations_table_editor"), definition=function(object) {
  tibble <- tibble::tibble(Date=character(), Time=character(), Observation=numeric())
  return(tibble)
})
