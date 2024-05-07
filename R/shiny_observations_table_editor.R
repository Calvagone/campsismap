
#' Observations table editor class.
#'
#' @export
setClass(
  "observations_table_editor",
  representation(
  ),
  contains="datetime_table_editor"
)

ObservationsTableEditor <- function(tableReact=NULL, ns=shiny::NS("observations_table"), fun=NULL) {
  editor <- new("observations_table_editor", tableReact=NA, ns=ns, fun=preprocessFun(fun), extra_variables=c("Observation"))
  if (is.null(tableReact)) {
    tableReact <- reactiveVal(editor %>% getInitialTable(init_obs=50, init_time="12:00"))
    editor@tableReact <- tableReact
  }
  return(editor)
}

#_______________________________________________________________________________
#----                                getInitialTable                                  ----
#_______________________________________________________________________________

#' @rdname getInitialTable
setMethod("getInitialTable", signature=c("observations_table_editor"), definition=function(object, init_obs, init_time) {
  dates <- c(Sys.Date())
  times <- c(init_time)
  observations <- c(init_obs)
  tibble <- tibble::tibble(Date=dates, Time=times, Observation=observations)
  return(tibble)
})
