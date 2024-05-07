
#' Datetime table editor class.
#'
#' @export
setClass(
  "datetime_table_editor",
  representation(
    tableReact="ANY",
    extra_variables="character",
    ns="function",
    fun="function",
    outputs="list",
    default_ii="numeric",
    default_time="character",
    default_value="numeric"
  ),
  prototype=prototype(default_ii=24, default_time="08:00", default_value=100) # 24 hours by default
)

#_______________________________________________________________________________
#----                                getUI                                  ----
#_______________________________________________________________________________

#' @rdname getUI
#' @importFrom DT DTOutput
setMethod("getUI", signature=c("datetime_table_editor"), definition=function(object) {
  ns <- object@ns
  ui <- fluidRow(
    column(
      12,
      DT::DTOutput(outputId=getDateTimeTableOutputId(ns)),
      br(),
      actionButton(inputId=getDateTimeTableDeleteButtonId(ns), label="Delete"),
      actionButton(inputId=getDateTimeTableEditButtonId(ns), label="Edit"),
      actionButton(inputId=getDateTimeTableAddButtonId(ns), label="Add")
    )
  )
  return(ui)
})

#_______________________________________________________________________________
#----                                server                                 ----
#_______________________________________________________________________________


#' @rdname server
#' @importFrom DT datatable renderDT
setMethod("server", signature=c("datetime_table_editor", "ANY", "ANY", "ANY"), definition=function(object, input, output, session, dateTime0React) {
  ns <- object@ns
  tableReact <- object@tableReact

  output[[getDateTimeTableOutputId(ns)]] <- DT::renderDT({
    dt <- DT::datatable(tableReact(), filter="none", selection="single", options=list(dom='t', ordering=FALSE))
    return(dt)
  })

  observeEvent(input[[getDateTimeTableDeleteButtonId(ns)]], {
    index <- input[[paste0(getDateTimeTableOutputId(ns), "_rows_selected")]]
    if (!is.null(index)) {
      tableReact(tableReact()[-as.numeric(index), ])
    }
  })

  observeEvent(input[[getDateTimeTableAddButtonId(ns)]], {
    showModal(dateTimeEditorDialog(object=object, add=TRUE))
  })

  observeEvent(input[[getDateTimeTableEditButtonId(ns)]], {
    index <- input[[paste0(getDateTimeTableOutputId(ns), "_rows_selected")]]
    if (!is.null(index)) {
      row <- tableReact()[index, ]
      showModal(dateTimeEditorDialog(object=object, data=row, edit=TRUE))
    }
  })

  observeEvent(input[[getDateTimeDialogAddConfirmButtonId(ns)]], {
    table <- rbind(tableReact(), getRowAsTibble(object, input))
    table <- table %>%
      toDateTimeTable() %>%
      dplyr::select(-Datetime)
    tableReact(table)
    removeModal()
  })

  observeEvent(input[[getDateTimeDialogEditConfirmButtonId(ns)]], {
    index <- input[[paste0(getDateTimeTableOutputId(ns), "_rows_selected")]]
    table <- rbind(tableReact()[-index, ], getRowAsTibble(object, input))
    table <- table %>%
      toDateTimeTable() %>%
      dplyr::select(-Datetime)
    tableReact(table)
    removeModal()
  })

  tableOutputReact <- reactiveVal(list())

  observeEvent(tableReact(), {
    table <- tableReact()
    if (nrow(table) > 0) {
      dateTime0 <- dateTime0React()
      if (is.na(dateTime0)) {
        dateTime0 <- getReferenceDateTime(table=table)
      }
      tableOutputReact(toCampsis(tableReact(), fun=object@fun, dateTime0=dateTime0))
    } else {
      tableOutputReact(list())
    }
  })

  # Outputs
  object@outputs$tableReact <- tableReact
  object@outputs$tableOutputReact <- tableOutputReact

  return(object)
})

#' Get the reference date time.
#' 
#' @param table table content
#' @export
getReferenceDateTime <- function(table) {
  if (nrow(table) > 0) {
    dateTime0 <- toDateTime(date=table[1, ]$Date, time=table[1, ]$Time)
    return(dateTime0)
  } else {
    return(as.POSIXct(NA))
  }
}

getRowAsTibble <- function(object, input) {
  ns <- object@ns
  retValue <- tibble::tibble(
    Date=as.character(input[[getDateTimeDialogDateId(ns)]]),
    Time=posixToTimeStr(input[[getDateTimeDialogTimeId(ns)]]),
  )
  for (variable in object@extra_variables) {
    retValue[[variable]] <- input[[getDateTimeDialogVariableId(ns, variable)]]
  }
  return(retValue)
}

posixToDateStr <- function(x) {
  return(strftime(x, "%Y-%m-%d"))
}

posixToTimeStr <- function(x, seconds=FALSE) {
  if (seconds) {
    return(strftime(x, "%H:%M:%S"))
  } else {
    return(strftime(x, "%H:%M"))
  }
}

#' @importFrom shinyTime timeInput
dateTimeEditorDialog <- function(object, data=NULL, add=NULL, edit=NULL) {
  okLabel <- ""
  okButtonId <- ""
  extraVariables <- object@extra_variables
  ns <- object@ns

  if (isTRUE(add)) {
    okLabel <- "Add entry"
    okButtonId <- getDateTimeDialogAddConfirmButtonId(ns)
  }
  if (isTRUE(edit)) {
    okLabel <- "Update entry"
    okButtonId <- getDateTimeDialogEditConfirmButtonId(ns)
  }
  if (is.null(data)) {
    if (isTRUE(add)) {
      table <- object@tableReact()
      if (nrow(table) > 0) {
        lastRow <- table[nrow(table),]
        suggestedDateTime <- toDateTime(date=lastRow$Date, time=lastRow$Time) + 
          lubridate::hours(object@default_ii)
        date <- posixToDateStr(suggestedDateTime)
        time <- posixToTimeStr(suggestedDateTime, seconds=TRUE)
      } else {
        date <- posixToDateStr(Sys.Date())
        time <- paste0(object@default_time, ":00")
      }
    } else {
      date <- posixToDateStr(Sys.Date())
      time <- paste0(object@default_time, ":00")
    }
  } else {
    date <- data$Date
    time <- paste0(data$Time, ":00")
  }

  uiElements <- list()

  for (index in seq_along(extraVariables)) {
    variable <- extraVariables[index]
   if (is.null(data)) {
     # Add mode
     table <- object@tableReact()
     if (nrow(table) > 0) {
       # Reuse value from last row
       lastRow <- table[nrow(table),]
       value <- lastRow[[variable]]
     } else {
       # Use default value
       value <- object@default_value
     }
    } else {
      # Edit mode
      value <- data[[variable]]
    }
    uiElements[[index]] <- numericInput(inputId=getDateTimeDialogVariableId(ns, variable), label=paste0(variable, ":"), value=value)
  }

  dialog <- modalDialog(
    dateInput(inputId=getDateTimeDialogDateId(ns), label="Date:", value=date),
    shinyTime::timeInput(inputId=getDateTimeDialogTimeId(ns), label="Time:", value=time, seconds=FALSE),
    uiElements,
    actionButton(inputId=okButtonId, label=okLabel),
    easyClose=TRUE, footer=NULL)
  return(dialog)
}

toDateTimeTable <- function(table, sort=TRUE) {
  table <- table %>%
    dplyr::mutate(Datetime=toDateTime(date=.data$Date, time=.data$Time))

  if (sort) {
    table <- table %>%
      dplyr::arrange(Datetime)
  }

  return(table)
}

#' @importFrom lubridate ymd_hm
toDateTime <- function(date, time) {
  return(lubridate::ymd_hm(paste(date, time), tz=Sys.timezone()))
}

#' @importFrom lubridate as.duration interval
toRelativeTimeTable <- function(table, dateTime0) {
  if (!"Datetime" %in% colnames(table)) {
    table <- toDateTimeTable(table=table)
  }

  table <- table %>%
    dplyr::mutate(Datetime0=dateTime0) %>%
    dplyr::mutate(TIME=(lubridate::interval(Datetime0, Datetime, tzone=Sys.timezone()) %>%
                          lubridate::as.duration() %>% as.numeric())/3600) %>%
    dplyr::select(-dplyr::any_of(c("Date", "Time", "Datetime0")))

  return(table)
}

toCampsis <- function(table, fun, dateTime0) {
  table <- toRelativeTimeTable(table=table, dateTime0=dateTime0)
  retValue <- list()

  for (index in seq_len(nrow(table))) {
    row <- table[index, ]
    retValue[[index]] <- fun(row)
  }

  return(retValue)
}

preprocessFun <- function(fun=NULL) {
  if (is.null(fun)) {
    fun <- ~.x
  }
  if (rlang::is_formula(fun)) {
    fun <- rlang::as_function(fun)
    # Reverse class names (then accepted as a function by the S4 class)
    class(fun) <- c("function", "rlang_lambda_function")
  }
  return(fun)
}

#_______________________________________________________________________________
#----                         getInitialTable                               ----
#_______________________________________________________________________________

#' Get initial table.
#'
#' @param object generic object
#' @param ... extra arguments
#' @return initial table, tibble
#' @export
#' @rdname getInitialTable
getInitialTable <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getInitialTable", function(object, ...) {
  standardGeneric("getInitialTable")
})

#_______________________________________________________________________________
#----                            utilities                                  ----
#_______________________________________________________________________________

getDateTimeDialogDateId <- function(ns) {
  return(ns("datetime_dialog_date"))
}

getDateTimeDialogTimeId <- function(ns) {
  return(ns("datetime_dialog_time"))
}

getDateTimeDialogVariableId <- function(ns, variable) {
  return(ns(sprintf("datetime_dialog_%s", variable)))
}

getDateTimeDialogAddConfirmButtonId <- function(ns) {
  return(ns("datetime_dialog_add_confirm_button"))
}

getDateTimeDialogEditConfirmButtonId <- function(ns) {
  return(ns("datetime_dialog_edit_confirm_button"))
}

getDateTimeTableOutputId <- function(ns) {
  return(ns("datetime_table_table_output"))
}

getDateTimeTableDeleteButtonId <- function(ns) {
  return(ns("datetime_table_delete_button"))
}

getDateTimeTableEditButtonId <- function(ns) {
  return(ns("datetime_table_edit_button"))
}

getDateTimeTableAddButtonId <- function(ns) {
  return(ns("datetime_table_add_button"))
}
