
#_______________________________________________________________________________
#----                                getID                                  ----
#_______________________________________________________________________________

#' Get identifier.
#'
#' @param object generic object
#' @return identifier, character
#' @export
#' @rdname getID
getID <- function(object) {
  stop("No default function is provided")
}

setGeneric("getID", function(object) {
  standardGeneric("getID")
})

#_______________________________________________________________________________
#----                              getDialog                                ----
#_______________________________________________________________________________

#' Get dialog.
#'
#' @param object generic object
#' @param failed logical
#' @param errorMessage error message
#' @param code code to be shown
#' @return a dialog to be shown
#' @export
#' @rdname getDialog
getDialog <- function(object, failed, errorMessage, code) {
  stop("No default function is provided")
}

setGeneric("getDialog", function(object, failed, errorMessage=NULL, code) {
  if (is.null(errorMessage)) {
    errorMessage <- ""
  }
  standardGeneric("getDialog")
})

#_______________________________________________________________________________
#----                             getLabel                                  ----
#_______________________________________________________________________________

#' Get label.
#'
#' @param object generic object
#' @return label, character
#' @export
#' @rdname getLabel
getLabel <- function(object) {
  stop("No default function is provided")
}

setGeneric("getLabel", function(object) {
  standardGeneric("getLabel")
})

#_______________________________________________________________________________
#----                           getMenuItem                                 ----
#_______________________________________________________________________________

#' Get menu item.
#'
#' @param object generic object
#' @return menu item (shiny component)
#' @export
#' @rdname getMenuItem
getMenuItem <- function(object) {
  stop("No default function is provided")
}

setGeneric("getMenuItem", function(object) {
  standardGeneric("getMenuItem")
})

#_______________________________________________________________________________
#----                                server                                 ----
#_______________________________________________________________________________

#' Call to server.
#'
#' @param object generic object
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param ... extra arguments
#' @return this is up to the implementation
#' @export
#' @rdname server
server <- function(object, input, output, session, ...) {
  stop("No default function is provided")
}

setGeneric("server", function(object, input, output, session, ...) {
  standardGeneric("server")
})

#_______________________________________________________________________________
#----                           serverFunction                              ----
#_______________________________________________________________________________

#' Get shiny server function.
#'
#' @param object generic object
#' @return a server function for shiny
#' @export
#' @rdname serverFunction
serverFunction <- function(object) {
  stop("No default function is provided")
}

setGeneric("serverFunction", function(object) {
  standardGeneric("serverFunction")
})

#_______________________________________________________________________________
#----                                getUI                                  ----
#_______________________________________________________________________________

#' Get user interface.
#'
#' @param object generic object
#' @return user interface (shiny component)
#' @export
#' @rdname getUI
getUI <- function(object) {
  stop("No default function is provided")
}

setGeneric("getUI", function(object) {
  standardGeneric("getUI")
})
