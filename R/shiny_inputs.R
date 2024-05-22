#_______________________________________________________________________________
#----                       shiny_inputs class                           ----
#_______________________________________________________________________________

#' 
#' Shiny inputs class.
#' 
#' @export
setClass(
  "shiny_inputs",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="shiny_input")
)

#' 
#' Create a list of shiny inputs.
#' 
#' @return a list of shiny inputs
#' @export
ShinyInputs <- function() {
  return(new("shiny_inputs"))
}

#' Find given input in object.
#'
#' @param object list of inputs
#' @param x input
#' @return input stored in object
#' @export
findInput <- function(object, x) {
  input <- object %>% campsismod::find(x)
  if (is.null(input)) {
    stop(paste0("Input ", x@id, " could not be found"))
  }
  return(input)
}

#_______________________________________________________________________________
#----                             findAll                                   ----
#_______________________________________________________________________________

#' Find all elements. A list object is returned with all requested elements.
#'
#' @param object list object
#' @param x first element to find
#' @param ... other elements to find
#' @return filtered list object
#' @export
#' @rdname findAll
findAll <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("findAll", function(object, x, ...) {
  standardGeneric("findAll")
})

#' @rdname findAll
setMethod("findAll", signature=c("shiny_inputs", "shiny_input"), definition=function(object, x, ...) {
  input <- object %>% findInput(x)
  copy <- object
  
  # Clearing list
  object@list <- list()
  
  # Add first input
  object <- object %>% add(input)
  
  # Find and add all other inputs
  for (x_ in list(...)) {
    otherInput <- copy %>% findInput(x_)
    object <- object %>% add(otherInput)
  }
  return(object)
})

#_______________________________________________________________________________
#----                            getEntries                                 ----
#_______________________________________________________________________________

#' Get entries.
#'
#' @param object generic object
#' @return named vector
#' @export
#' @rdname getEntries
getEntries <- function(object) {
  stop("No default function is provided")
}

setGeneric("getEntries", function(object) {
  standardGeneric("getEntries")
})

#' @rdname getEntries
setMethod("getEntries", signature=c("shiny_inputs"), definition=function(object) {
  entries <- object@list %>% purrr::map(~.x@value)
  entries <- setNames(entries, object@list %>% purrr::map(~.x@ns(.x@id)))
  return(entries)
})

#_______________________________________________________________________________
#----                           getInputID                                  ----
#_______________________________________________________________________________

#' Get input ID (including namespace).
#'
#' @param object generic object
#' @param x input to get the ID
#' @return input ID, including namespace
#' @export
#' @rdname getInputID
getInputID <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("getInputID", function(object, x) {
  standardGeneric("getInputID")
})

#' @rdname getInputID
setMethod("getInputID", signature=c("shiny_inputs", "shiny_input"), definition=function(object, x) {
  input <- object %>% findInput(x)
  return(input@ns(input@id))
})

#_______________________________________________________________________________
#----                         getInputValue                                 ----
#_______________________________________________________________________________

#' Get input value.
#'
#' @param object generic object
#' @param x input to get the ID
#' @return input value
#' @export
#' @rdname getInputValue
getInputValue <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("getInputValue", function(object, x) {
  standardGeneric("getInputValue")
})

#' @rdname getInputValue
setMethod("getInputValue", signature=c("shiny_inputs", "shiny_input"), definition=function(object, x) {
  input <- object %>% findInput(x)
  return(input@value)
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Import JSON file.
#' 
#' @param file JSON filename
#' @return a list
#' @importFrom RJSONIO fromJSON
#' @export
importJson <- function(file) {
  # Sometimes, an array is returned
  # Therefore, output always cast to a list
  return(RJSONIO::fromJSON(content=file) %>%
           as.list())
}

#' Read e-Campsis inputs.
#' 
#' @param content object returned by method fromJSON
#' @param defaultInputs default inputs that must be read
#' @return e-Campsis inputs
#' @export
read.inputs <- function(content, defaultInputs) {
  entries <- names(content)
  inputs <- defaultInputs
  
  for (entry in entries) {
    value <- content[[entry]]
    inputs <- tryCatch(
      expr=inputs %>% replaceInput(shinyInput(id=entry, value=""), newvalue=value),
      error=function(cond) {
        print(cond)
        return(inputs)
      })
  }
  
  return(inputs)
}

#_______________________________________________________________________________
#----                           replaceInput                                ----
#_______________________________________________________________________________

#' Replace input.
#'
#' @param object generic object
#' @param x input to replace
#' @param newvalue input updated value
#' @return updated object
#' @export
#' @rdname replaceInput
replaceInput <- function(object, x, newvalue) {
  stop("No default function is provided")
}

setGeneric("replaceInput", function(object, x, newvalue) {
  standardGeneric("replaceInput")
})

#' @rdname replaceInput
setMethod("replaceInput", signature=c("shiny_inputs", "shiny_input", "ANY"), definition=function(object, x, newvalue) {
  input_ <- object %>% findInput(x)
  input_@value <- newvalue
  return(object %>% campsismod::replace(input_))
})

#_______________________________________________________________________________
#----                         updateInputs                                  ----
#_______________________________________________________________________________

#' Update inputs from shiny GUI.
#' 
#' @param object shiny inputs (ecampsis object)
#' @param input shiny input list from GUI
#' @return updated inputs
#' @export
#' @rdname updateInputs
updateInputs <- function(object, input) {
  stop(noDefaultFunctionProvided(object=object, input=input))
}

setGeneric("updateInputs", function(object, input) {
  standardGeneric("updateInputs")
})

#' @rdname updateInputs
setMethod("updateInputs", signature=c("shiny_inputs", "ANY"), definition=function(object, input) {
  retValue <- ShinyInputs()
  for (eInput in object@list) {
    id <- object %>% getInputID(eInput)
    value <- input[[id]]
    eInput@value <- value
    retValue <- retValue %>% add(eInput)
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' @importFrom RJSONIO toJSON
#' @importFrom purrr map set_names
setMethod("write", signature=c("shiny_inputs", "character"), definition=function(object, file, ...) {
  names <- object@list %>% purrr::map_chr(~paste0(.x %>% getName()))
  
  values <- object@list %>%
    purrr::map(~.x@value) %>%
    purrr::set_names(names)
  
  json <- RJSONIO::toJSON(values)
  
  success <- tryCatch(
    writeLines(json, con=file(file)),
    error=function(e) {
      return(FALSE)
    }
  )
  if (is.null(success)) {
    success <- TRUE
  }
  
  return(success)
})

