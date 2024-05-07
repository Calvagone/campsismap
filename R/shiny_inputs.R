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
  retValue <- EmptyInputs()
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

