library(shiny)
library(campsismap)
library(lubridate)

ui <- fluidPage(
  TargetTableEditor() %>% getUI()
)
server <- function(input, output, session) {
  
  dateTime0React <- shiny::reactiveVal(Sys.time())
  
  targetEditor <- TargetTableEditor(fun=~tibble::tibble(TIME=.x$TIME, TARGET=.x$Target), greyOutPast=FALSE, dateOnly=TRUE) %>%
    campsismap::server(input=input, output=output, session=session, dateTime0React=dateTime0React)
}
shinyApp(ui, server)
