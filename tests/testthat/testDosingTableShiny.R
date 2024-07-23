library(shiny)
library(campsismap)
library(lubridate)

ui <- fluidPage(
  DosingTableEditor() %>% getUI()
)
server <- function(input, output, session) {
  
  nowReact <- reactiveVal(lubridate::ymd_hm("24-07-24 20:00"))
  
  dosingEditor <- DosingTableEditor(fun=~Infusion(time=.x$TIME, amount=.x$Dose, compartment=1), greyOutPast=TRUE) %>%
    campsismap::server(input=input, output=output, session=session, dateTime0React=function(){return(NA)}, nowReact=nowReact)
}
shinyApp(ui, server)