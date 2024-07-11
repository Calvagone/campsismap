library(testthat)
library(tibble)
library(tictoc)
library(dplyr)

context("Test the target definition objects")
source(paste0("C:/prj/campsismap/tests/testthat/", "testUtils.R"))

test_that(getTestName("Test simple recommendation"), {
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=0, amount=2000))     # Dose to adapt
  
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.25))
  
  model %>% recommend(dataset=dataset, target=target)
})