library(testthat)
library(tibble)

context("Test the calculation of individual predictions")

test_that("Method individualPrediction works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC")
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  results <- model %>% simulateModel(dataset=dataset, etas=c(0,0,-0.4))
  
  quickPlot(model, dataset)
  quickPlot(model, dataset, etas=c(0,0,-0.4))
  
})
