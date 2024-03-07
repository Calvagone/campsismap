library(testthat)
library(tibble)

context("Test the estimation of individual parameters")

test_that("Method estimate works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  res <- model %>% estimate(dataset=dataset)
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.022, -0.140, -0.392))
  
  quickPlot(model, dataset, etas=res$par)
  
})