library(testthat)
library(tibble)
library(tictoc)

context("Test the estimation of individual parameters")

test_that("Method estimate works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  tic()
  res <- model %>% estimate(dataset=dataset) # 45s
  toc()
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.022, -0.140, -0.392))
  
  quickPlot(model, dataset, etas=res$par)
  
})