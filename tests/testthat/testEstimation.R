library(testthat)
library(tibble)
library(tictoc)

context("Test the estimation of individual parameters")

test_that("Method estimate works as expected (mrgsolve)", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC", dest="mrgsolve") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  tic()
  res <- model %>% estimate(dataset=dataset)
  toc()
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.022, -0.140, -0.392))
  
  quickPlot(model, dataset, etas=res$par)
})

test_that("Method estimate works as expected (rxode2)", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC", dest="rxode2") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  tic()
  res <- model %>% estimate(dataset=dataset)
  toc()
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.022, -0.140, -0.392))
  
  quickPlot(model, dataset, etas=res$par)
})

test_that("Method estimate works as expected with 2 samples (mrgsolve)", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC", dest="mrgsolve") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=c(20,30), DV=c(10,14)))
  
  tic()
  res <- model %>% estimate(dataset=dataset)
  toc()
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.028, -0.186, -1.018))
  
  quickPlot(model, dataset, etas=res$par)
})

test_that("Method estimate works as expected with 2 samples (rxode2)", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC", dest="rxode2") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=c(20,30), DV=c(10,14)))
  
  tic()
  res <- model %>% estimate(dataset=dataset)
  toc()
  expect_equal(as.numeric(res$par) %>% round(3), c(-0.028, -0.186, -1.018))
  
  quickPlot(model, dataset, etas=res$par)
})
