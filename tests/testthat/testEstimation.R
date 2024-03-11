library(testthat)
library(tibble)
library(tictoc)

context("Test the estimation of individual parameters")
source(paste0("", "testUtils.R"))

test_that(getTestName("Method estimate works as expected when 1 sample is provided"), {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=20, DV=10))
  
  estimation <- expression(model %>% setup(dest=destEngine) %>% estimate(dataset=dataset))
  test <- expression(
    expect_equal(as.numeric(results$par) %>% round(3), c(-0.022, -0.140, -0.392)),
    quickPlot(model, dataset, etas=results$par)
  )
  
  campsismapTest(estimation, test, env=environment())
})

test_that(getTestName("Method estimate works as expected when 2 samples are provided"), {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addDV(tibble(TIME=c(20,30), DV=c(10,14)))
  
  estimation <- expression(model %>% setup(dest=destEngine) %>% estimate(dataset=dataset))
  test <- expression(
    expect_equal(as.numeric(results$par) %>% round(3), c(-0.028, -0.186, -1.018)),
    quickPlot(model, dataset, etas=results$par)
  )
  
  campsismapTest(estimation, test, env=environment())
})
