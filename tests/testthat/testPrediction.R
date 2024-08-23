library(testthat)
library(tibble)

context("Test the calculation of individual predictions")
source(paste0("", "testUtils.R"))

test_that(getTestName("Method predict works as expected"), {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC")
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=20, DV=10))
  
  env <- environment()
  prediction <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      predict(dataset=dataset, etas=c(0,0,-0.4))
  )
  test <- expression(
    expect_equal(results$CONC %>% round(3), c(8.821))
  )

  campsismapTest(prediction, test, env=environment())
  
  quickPlot(model, dataset)
  quickPlot(model, dataset, etas=c(0,0,-0.4))
  
})
