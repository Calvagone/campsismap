library(testthat)

context("Test the likelihood formulas")

test_that("Population likelihood works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC")
  
  expect_equal(populationLikelihood(model, c(0, 0, 0)) %>% round(digits=2), 1.45)
  expect_equal(populationLikelihood(model, c(0.1, 0.1, 0.1)) %>% round(digits=2), 1.2)
  expect_equal(populationLikelihood(model, c(1, 1, 1)) %>% round(digits=2), -23.29)
})
