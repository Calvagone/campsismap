library(testthat)
library(tibble)

context("Test the likelihood formulas")

test_that("Population likelihood calculation works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC")
  
  expect_equal(populationLikelihood(model, c(0, 0, 0)) %>% round(digits=2), 1.45)
  expect_equal(populationLikelihood(model, c(0.1, 0.1, 0.1)) %>% round(digits=2), 1.2)
  expect_equal(populationLikelihood(model, c(1, 1, 1)) %>% round(digits=2), -23.29)
})

test_that("Individual likelihood calculation works as expected", {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1)) %>%
    setup(dest="mrgsolve")
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=20, DV=10)) %>%
    export(dest="mrgsolve")
  
  # Optimum
  expect_equal(individualLikelihood(model=model, dataset=dataset, samples=tibble(TIME=20, DV=10),
                                    etas=c(-0.022, -0.140, -0.392)) %>% round(digits=2), -1.24)
  
  # Not optimum
  expect_equal(individualLikelihood(model=model, dataset=dataset, samples=tibble(TIME=20, DV=10),
                                    etas=c(-0.022, -0.140, 0)) %>% round(digits=2), -15.66)
})
