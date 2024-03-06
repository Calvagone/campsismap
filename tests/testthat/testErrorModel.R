library(testthat)

context("Test the error models")

test_that("Proportional error model works as expected", {
  set.seed(1)
  
  # Let's generate some data with a proportional error of 20%
  ipred <- rep(50, 10000)
  data <- ipred + ipred*rnorm(n=10000, mean=0, sd=0.2)
  
  error <- ProportionalErrorModel(0.2)
  roundedSd <- sd(data - ipred) %>% round()
  expect_equal(rep(roundedSd, 10000), error %>% computeSd(ipred))

  
})
