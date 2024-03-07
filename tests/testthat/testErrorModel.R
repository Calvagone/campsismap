library(testthat)

context("Test the error models")

test_that("Proportional error model works as expected", {
  set.seed(1)
  
  # Let's generate some data with a proportional error of 20%
  samples <- 100000
  x <- rep(50, samples)
  data <- x + x*rnorm(n=samples, mean=0, sd=0.2)
  
  error <- ProportionalErrorModel(0.2)
  roundedSd <- sd(data - x) %>% round(1)
  expect_equal(rep(roundedSd, samples), error %>% computeSd(x))
})

test_that("Combined error model works as expected", {
  set.seed(2)
  
  # Let's generate some data with a proportional error of 20% and additive error of 3
  samples <- 100000
  x <- rep(50, samples)
  data <- x + x*rnorm(n=samples, mean=0, sd=0.2) + rnorm(n=samples, mean=0, sd=3)
  
  error <- CombinedErrorModel(0.2, 3)
  roundedSd <- sd(data - x) %>% round(1)
  expect_equal(rep(roundedSd, samples), error %>% computeSd(x) %>% round(1))
})
