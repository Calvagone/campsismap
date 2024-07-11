library(testthat)
library(tibble)
library(tictoc)

context("Test the estimation of individual parameters")
source(paste0("C:/prj/campsismap/tests/testthat/", "testUtils.R"))

getUsecaseFile <- function(usecase, file) {
  return(file.path(testFolder, "usecases", usecase, file))
}

test_that(getTestName("Method estimate works as expected when 1 sample is provided"), {
  
  usecase <- "usecase1"
  
  dosing <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"))
  obs <- read.datetimecsv(getUsecaseFile(usecase, "observations.csv"))
  target <- read.datetimecsv(getUsecaseFile(usecase, "target.csv"))
  
  campsismapTest(estimation, test, env=environment())
})