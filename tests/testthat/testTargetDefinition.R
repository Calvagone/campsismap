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
  
  dateTime0 <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"))$Datetime[1]
  dosing <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"), relative=TRUE, dateTime0=dateTime0)
  obs <- read.datetimecsv(getUsecaseFile(usecase, "observations.csv"), relative=TRUE, dateTime0=dateTime0)
  target <- read.datetimecsv(getUsecaseFile(usecase, "target.csv"), relative=TRUE, dateTime0=dateTime0)
  
  campsismapTest(estimation, test, env=environment())
})