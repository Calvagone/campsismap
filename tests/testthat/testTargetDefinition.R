library(testthat)
library(tibble)
library(tictoc)
library(dplyr)

context("Test the target definition objects")
source(paste0("C:/prj/campsismap/tests/testthat/", "testUtils.R"))

getUsecaseFile <- function(usecase, file) {
  return(file.path(testFolder, "usecases", usecase, file))
}

test_that(getTestName("Target definition per window can be converted to target definition per dose"), {
  
  usecase <- "usecase1"
  
  dateTime0 <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"))$Datetime[1]
  dosing <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"), relative=TRUE, dateTime0=dateTime0)
  obs <- read.datetimecsv(getUsecaseFile(usecase, "observations.csv"), relative=TRUE, dateTime0=dateTime0)
  target <- read.datetimecsv(getUsecaseFile(usecase, "target.csv"), relative=TRUE, dateTime0=dateTime0)
  
  targetWindow <- TargetDefinitionPerWindow(target %>% rename(VALUE=Target))
  targetDose <- targetWindow %>% export(TargetDefinitionPerDose(), dosing=dosing)
  
  expect_equal(length(targetDose), nrow(dosing))
  expect_equal(targetDose, TargetDefinitionPerDose(tibble(DOSENO=1:8, VALUE=c(50,50,60,60,70,70,70,70))))
})