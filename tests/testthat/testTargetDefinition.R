library(testthat)
library(tibble)
library(tictoc)
library(dplyr)

context("Test the target definition objects")
source(paste0("", "testUtils.R"))

getUsecaseFile <- function(usecase, file) {
  if (testFolder=="") {
    return(file.path("usecases", usecase, file))
  } else {
    return(file.path(testFolder, "usecases", usecase, file))
  }
}

importUsecase <- function(usecase) {
  dateTime0 <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"))$Datetime[1]
  dosing <- read.datetimecsv(getUsecaseFile(usecase, "dosing.csv"), relative=TRUE, dateTime0=dateTime0)
  obs <- read.datetimecsv(getUsecaseFile(usecase, "observations.csv"), relative=TRUE, dateTime0=dateTime0)
  target <- read.datetimecsv(getUsecaseFile(usecase, "target.csv"), relative=TRUE, dateTime0=dateTime0)
  return(list(dateTime0=dateTime0, dosing=dosing, obs=obs, target=target))
}

test_that(getTestName("Target definition per window can be converted to target definition per dose"), {
  
  usecase <- "usecase1"
  data <- importUsecase(usecase)
  target <- data$target
  dosing <- data$dosing
  
  targetWindow <- TargetDefinitionPerWindow(target %>% rename(VALUE=Target))
  targetDose <- targetWindow %>% export(TargetDefinitionPerDose(), dosing=dosing)
  
  expect_equal(length(targetDose), nrow(dosing))
  expect_equal(targetDose, TargetDefinitionPerDose(tibble(DOSENO=1:8, VALUE=c(50,50,60,60,70,70,70,70))))
})

test_that(getTestName("Target definition per window can be converted to an effective target definition for campsismap"), {
  
  usecase <- "usecase1"
  data <- importUsecase(usecase)
  target <- data$target
  dosing <- data$dosing
  
  rules <- Rules(TroughTimeRule(ii=12, use_next_dose=TRUE))
  targetWindow <- TargetDefinitionPerWindow(target %>% rename(VALUE=Target))
  targetEffectiveA <- targetWindow %>% export(TargetDefinitionEffective(), dosing=dosing, rules=rules)
  
  expect_equal(length(targetEffectiveA), nrow(dosing))
  expect_equal(targetEffectiveA, TargetDefinitionEffective(tibble(TIME=seq(12,96,by=12), VALUE=c(50,50,60,60,70,70,70,70), LAST_DOSENO=1:8, LAST_DOSE_TIME=seq(0,84,by=12))))
  
  # Or in 2 steps
  targetDose <- targetWindow %>% export(TargetDefinitionPerDose(), dosing=dosing)
  targetEffectiveB <- targetDose %>% export(TargetDefinitionEffective(), dosing=dosing, rules=rules)
  
  expect_true(s4Eq(targetEffectiveA, targetEffectiveB))
})

test_that(getTestName("Negative time in target should work"), {
  rules <- Rules(TroughTimeRule(ii=24, use_next_dose=TRUE))
  targetWindow <- TargetDefinitionPerWindow(tibble(TIME=-8, VALUE=100))
  targetEffective <- targetWindow %>% export(TargetDefinitionEffective(), dosing=tibble::tibble(TIME=c(0,24,48)), rules=rules)
  expect_equal(targetEffective, TargetDefinitionEffective(tibble(TIME=c(24,48,72), VALUE=100, LAST_DOSENO=1:3, LAST_DOSE_TIME=c(0,24,48))))
})

test_that(getTestName("Target must contain the correct column names"), {
  expect_error(TargetDefinitionPerWindow(tibble(TIME=-8, Target=100)), regexp="Expected columns")
})
