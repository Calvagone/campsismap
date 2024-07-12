library(testthat)
library(tibble)
library(tictoc)
library(dplyr)

context("Test the target definition objects")
source(paste0("", "testUtils.R"))

getRules <- function() {
  rules <- Rules() %>%
    add(TroughTimeRule(ii=12)) %>%
    add(DoseRoundingRule())
  return(rules)
}

test_that(getTestName("Test basic recommendation"), {
  model <- model_suite$pk$'2cpt_fo'
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=12, amount=2000))    # Dose to adapt

  mapModel <- CampsismapModel(model=model, "CONC") %>%
    campsismap::setup(dest="mrgsolve")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  dataset_ <- mapModel %>%
    recommend(dataset=dataset, target=target, now=10, rules=getRules()) %>%
    add(Observations(seq(0,100,by=0.1)))
  
  expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000) # Didn't changed because of 'now'
  expect_equal(dataset_ %>% retrieveDoseAmount(2), 3425)
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)
  
  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(xintercept=24, color="red", linetype="dotted") +
    ggplot2::geom_hline(yintercept=target@table$VALUE, color="red", linetype="dotted")
  
  # Test 2: negative recommendation is not possible, 0 is returned
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=5)) 
  
  dataset_ <- mapModel %>%
    recommend(dataset=dataset, target=target, now=10, rules=getRules()) %>%
    add(Observations(seq(0,100,by=0.1)))
  
  expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000)
  expect_equal(dataset_ %>% retrieveDoseAmount(2), 0)
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)
  
  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(xintercept=24, color="red", linetype="dotted") +
    ggplot2::geom_hline(yintercept=target@table$VALUE, color="red", linetype="dotted")
  
  conc <- results %>% filter(TIME==24) %>% pull(CONC)
  expect_equal(conc %>% round(), 19)
})


test_that(getTestName("Test multiple targets"), {
  model <- model_suite$pk$'2cpt_fo'
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>%  # Fixed loading dose
    add(Bolus(time=12, amount=2000)) %>% # Dose 1 to adapt
    add(Bolus(time=24, amount=2000)) %>% # Dose 2 to adapt
    add(Bolus(time=36, amount=2000)) %>% # Dose 3 to adapt
    add(Bolus(time=48, amount=2000)) %>% # Dose 4 to adapt
    add(Bolus(time=60, amount=2000))     # Dose 5 to adapt
  
  mapModel <- CampsismapModel(model=model, "CONC") %>%
    campsismap::setup(dest="mrgsolve")
  
  target <- TargetDefinitionPerWindow(tibble(TIME=c(0,30), VALUE=c(50,60)))
  
  dataset_ <- mapModel %>%
    recommend(dataset=dataset, target=target, now=10, rules=getRules()) %>%
    add(Observations(seq(0,100,by=0.1)))
  
  expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000) # Didn't changed because of 'now'
  expect_equal(dataset_ %>% retrieveDoseAmount(2), 3425)
  expect_equal(dataset_ %>% retrieveDoseAmount(3), 2645)
  expect_equal(dataset_ %>% retrieveDoseAmount(4), 3746)
  expect_equal(dataset_ %>% retrieveDoseAmount(5), 3175)
  expect_equal(dataset_ %>% retrieveDoseAmount(6), 3175)
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)
  
  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=TIME), data=tibble(TIME=c(12,24,36,48,60,72)), color="red", linetype="dotted") +
    ggplot2::geom_hline(mapping=ggplot2::aes(yintercept=VALUE), data=target@table, color="red", linetype="dotted")
})
