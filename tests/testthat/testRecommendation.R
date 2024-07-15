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

  mapModel <- CampsismapModel(model=model, "CONC")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  recommendation <- expression(
    mapModel %>%
      setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=10, rules=getRules()) %>%
      add(Observations(seq(0,100,by=0.1)))
  )
  test <- expression(
    expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(dataset_ %>% retrieveDoseAmount(2), 3425),
    dataset_
  )
  
  dataset_ <- campsismapTest(recommendation, test, env=environment(), output_name="dataset_")

  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)
  
  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(xintercept=24, color="red", linetype="dotted") +
    ggplot2::geom_hline(yintercept=target@table$VALUE, color="red", linetype="dotted")
  
  # Test 2: negative recommendation is not possible, 0 is returned
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=5))

  test <- expression(
    expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(dataset_ %>% retrieveDoseAmount(2), 0),
    dataset_
  )
  
  dataset_ <- campsismapTest(recommendation, test, env=environment(), output_name="dataset_")

  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)

  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(xintercept=24, color="red", linetype="dotted") +
    ggplot2::geom_hline(yintercept=target@table$VALUE, color="red", linetype="dotted")

  conc <- results %>% filter(TIME==24) %>% pull(CONC)
  expect_equal(conc %>% round(), 19)
})


test_that(getTestName("Test multiple targets"), {
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>%  # Fixed loading dose
    add(Bolus(time=12, amount=2000)) %>% # Dose 1 to adapt
    add(Bolus(time=24, amount=2000)) %>% # Dose 2 to adapt
    add(Bolus(time=36, amount=2000)) %>% # Dose 3 to adapt
    add(Bolus(time=48, amount=2000)) %>% # Dose 4 to adapt
    add(Bolus(time=60, amount=2000))     # Dose 5 to adapt

  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC")

  target <- TargetDefinitionPerWindow(tibble(TIME=c(0,30), VALUE=c(50,60)))
  now <- 10

  recommendationLogic <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=now, rules=getRules()) %>%
      add(Observations(seq(0,100,by=0.1)))
  )
  test <- expression(
    expect_equal(dataset_ %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(dataset_ %>% retrieveDoseAmount(2), 3425),
    expect_equal(dataset_ %>% retrieveDoseAmount(3), 2645),
    expect_equal(dataset_ %>% retrieveDoseAmount(4), 3746),
    expect_equal(dataset_ %>% retrieveDoseAmount(5), 3175),
    expect_equal(dataset_ %>% retrieveDoseAmount(6), 3175),
    dataset_
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="dataset_")

  quickPlot(model=model, dataset=dataset, plot=RecommendationPlotType(), recommendation=recommendation, target=target, now=now)
})
