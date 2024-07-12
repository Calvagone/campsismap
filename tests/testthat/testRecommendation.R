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
  
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  mapModel <- CampsismapModel(model=model, "CONC") %>%
    campsismap::setup(dest="mrgsolve")
  
  dataset_ <- mapModel %>%
    recommend(dataset=dataset, target=target, now=10, rules=getRules()) %>%
    add(Observations(seq(0,100,by=0.1)))
  
  expect_equal(dataset_ %>% retrieveDoseAmount(2), 3425)
  
  results <- simulate(model=model %>% disable("IIV"), dataset=dataset_)
  
  spaghettiPlot(results, "CONC") +
    ggplot2::geom_vline(xintercept=24, color="red", linetype="dotted") +
    ggplot2::geom_hline(yintercept=target@table$VALUE, color="red", linetype="dotted")
  
})
