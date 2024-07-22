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
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=12, amount=2000))    # Dose to adapt

  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite

  recommendationLogic <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=10, rules=getRules())
  )
  test <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(recommendation %>% retrieveDoseAmount(2), 3425),
    recommendation
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))

  # Full plot with options
  options <- PlotDisplayOptions(timeref=Sys.time())
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
  
  # Bar plot value above and in black
  options@bar_plot_value_mode <- "above"
  options@ylim_bar_plot <- as.numeric(1)
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
  
  # Recommendation bar plot without options
  plot <- quickPlot(model=model, recommendation=recommendation)
  plot

  # Test 2: negative recommendation is not possible, 0 is returned
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=5))

  test <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(recommendation %>% retrieveDoseAmount(2), 0),
    recommendation
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))

  results <- simulate(model=model_suite$pk$'2cpt_fo' %>% disable("IIV"),
                      dataset=recommendation@recommended_dataset)
  conc <- results %>% filter(TIME==24) %>% pull(CONC)
  expect_equal(conc %>% round(), 19)
})


test_that(getTestName("Test basic recommendation (ETAs vs typical)"), {
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=12, amount=2000))    # Dose to adapt
  
  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  recommendationLogicA <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=10, rules=getRules())
  )
  recommendationLogicB <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=10, rules=getRules(), etas=c(ETA_KA=0, ETA_VC=0, ETA_VP=0, ETA_Q=0, ETA_CL=0.5))
  )
  
  testA <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(recommendation %>% retrieveDoseAmount(2), 3425),
    recommendation
  )
  
  testB <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(recommendation %>% retrieveDoseAmount(2), 7192),
    recommendation
  )
  
  recommendationA <- campsismapTest(recommendationLogicA, testA, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))
  recommendationB <- campsismapTest(recommendationLogicB, testB, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))

  quickPlot(model=model, recommendation=recommendationA)
  quickPlot(model=model, recommendation=recommendationB)
})

test_that(getTestName("Test basic recommendation (also adapt first dose"), {
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=12, amount=2000))    # Dose to adapt
  
  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  recommendationLogic <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=-10, rules=getRules())
  )
  test <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 5501),
    expect_equal(recommendation %>% retrieveDoseAmount(2), 2646),
    recommendation
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))
  
  options <- PlotDisplayOptions(timeref=Sys.time())
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
})

test_that(getTestName("No recommendation given if now is after the adaptable doses"), {
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=4000)) %>% # Fixed loading dose
    add(Bolus(time=12, amount=2000))    # Dose to adapt
  
  model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC")
  
  # Test 1: basic recommendation
  target <- TargetDefinitionPerWindow(tibble(TIME=0, VALUE=50)) # Target is 50 from 0 to infinite
  
  recommendationLogic <- expression(
    model %>%
      campsismap::setup(dest=destEngine) %>%
      recommend(dataset=dataset, target=target, now=15, rules=getRules())
  )
  test <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000),
    expect_equal(recommendation %>% retrieveDoseAmount(2), 2000),
    recommendation
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))
  
  options <- PlotDisplayOptions(timeref=Sys.time())
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
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
      recommend(dataset=dataset, target=target, now=now, rules=getRules())
  )
  
  test <- expression(
    expect_equal(recommendation %>% retrieveDoseAmount(1), 4000), # Didn't changed because of 'now'
    expect_equal(recommendation %>% retrieveDoseAmount(2), 3425),
    expect_equal(recommendation %>% retrieveDoseAmount(3), 2645),
    expect_equal(recommendation %>% retrieveDoseAmount(4), 3746),
    expect_equal(recommendation %>% retrieveDoseAmount(5), 3175),
    expect_equal(recommendation %>% retrieveDoseAmount(6), 3175),
    recommendation
  )
  
  recommendation <- campsismapTest(recommendationLogic, test, env=environment(), output_name="recommendation") %>%
    add(Observations(seq(0,100,by=0.1)))
  
  options <- PlotDisplayOptions(timeref=Sys.time())
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot

})

test_that(getTestName("Issues with geom_col if 1 bar"), {
  
  # # X axis problem if 1 bar
  # xx <- Sys.time()
  # plot <- ggplot2::ggplot(data=data.frame(TIME=c(xx, xx), value=c(1,2), AMT=c("A", "B")),
  #                         mapping=ggplot2::aes(x=TIME, y=value)) +
  #   ggplot2::geom_col(mapping=ggplot2::aes(fill=AMT), position="dodge2")
  # plot
  # 
  # # X axis problem if 1 bar
  # xx <- Sys.time()
  # plot <- ggplot2::ggplot() +
  #   ggplot2::geom_col(data=data.frame(TIME=c(xx, xx), value=c(1,2), AMT=c("A", "B")), mapping=ggplot2::aes(x=TIME, y=value, fill=AMT), position="dodge2")
  # plot
  # 
  # # No Xaxis problem if 1 bar and date
  # xx <- Sys.Date()
  # plot <- ggplot2::ggplot(data=data.frame(TIME=c(xx, xx), value=c(1,2), AMT=c("A", "B")),
  #                         mapping=ggplot2::aes(x=TIME, y=value)) +
  #   ggplot2::geom_col(mapping=ggplot2::aes(fill=AMT), position="dodge2")
  # plot
  # 
  # # No X axis problem if 2 bars
  # xx <- Sys.time()
  # yy <- Sys.time() + lubridate::dhours(10)
  # plot <- ggplot2::ggplot(data=data.frame(TIME=c(xx, xx, yy, yy), value=c(1,2,1,2), AMT=c("A", "B", "A", "B")),
  #                         mapping=ggplot2::aes(x=TIME, y=value)) +
  #   ggplot2::geom_col(mapping=ggplot2::aes(fill=AMT), position="dodge2")
  # plot
  # 
  # 
  # plot <- ggplot2::ggplot(data=data.frame(TIME=c(1, 1), value=c(1,2), AMT=c("A", "B")),
  #                         mapping=ggplot2::aes(x=TIME, y=value)) +
  #   ggplot2::geom_col(mapping=ggplot2::aes(fill=AMT), position="dodge")
  # plotToPOSIXct(plot, timeref=Sys.time())
  
})
