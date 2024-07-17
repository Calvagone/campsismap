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

  # Plot test 1
  
  options <- PlotDisplayOptions(timeref=Sys.time(), show_legend=FALSE, legend_title="")
  
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
  
  # Recommendation bar plot with options
  options@date_limits <- c(minValueInPlot(plot, "TIME"), maxValueInPlot(plot, "TIME"))
  plot <- quickPlot(model=model, recommendation=recommendation, plot=RecommendationBarPlotType(), options=options)
  plot
  
  # Recommendation bar plot without options
  plot <- quickPlot(model=model, recommendation=recommendation, plot=RecommendationBarPlotType())
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

  quickPlot(model=model, recommendation=recommendation)
  
  results <- simulate(model=model_suite$pk$'2cpt_fo' %>% disable("IIV"),
                      dataset=recommendation@recommended_dataset)
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
  
  options <- PlotDisplayOptions(timeref=Sys.time(), show_legend=FALSE, legend_title="")
  
  plot <- quickPlot(model=model, recommendation=recommendation, options=options)
  plot
  
  options@date_limits <- c(minValueInPlot(plot, "TIME"),  maxValueInPlot(plot, "TIME"))
  plot <- quickPlot(model=model, recommendation=recommendation, plot=RecommendationBarPlotType(), options=options, position_dodge_width=3600*12*0.9)
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
