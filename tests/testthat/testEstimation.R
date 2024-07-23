library(testthat)
library(tibble)
library(tictoc)

context("Test the estimation of individual parameters")
source(paste0("", "testUtils.R"))

relativeTolerance <- function() {
  return(1e-2)
}

absoluteRelativeError <- function(actual, expected) {
  return(abs((expected - actual)/actual))  
}

test_that(getTestName("Method estimate works as expected when 1 sample is provided"), {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=20, DV=10))
  
  estimation <- expression(model %>% setup(dest=destEngine) %>% estimate(dataset=dataset))
  test <- expression(
    expect_equal(as.numeric(results$par) %>% round(3), c(-0.022, -0.140, -0.392)),
    quickPlot(model, dataset, etas=results$par)
  )
  
  campsismapTest(estimation, test, env=environment())
})

test_that(getTestName("Method estimate works as expected when 2 samples are provided"), {
  
  model <- CampsismapModel(model=model_suite$testing$pk$'1cpt_fo', "CONC") %>%
    add(ProportionalErrorModel(0.1))
  
  dataset <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=c(20,30), DV=c(10,14)))
  
  estimation <- expression(model %>% setup(dest=destEngine) %>% estimate(dataset=dataset))
  test <- expression(
    expect_true(all(absoluteRelativeError(results$par, c(-0.02756761, -0.18649402, -1.01813202)) < relativeTolerance())),
    quickPlot(model, dataset, etas=results$par)
  )
  
  campsismapTest(estimation, test, env=environment())
})

test_that("Run the 2 previous tests with mapbayr", {
  
  code <- "
[PARAM] @annotated
THETA_KA : 1 : THETA_KA
THETA_VC : 60 : THETA_VC
THETA_CL : 3 : THETA_CL
THETA_PROP_RUV : 0.1 : THETA_PROP_RUV
ETA1 : 0 : ETA1
ETA2 : 0 : ETA2
ETA3 : 0 : ETA3

[CMT] @annotated
A_ABS : ABS
A_CENTRAL : CENTRAL

[OMEGA] 0.0606246218164348 0.0606246218164348 0.0606246218164348

[SIGMA] 
0.01 // Proportional error 
0    // Additive error

[MAIN]
double TVKA=THETA_KA;
double TVVC=THETA_VC;
double TVCL=THETA_CL;

double KA=TVKA * exp(ETA1 + ETA(1));
double VC=TVVC * exp(ETA2 + ETA(2));
double CL=TVCL * exp(ETA3 + ETA(3));

[ODE]
dxdt_A_ABS=-KA*A_ABS;
dxdt_A_CENTRAL=KA*A_ABS - CL/VC*A_CENTRAL;

$TABLE
double DV = (A_CENTRAL/VC) * (1 + EPS(1));

$CAPTURE DV CL
"

  model <- mrgsolve::mcode("1cpt_fo_mrgsolve", code)
  
  dataset1 <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=20, DV=10))
  
  results1 <- mapbayr::mapbayest(model, data=dataset1 %>% export(dest="mrgsolve"))
  expect_equal(getMapbayrEstimates(results1) %>% round(3), c(-0.022, -0.140, -0.392))
  
  dataset2 <- Dataset() %>%
    add(Bolus(time=0, amount=1000)) %>%
    addSamples(tibble(TIME=c(20,30), DV=c(10,14)))
  
  results2 <- mapbayr::mapbayest(model, data=dataset2 %>% export(dest="mrgsolve"))
  expect_equal(getMapbayrEstimates(results2) %>% round(3), c(-0.028, -0.186, -1.018))
})
