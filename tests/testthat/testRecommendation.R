testFolder <- "C:/prj/campsismap/tests/testthat/"

library(campsismap)

model <- CampsismapModel(model=model_suite$pk$'2cpt_fo', "CONC") %>%
  add(ProportionalErrorModel(0.25))

dataset <- Dataset() %>%
  add(Bolus(time=0, amount=1000)) %>%
  addSamples(tibble(TIME=20, DV=10))