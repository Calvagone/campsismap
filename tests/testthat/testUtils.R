
# setwd("C:/prj/campsismap/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismap/tests/")
# testFolder <- "C:/prj/campsismap/tests/testthat/"

testFolder <- ""
overwriteNonRegressionFiles <- FALSE
testEngines <- c("rxode2", "mrgsolve")

engineInstalled <- function(name) {
  return(find.package(name, quiet=TRUE) %>% length() > 0)
}

noEngineInstalled <- function() {
  cond1 <- engineInstalled("rxode2")
  cond2 <- engineInstalled("mrgsolve")
  return(!(cond1 || cond2))
}

campsismapTest <- function(x, test, env, output_name="results") {
  # Iteration over all test engines to be tested
  for (testEngine in testEngines) {
    env$destEngine <-  testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engineInstalled(testEngine)) {
      env[[output_name]] <- eval(x, envir=env)
      retValue <- eval(test, envir=env)
    }
  }
  # Return last evaluation such that other tests can be performed on output
  return(retValue)
}

getTestName <- function(name) {
  return(paste0(name, " (", paste0(testEngines, collapse="/"), ")"))
}

getMapbayrEstimates <- function(results) {
  opt.value <- results$opt.value
  if (nrow(opt.value) > 1) {
    stop("1 individual/row expected")
  }
  etaNames <- names(results$arg.optim$par)
  return(opt.value %>% dplyr::select(dplyr::all_of(etaNames)) %>% as.numeric())
}
