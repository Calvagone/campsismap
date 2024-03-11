
# setwd("C:/prj/campsismap/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsismap/tests/")
# testFolder <<- "C:/prj/campsismap/tests/testthat/"

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

campsismapTest <- function(simulation, test, env) {
  # Iteration over all test engines to be tested
  for (testEngine in testEngines) {
    env$destEngine <-  testEngine
    # Check if package exists (as test engines are suggested packages)
    # This is needed for CRAN when package is tested with `_R_CHECK_DEPENDS_ONLY_`=TRUE
    if (engineInstalled(testEngine)) {
      env$results <- eval(simulation, envir=env)
      eval(test, envir=env)
    }
  }
}

getTestName <- function(name) {
  return(paste0(name, " (", paste0(testEngines, collapse="/"), ")"))
}