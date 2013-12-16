
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_max_Sharpe.R", package="PortfolioAnalytics"))


context("demo_max_Sharpe")

test_that("init.portf contains mean as an objective", 
          { expect_that(init.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("init.portf contains StdDev as an objective", 
          { expect_that(init.portf$objectives[[2]]$name == "StdDev", is_true()) })

##### maxSR.lo.ROI #####
context("maxSR.lo.ROI")

test_that("maxSR.lo.ROI objective measure mean = 0.006062083", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$mean), 0.006062083, tolerance=1e-6) })

test_that("maxSR.lo.ROI objective measure StdDev = 0.008843188", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$StdDev), 0.008843188, tolerance=1e-6) })

##### maxSR.lo.RP #####
context("maxSR.lo.RP")

test_that("maxSR.lo.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$mean), is_true()) })

test_that("maxSR.lo.RP objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$StdDev), is_true()) })

##### maxSR.lo.DE #####
context("maxSR.lo.DE")

test_that("maxSR.lo.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$mean), is_true()) })

test_that("maxSR.lo.DE objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$StdDev), is_true()) })

