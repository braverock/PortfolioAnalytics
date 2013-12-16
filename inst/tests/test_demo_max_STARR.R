
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_max_STARR.R", package="PortfolioAnalytics"))


context("demo_max_STARR")

test_that("init.portf contains mean as an objective", 
          { expect_that(init.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("init.portf contains ES as an objective", 
          { expect_that(init.portf$objectives[[2]]$name == "ES", is_true()) })

test_that("init.portf contains ES as an objective with p=0.925", 
          { expect_equal(init.portf$objectives[[2]]$arguments$p, 0.925) })

##### maxSR.lo.ROI #####
context("maxSTARR.lo.ROI")

test_that("maxSTARR.lo.ROI objective measure mean = 0.006657183", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$mean), 0.006657183, tolerance=1e-6) })

test_that("maxSTARR.lo.ROI objective measure ES = 0.01394436", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$ES), 0.01394436, tolerance=1e-6) })

##### maxSTARR.lo.RP #####
context("maxSTARR.lo.RP")

test_that("maxSTARR.lo.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$mean), is_true()) })

test_that("maxSTARR.lo.RP objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$ES), is_true()) })

##### maxSTARR.lo.DE #####
context("maxSTARR.lo.DE")

test_that("maxSTARR.lo.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$mean), is_true()) })

test_that("maxSR.lo.DE objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$ES), is_true()) })
