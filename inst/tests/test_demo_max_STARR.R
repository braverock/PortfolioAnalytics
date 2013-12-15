
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source("demo/demo_max_STARR.R")

context("demo_max_STARR")

test_that("init.portf contains mean as an objective", 
          { expect_that(init.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("init.portf contains ES as an objective", 
          { expect_that(init.portf$objectives[[2]]$name == "ES", is_true()) })

test_that("init.portf contains ES as an objective with p=0.925", 
          { expect_that(init.portf$objectives[[2]]$arguments$p == 0.925, is_true()) })

##### maxSR.lo.ROI #####
context("maxSTARR.lo.ROI")

test_that("maxSTARR.lo.ROI objective measure mean = 0.006657183", 
          { expect_that(all.equal(extractObjectiveMeasures(maxSTARR.lo.ROI)$mean, 0.006657183), is_true()) })

test_that("maxSTARR.lo.ROI objective measure ES = 0.01394436", 
          { expect_that(all.equal(extractObjectiveMeasures(maxSTARR.lo.ROI)$ES, 0.01394436), is_true()) })

##### maxSTARR.lo.RP #####
context("maxSTARR.lo.RP")

test_that("maxSTARR.lo.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$mean), is_true()) })

test_that("maxSTARR.lo.RP objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$StdDev), is_true()) })

##### maxSTARR.lo.DE #####
context("maxSTARR.lo.DE")

test_that("maxSTARR.lo.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$mean), is_true()) })

test_that("maxSR.lo.DE objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$StdDev), is_true()) })
