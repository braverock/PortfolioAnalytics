##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source("demo/demo_max_quadratic_utility.R")

context("demo_max_quadratic_utility")

##### init.portf objectives #####
context("objectives for quadratic utility")

test_that("init.portf contains mean as an objective", 
          { expect_that(init.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("init.portf contains StdDev as an objective", 
          { expect_that(init.portf$objectives[[2]]$name == "StdDev", is_true()) })

test_that("init.portf contains risk_aversion parameter equal to 4", 
          { expect_that(init.portf$objectives[[2]]$risk_aversion == 4, is_true()) })

##### ROI, full_investment, long only, max qu ######
context("maxQU.lo.ROI")

test_that("maxQU.lo.ROI objective measure mean = 0.007813251", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.lo.ROI)$mean, 0.007813251), is_true()) })

test_that("maxQU.lo.ROI objective measure StdDev = 0.01556929", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.lo.ROI)$StdDev, 0.01556929), is_true()) })

test_that("maxQU.lo.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.lo.ROI) >= maxQU.lo.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.lo.ROI) <= maxQU.lo.ROI$portfolio$constraints[[2]]$max), is_true()) })

##### ROI, full_investment, long only, max qu to approximate max return ######
context("maxQU.maxret.ROI")

test_that("risk aversion parameter = 1e-6", 
          { expect_that(all.equal(init.portf$objectives[[2]]$risk_aversion, 1e-6), is_true()) })

test_that("maxQU.maxret.ROI objective measure mean = 0.008246053", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.maxret.ROI)$mean, 0.008246053), is_true()) })

test_that("maxQU.maxret.ROI objective measure StdDev = 0.03857144", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.maxret.ROI)$StdDev, 0.03857144), is_true()) })

test_that("maxQU.maxret.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.maxret.ROI) >= maxQU.maxret.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxQU.maxret.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.maxret.ROI) <= maxQU.maxret.ROI$portfolio$constraints[[2]]$max), is_true()) })

##### ROI, full_investment, long only, max qu to approximate min StdDev ######
context("maxQU.minvol.ROI")

test_that("risk aversion parameter = 1e6", 
          { expect_that(all.equal(init.portf$objectives[[2]]$risk_aversion, 1e6), is_true()) })

test_that("maxQU.minvol.ROI objective measure mean = 0.00603498", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.minvol.ROI)$mean, 0.00603498), is_true()) })

test_that("maxQU.minvol.ROI objective measure StdDev = 0.008251084", 
          { expect_that(all.equal(extractObjectiveMeasures(maxQU.minvol.ROI)$StdDev, 0.008251084), is_true()) })

test_that("maxQU.minvol.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.minvol.ROI) >= maxQU.minvol.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxQU.minvol.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.minvol.ROI) <= maxQU.minvol.ROI$portfolio$constraints[[2]]$max), is_true()) })
