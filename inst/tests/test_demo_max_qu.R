##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_max_quadratic_utility.R", package="PortfolioAnalytics"))


##### init.portf objectives #####
context("objectives for quadratic utility")

test_that("init.portf contains mean as an objective", 
          { expect_that(init.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("init.portf contains StdDev as an objective", 
          { expect_that(init.portf$objectives[[2]]$name == "StdDev", is_true()) })

##### ROI, full_investment, long only, max qu ######
context("maxQU.lo.ROI")

test_that("risk aversion parameter = 4", 
          { expect_equal(maxQU.lo.ROI$portfolio$objectives[[2]]$risk_aversion, 4) })

test_that("maxQU.lo.ROI objective measure mean = 0.007813251", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$mean), 0.007813251, tolerance=1e-6) })

test_that("maxQU.lo.ROI objective measure StdDev = 0.01556929", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$StdDev), 0.01556929, tolerance=1e-6) })

test_that("maxQU.lo.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.lo.ROI) >= maxQU.lo.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.lo.ROI) <= maxQU.lo.ROI$portfolio$constraints[[2]]$max), is_true()) })

##### ROI, full_investment, long only, max qu to approximate max return ######
context("maxQU.maxret.ROI")

test_that("risk aversion parameter = 1e-6", 
          { expect_equal(maxQU.maxret.ROI$portfolio$objectives[[2]]$risk_aversion, 1e-6) })

test_that("maxQU.maxret.ROI objective measure mean = 0.008246053", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.maxret.ROI)$mean), 0.008246053, tolerance=1e-6) })

test_that("maxQU.maxret.ROI objective measure StdDev = 0.03857144", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.maxret.ROI)$StdDev), 0.03857144, tolerance=1e-6) })

test_that("maxQU.maxret.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.maxret.ROI) >= maxQU.maxret.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxQU.maxret.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.maxret.ROI) <= maxQU.maxret.ROI$portfolio$constraints[[2]]$max), is_true()) })

##### ROI, full_investment, long only, max qu to approximate min StdDev ######
context("maxQU.minvol.ROI")

test_that("risk aversion parameter = 1e6", 
          { expect_equal(maxQU.minvol.ROI$portfolio$objectives[[2]]$risk_aversion, 1e6) })

test_that("maxQU.minvol.ROI objective measure mean = 0.00603498", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$mean), 0.00603498, tolerance=1e-6) })

test_that("maxQU.minvol.ROI objective measure StdDev = 0.008251084", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$StdDev), 0.008251084, tolerance=1e-6) })

test_that("maxQU.minvol.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.minvol.ROI) >= maxQU.minvol.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxQU.minvol.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxQU.minvol.ROI) <= maxQU.minvol.ROI$portfolio$constraints[[2]]$max), is_true()) })
