##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_max_return.R", package="PortfolioAnalytics"))


context("demo_max_return")

###### ROI, full_investment, long only, max return ######
context("maxret.lo.ROI")

test_that("maxret.lo.ROI contains mean as an objective", 
          { expect_that(maxret.lo.ROI$portfolio$objectives[[1]]$name == "mean", is_true()) })

test_that("maxret.lo.ROI objective measure mean = 0.008246053", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxret.lo.ROI)$mean), 0.008246053, tolerance=1e-6) })

test_that("maxret.lo.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.lo.ROI) >= maxret.lo.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.lo.ROI) <= maxret.lo.ROI$portfolio$constraints[[2]]$max), is_true()) })


###### ROI, full_investment, box, max return ######
context("maxret.box.ROI")

test_that("maxret.box.ROI contains mean as an objective", 
          { expect_that(maxret.box.ROI$portfolio$objectives[[1]]$name == "mean", is_true()) })

test_that("maxret.box.ROI objective measure mean = 0.007508355", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxret.box.ROI)$mean), 0.007508355, tolerance=1e-6) })

test_that("maxret.box.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box.ROI) >= maxret.box.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box.ROI) <= maxret.box.ROI$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box with shorting, max return ######
context("maxret.box1.RP")

test_that("maxret.box1.RP contains StdDev as an objective", 
          { expect_that(maxret.box1.RP$portfolio$objectives[[2]]$name == "StdDev", is_true()) })

test_that("maxret.box1.RP contains mean as an objective", 
          { expect_that(maxret.box1.RP$portfolio$objectives[[1]]$name == "mean", is_true()) })

test_that("maxret.box1.RP objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$StdDev), is_true()) })

test_that("maxret.box1.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$mean), is_true()) })

test_that("maxret.box1.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box1.RP) >= maxret.box1.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box1.RP) <= maxret.box1.RP$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box, max return ######
context("maxret.box2.RP")

test_that("maxret.box2.RP contains StdDev as an objective", 
          { expect_that(maxret.box2.RP$portfolio$objectives[[2]]$name == "StdDev", is_true()) })

test_that("maxret.box2.RP contains mean as an objective", 
          { expect_that(maxret.box2.RP$portfolio$objectives[[1]]$name == "mean", is_true()) })

test_that("maxret.box2.RP objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$StdDev), is_true()) })

test_that("maxret.box2.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$mean), is_true()) })

test_that("maxret.box2.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box2.RP) >= maxret.box2.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.box2.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box2.RP) <= maxret.box2.RP$portfolio$constraints[[2]]$max), is_true()) })

###### DE, full_investment, box, max return ######
context("maxret.box.DE")

test_that("maxret.box.DE contains StdDev as an objective", 
          { expect_that(maxret.box.DE$portfolio$objectives[[2]]$name == "StdDev", is_true()) })

test_that("maxret.box.DE contains mean as an objective", 
          { expect_that(maxret.box.DE$portfolio$objectives[[1]]$name == "mean", is_true()) })

test_that("maxret.box.DE objective measure StdDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box.DE)$StdDev), is_true()) })

test_that("maxret.box.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(maxret.box.DE)$mean), is_true()) })

test_that("maxret.box.DE min box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box.DE) >= maxret.box.DE$portfolio$constraints[[2]]$min), is_true()) })

test_that("maxret.box.DE max box constraints are not violated", 
          { expect_that(all(extractWeights(maxret.box.DE) <= maxret.box.DE$portfolio$constraints[[2]]$max), is_true()) })
