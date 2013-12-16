##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_min_StdDev.R", package="PortfolioAnalytics"))


context("demo_min_StdDev")

###### ROI, full_investment, long only, min StdDev ######
context("minStdDev.lo.ROI")

test_that("minStdDev.lo.ROI contains StdDev as an objective", 
          { expect_that(minStdDev.lo.ROI$portfolio$objectives[[1]]$name == "StdDev", is_true()) })

test_that("minStdDev.lo.ROI objective measure StdDev = 0.008251084", 
          { expect_equal(as.numeric(extractObjectiveMeasures(minStdDev.lo.ROI)$StdDev), 0.008251084, tolerance=1e-6) })

test_that("minStdDev.lo.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.lo.ROI) >= minStdDev.lo.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("minStdDev.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.lo.ROI) <= minStdDev.lo.ROI$portfolio$constraints[[2]]$max), is_true()) })

###### ROI, full_investment, box, min StdDev ######
context("minStdDev.box.ROI")

test_that("minStdDev.box.ROI contains StdDev as an objective", 
          { expect_that(minStdDev.box.ROI$portfolio$objectives[[1]]$name == "StdDev", is_true()) })

test_that("minStdDev.box.ROI objective measure StdDev = 0.01096122", 
          { expect_equal(as.numeric(extractObjectiveMeasures(minStdDev.box.ROI)$StdDev), 0.01096122, tolerance=1e-6) })

test_that("minStdDev.box.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box.ROI) >= minStdDev.box.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("minStdDev.box.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box.ROI) <= minStdDev.box.ROI$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box, min ES ######
context("minStdDev.box1.RP")

test_that("minStdDev.box1.RP contains StdDev as an objective", 
          { expect_that(minStdDev.box1.RP$portfolio$objectives[[1]]$name == "StdDev", is_true()) })

test_that("minStdDev.box1.RP contains mean as an objective", 
          { expect_that(minStdDev.box1.RP$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minStdDev.box1.RP objective measure StDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box1.RP)$StdDev), is_true()) })

test_that("minStdDev.box1.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box1.RP)$mean), is_true()) })

test_that("minStdDev.box1.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box1.RP) >= minStdDev.box1.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box1.RP) <= minStdDev.box1.RP$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box, min StdDev ######
context("minStdDev.box2.RP")

test_that("minStdDev.box2.RP contains StdDev as an objective", 
          { expect_that(minStdDev.box2.RP$portfolio$objectives[[1]]$name == "StdDev", is_true()) })

test_that("minStdDev.box2.RP contains mean as an objective", 
          { expect_that(minStdDev.box2.RP$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minStdDev.box2.RP objective measure StDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box2.RP)$StdDev), is_true()) })

test_that("minStdDev.box2.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box2.RP)$mean), is_true()) })

test_that("minStdDev.box2.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box2.RP) >= minStdDev.box2.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box2.RP) <= minStdDev.box2.RP$portfolio$constraints[[2]]$max), is_true()) })

###### DE, full_investment, box, min StdDev ######
context("minStdDev.box.DE")

test_that("minStdDev.box.DE contains StdDev as an objective", 
          { expect_that(minStdDev.box.DE$portfolio$objectives[[1]]$name == "StdDev", is_true()) })

test_that("minStdDev.box.DE contains mean as an objective", 
          { expect_that(minStdDev.box.DE$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minStdDev.box.DE objective measure StDev is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box.DE)$StdDev), is_true()) })

test_that("minStdDev.box.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minStdDev.box.DE)$mean), is_true()) })

test_that("minStdDev.box.DE min box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box.DE) >= minStdDev.box.DE$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(minStdDev.box.DE) <= minStdDev.box.DE$portfolio$constraints[[2]]$max), is_true()) })

