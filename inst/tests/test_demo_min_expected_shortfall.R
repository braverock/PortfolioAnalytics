
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source("demo/demo_min_expected_shortfall.R")

context("demo_min_expected_shortfall")

###### ROI, full_investment, long only, min ES ######
context("minES.lo.ROI")

test_that("minES.lo.ROI contains ES as an objective", 
          { expect_that(minES.lo.ROI$portfolio$objectives[[1]]$name == "ES", is_true()) })

test_that("minES.lo.ROI ES objective p=0.9", 
          { expect_that(minES.lo.ROI$portfolio$objectives[[1]]$arguments$p == 0.9, is_true()) })

test_that("minES.lo.ROI objective measure ES = 0.01013571", 
          { expect_that(all.equal(extractObjectiveMeasures(minES.lo.ROI)$ES, 0.01013571), is_true()) })

test_that("minES.lo.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(minES.lo.ROI) >= minES.lo.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.lo.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(minES.lo.ROI) <= minES.lo.ROI$portfolio$constraints[[2]]$max), is_true()) })

###### ROI, full_investment, long only, min ES ######
context("minES.box.ROI")

test_that("minES.box.ROI contains ES as an objective", 
          { expect_that(minES.box.ROI$portfolio$objectives[[1]]$name == "ES", is_true()) })

test_that("minES.box.ROI ES objective p=0.9", 
          { expect_that(minES.box.ROI$portfolio$objectives[[1]]$arguments$p == 0.9, is_true()) })

test_that("minES.box.ROI objective measure ES = 0.01477709", 
          { expect_that(all.equal(extractObjectiveMeasures(minES.box.ROI)$ES, 0.01477709), is_true()) })

test_that("minES.box.ROI min box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box.ROI) >= minES.box.ROI$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box.ROI max box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box.ROI) <= minES.box.ROI$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box, min ES ######
context("minES.box1.RP")

test_that("minES.box1.RP contains ES as an objective", 
          { expect_that(minES.box1.RP$portfolio$objectives[[1]]$name == "ES", is_true()) })

test_that("minES.box1.RP ES objective p=0.9", 
          { expect_that(minES.box1.RP$portfolio$objectives[[1]]$arguments$p == 0.9, is_true()) })

test_that("minES.box1.RP contains mean as an objective", 
          { expect_that(minES.box1.RP$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minES.box1.RP objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box1.RP)$ES), is_true()) })

test_that("minES.box1.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box1.RP)$mean), is_true()) })

test_that("minES.box1.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box1.RP) >= minES.box1.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box1.RP) <= minES.box1.RP$portfolio$constraints[[2]]$max), is_true()) })

###### RP, full_investment, box, min ES ######
context("minES.box2.RP")

test_that("minES.box2.RP contains ES as an objective", 
          { expect_that(minES.box2.RP$portfolio$objectives[[1]]$name == "ES", is_true()) })

test_that("minES.box2.RP ES objective p=0.9", 
          { expect_that(minES.box2.RP$portfolio$objectives[[1]]$arguments$p == 0.9, is_true()) })

test_that("minES.box2.RP contains mean as an objective", 
          { expect_that(minES.box2.RP$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minES.box2.RP objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box2.RP)$ES), is_true()) })

test_that("minES.box2.RP objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box2.RP)$mean), is_true()) })

test_that("minES.box2.RP min box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box2.RP) >= minES.box2.RP$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box2.RP max box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box2.RP) <= minES.box2.RP$portfolio$constraints[[2]]$max), is_true()) })

###### DE, full_investment, box, min ES ######
context("minES.box1.DE")

test_that("minES.box.DE contains ES as an objective", 
          { expect_that(minES.box.DE$portfolio$objectives[[1]]$name == "ES", is_true()) })

test_that("minES.box.DE ES objective p=0.9", 
          { expect_that(minES.box.DE$portfolio$objectives[[1]]$arguments$p == 0.9, is_true()) })

test_that("minES.box2.DE contains mean as an objective", 
          { expect_that(minES.box.DE$portfolio$objectives[[2]]$name == "mean", is_true()) })

test_that("minES.box.DE objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box.DE)$ES), is_true()) })

test_that("minES.box.DE objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(minES.box.DE)$mean), is_true()) })

test_that("minES.box.DE min box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box.DE) >= minES.box.DE$portfolio$constraints[[2]]$min), is_true()) })

test_that("minES.box.DE max box constraints are not violated", 
          { expect_that(all(extractWeights(minES.box.DE) <= minES.box.DE$portfolio$constraints[[2]]$max), is_true()) })


