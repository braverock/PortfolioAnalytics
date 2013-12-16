
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_leverage_exposure_constraint.R", package="PortfolioAnalytics"))


context("dollar neutral portfolio")

test_that("dollar.neutral.portf min_sum constraint is -0.01", 
          { expect_equal(dollar.neutral.portf$constraints[[1]]$min_sum, -0.01) })

test_that("dollar.neutral.portf max_sum constraint is 0.01", 
          { expect_equal(dollar.neutral.portf$constraints[[1]]$max_sum, 0.01) })

test_that("dollar.neutral.portf leverage exposure constraint is 2", 
          { expect_equal(dollar.neutral.portf$constraints[[3]]$leverage, 2) })

test_that("dollar.neutral.portf weights is a numeric vector", 
          { expect_that(is.numeric(extractWeights(dollar.neutral.opt)), is_true()) })

test_that("dollar.neutral.portf leverage exposure constraint is not violated", 
          { expect_that(sum(abs(extractWeights(dollar.neutral.opt))) <= 2, is_true()) })

test_that("dollar.neutral.portf objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(dollar.neutral.opt)$mean), is_true()) })

test_that("dollar.neutral.portf objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(dollar.neutral.opt)$ES), is_true()) })


context("leveraged portfolio")

test_that("leveraged.portf min_sum constraint is 0.99", 
          { expect_equal(leveraged.portf$constraints[[1]]$min_sum, 0.99) })

test_that("leveraged.portf max_sum constraint is 1.01", 
          { expect_equal(leveraged.portf$constraints[[1]]$max_sum, 1.01) })

test_that("leveraged.portf leverage exposure constraint is 1.6", 
          { expect_equal(leveraged.portf$constraints[[3]]$leverage, 1.6) })

test_that("leveraged.opt weights is a numeric vector", 
          { expect_that(is.numeric(extractWeights(leveraged.opt)), is_true()) })

test_that("leveraged.opt leverage exposure constraint is not violated", 
          { expect_that(sum(abs(extractWeights(leveraged.opt))) <= 1.6, is_true()) })

test_that("leveraged.opt objective measure mean is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(leveraged.opt)$mean), is_true()) })

test_that("leveraged.opt objective measure ES is numeric", 
          { expect_that(is.numeric(extractObjectiveMeasures(leveraged.opt)$ES), is_true()) })
