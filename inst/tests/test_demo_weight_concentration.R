
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source("demo/demo_weight_concentration.R")

context("weight concentration objective")

test_that("conc.portf contains weight_concentration as an objective", 
          { expect_that(inherits(conc.portf$objectives[[2]], "weight_concentration_objective"), is_true()) })

test_that("conc.portf contains weight_concentration as an objective", 
          { expect_that(conc.portf$objectives[[2]]$name == "HHI", is_true()) })

test_that("conc.portf contains weight_concentration as an objective with conc_aversion=0", 
          { expect_equal(conc.portf$objectives[[2]]$conc_aversion, rep(0, 4)) })

test_that("minimum variance and conc.portf weights are equal with conc_aversion=0", 
          { expect_equal(opt1$weights, opt2$weights) })

test_that("conc.portf with conc_aversion=1e6 results in an equal weight portfolio", 
          { expect_equal(as.numeric(opt4$weights), rep(1 / 8, 8)) })

