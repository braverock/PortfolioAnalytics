
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_risk_budgets.R", package="PortfolioAnalytics"))


context("Risk Budget Optimizations")

context("risk budget objective ES max_prisk")

test_that("rbES.portf contains risk_budget_objective", 
          { expect_that(inherits(rbES.portf$objectives[[2]], "risk_budget_objective"), is_true()) })

test_that("rbES.portf contains ES risk budget objective", 
          { expect_that(rbES.portf$objectives[[2]]$name == "ES", is_true()) })

test_that("rbES.portf max_prisk is 0.4", 
          { expect_equal(as.numeric(rbES.portf$objectives[[2]]$max_prisk), rep(0.4, 8)) })

test_that("rbES.portf min_concentration is false", 
          { expect_that(rbES.portf$objectives[[2]]$min_concentration, is_false()) })

test_that("rbES.portf min_difference is false", 
          { expect_that(rbES.portf$objectives[[2]]$min_difference, is_false()) })

test_that("rbES.DE optimal weights is a numeric vector", 
          { expect_that(is.numeric(extractWeights(rbES.DE)), is_true()) })

test_that("rbES.DE optimization does not violate max_prisk", 
          { expect_that(all(extractObjectiveMeasures(rbES.DE)$ES$pct_contrib_MES <= 0.4), is_true()) })

context("risk budget objective ES min_concentration")

test_that("eqES.portf contains risk_budget_objective", 
          { expect_that(inherits(eqES.portf$objectives[[2]], "risk_budget_objective"), is_true()) })

test_that("eqES.portf contains ES risk budget objective", 
          { expect_that(eqES.portf$objectives[[2]]$name == "ES", is_true()) })

test_that("eqES.portf min_concentration is false", 
          { expect_that(eqES.portf$objectives[[2]]$min_concentration, is_true()) })

test_that("eqES.portf min_difference is false", 
          { expect_that(eqES.portf$objectives[[2]]$min_difference, is_false()) })

test_that("eqES.RP optimal weights is a numeric vector", 
          { expect_that(is.numeric(extractWeights(eqES.RP)), is_true()) })

# This will be difficult to test for an exact value or limit
test_that("eqES.RP optimization pct_contrib_MES is a numeric vector", 
          { expect_that(is.numeric(extractObjectiveMeasures(eqES.RP)$ES$pct_contrib_MES), is_true()) })

context("risk budget objective StdDev max_prisk")

test_that("rbStdDev.portf contains risk_budget_objective", 
          { expect_that(inherits(rbStdDev.portf$objectives[[2]], "risk_budget_objective"), is_true()) })

test_that("rbStdDev.portf contains StdDev risk budget objective", 
          { expect_that(rbStdDev.portf$objectives[[2]]$name == "StdDev", is_true()) })

test_that("rbStdDev.portf max_prisk is 0.25", 
          { expect_equal(as.numeric(rbStdDev.portf$objectives[[2]]$max_prisk), rep(0.25, 8)) })

test_that("rbStdDev.portf min_concentration is false", 
          { expect_that(rbStdDev.portf$objectives[[2]]$min_concentration, is_false()) })

test_that("rbStdDev.portf min_difference is false", 
          { expect_that(rbStdDev.portf$objectives[[2]]$min_difference, is_false()) })

test_that("rbStdDev.DE optimal weights is a numeric vector", 
          { expect_that(is.numeric(extractWeights(rbStdDev.DE)), is_true()) })

test_that("rbStdDev.DE optimization does not violate max_prisk", 
          { expect_that(all(extractObjectiveMeasures(rbStdDev.DE)$ES$pct_contrib_MES <= 0.25), is_true()) })

