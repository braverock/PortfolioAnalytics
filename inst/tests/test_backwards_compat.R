
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source("demo/backwards_compat.R")

context("Backwards compatibility is maintained")

# class
test_that("Class of gen.constr is v1_constraint", 
          { expect_that(inherits(gen.constr, "v1_constraint", is_true()) })

# assets
test_that("Initial assets form an equal weight portfolio", 
          { expect_that(all.equal(as.numeric(gen.constr$assets), rep(1/4, 4)), is_true()) })

# min
test_that("Box constraints min vector is all 0s", 
          { expect_that(all.equal(as.numeric(gen.constr$min), rep(0, 4)), is_true()) })

# max
test_that("Box constraints max vector is all 1s", 
          { expect_that(all.equal(as.numeric(gen.constr$max), rep(1, 4)), is_true()) })

# min_mult
test_that("min_mult is null", 
          { expect_that(is.null(gen.constr$min_mult), is_true()) })

# max_mult
test_that("max_mult is null", 
          { expect_that(is.null(gen.constr$max_mult), is_true()) })

# min_sum
test_that("min_sum is 0.99", 
          { expect_that(all.equal(gen.constr$min_sum, 0.99), is_true()) })

# max_sum
test_that("min_sum is 1.01", 
          { expect_that(all.equal(gen.constr$max_sum, 1.01), is_true()) })

# mean objective
test_that("The objective name is 'mean'", 
          { expect_that(all.equal(gen.constr$objectives[[1]]$name, "mean"), is_true()) })

context("Optimization output")

# Not sure how to test for exact values of optimization results for DEoptim
# and random portfolios
# - use a specific data set of rp weights

# random portfolios optimization
test_that("random portfolios updated portfolio object", 
          { expect_that(inherits(optrpv1$portfolio, "portfolio.spec"), is_true()) })

test_that("random portfolios returns optimal weights", 
          { expect_that(is.numeric(extractWeights(optrpv1)), is_true()) })

test_that("random portfolios returns an objective measure", 
          { expect_that(is.numeric(extractObjectiveMeasures(optrpv1)$mean), is_true()) })

# DEoptim optimization
test_that("DE optim updated portfolio object", 
          { expect_that(inherits(optrdev1$portfolio, "portfolio.spec"), is_true()) })

test_that("DE optim returns optimal weights", 
          { expect_that(is.numeric(extractWeights(optdev1)), is_true()) })

test_that("DE optim returns an objective measure", 
          { expect_that(is.numeric(extractObjectiveMeasures(optdev1)$mean), is_true()) })

# ROI optimization
test_that("ROI updated portfolio object", 
          { expect_that(inherits(optroiv1$portfolio, "portfolio.spec"), is_true()) })

test_that("ROI returns optimal weights equal to c(0, 0, 0.46, 0.55)", 
          { expect_equal(as.numeric(extractWeights(optroiv1)), c(0, 0, 0.46, 0.55)) })

test_that("ROI returns an objective measure mean=0.008193842", 
          { expect_equal(is.numeric(extractObjectiveMeasures(optroiv1)$mean), 0.008193842) })

