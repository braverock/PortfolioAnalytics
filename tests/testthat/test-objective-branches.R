###############################################################################
# tests/testthat/test-objective-branches.R
#
# Source file: R/objective.R
#
# Targeted branches not yet covered by test-objective-constructors.R
# or test-objectives.R:
#
#   objective():
#     - is.null(name) stop (line 30)
#     - !is.list(arguments) stop (line 32)
#
#   add.objective_v1():
#     - type="return"    → return_objective        (v1 API path)
#     - type="risk"      → portfolio_risk_objective (v1 API path)
#     - type="risk_budget" → risk_budget_objective  (v1 API path)
#     - type="turnover"  → turnover_objective       (v1 API path)
#     - type="tmp_minmax" → minmax_objective        (v1 API path)
#     - type="weight_concentration" (v1 API path)
#     - type="null"      → early return             (v1 API path)
#     - indexnum update path (non-null indexnum)
#
#   add.objective() / add.objective_v2():
#     - constraints != NULL & inherits v1_constraint → dispatches to v1
#     - indexnum update path (replace existing objective at given index)
#     - type="null" → early return
#     - type="qu"  → quadratic_utility early return
#
#   risk_budget_objective():
#     - length(min_prisk) != length(max_prisk) stop
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------
utils::data(edhec)
assets5 <- colnames(edhec[, 1:5])


# ===========================================================================
# 1. objective() error stops
# ===========================================================================

# objective() is qualified in this block. ROI exports a function of the same
# name, 27 of the test files attach ROI, and library() does not move an
# already-attached package back up the search path. In a full-suite run
# objective() therefore resolves to ROI's, which takes a single argument x, and
# these tests exercise that instead of the one they are written for. They pass
# when this file is run on its own and fail when the suite is run, which is what
# they did before this change: an error on the valid call, and the wrong error
# message on the non-list call.
#
# The expected message is pinned for the same reason. "name" alone also appears
# in R's "unused arguments (name = NULL, ...)", so the first test passed under
# masking without testing anything.

test_that("objective(): NULL name triggers stop", {
  expect_error(
    PortfolioAnalytics::objective(name = NULL, arguments = list()),
    regexp = "you must specify an objective name"
  )
})

test_that("objective(): non-list arguments triggers stop", {
  expect_error(
    PortfolioAnalytics::objective(name = "mean", arguments = "not_a_list"),
    regexp = "arguments must be passed as a named list"
  )
})

test_that("objective(): valid call returns correct class", {
  obj <- PortfolioAnalytics::objective(name = "mean", arguments = list())
  expect_s3_class(obj, "objective")
  expect_equal(obj$name, "mean")
})


# ===========================================================================
# 2. add.objective_v1() — all type branches
# ===========================================================================

# Helper: build a base v1_constraint for 4 assets
make_v1 <- function() {
  c <- constraint_v1(assets = 4, min = 0, max = 1,
                     min_sum = 0.99, max_sum = 1.01)
  c
}

test_that("add.objective_v1(): type='return' creates return_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "return", name = "mean")
  expect_s3_class(cv1$objectives[[1]], "return_objective")
})

test_that("add.objective_v1(): type='risk' creates portfolio_risk_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "risk", name = "StdDev")
  expect_s3_class(cv1$objectives[[1]], "portfolio_risk_objective")
})

test_that("add.objective_v1(): type='return_objective' alias works", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "return_objective", name = "mean")
  expect_s3_class(cv1$objectives[[1]], "return_objective")
})

test_that("add.objective_v1(): type='portfolio_risk_objective' alias works", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "portfolio_risk_objective", name = "StdDev")
  expect_s3_class(cv1$objectives[[1]], "portfolio_risk_objective")
})

test_that("add.objective_v1(): type='risk_budget' creates risk_budget_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "risk_budget", name = "ES",
                           max_prisk = 0.4)
  expect_s3_class(cv1$objectives[[1]], "risk_budget_objective")
})

test_that("add.objective_v1(): type='risk_budget_objective' alias works", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "risk_budget_objective", name = "ES",
                           max_prisk = 0.4)
  expect_s3_class(cv1$objectives[[1]], "risk_budget_objective")
})

test_that("add.objective_v1(): type='turnover' creates turnover_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "turnover", name = "turnover")
  expect_s3_class(cv1$objectives[[1]], "turnover_objective")
})

test_that("add.objective_v1(): type='tmp_minmax' creates minmax_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "tmp_minmax", name = "mean",
                           min = 0.001, max = 0.05)
  expect_s3_class(cv1$objectives[[1]], "minmax_objective")
})

test_that("add.objective_v1(): type='weight_concentration' creates weight_concentration_objective", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "weight_concentration", name = "HHI",
                           conc_aversion = 0.01)
  expect_s3_class(cv1$objectives[[1]], "weight_concentration_objective")
})

test_that("add.objective_v1(): type='weight_conc' alias works", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "weight_conc", name = "HHI",
                           conc_aversion = 0.01)
  expect_s3_class(cv1$objectives[[1]], "weight_concentration_objective")
})

test_that("add.objective_v1(): type='null' returns constraints unchanged", {
  cv1 <- make_v1()
  result <- add.objective_v1(cv1, type = "null", name = "foo")
  expect_length(result$objectives, 0L)
})

test_that("add.objective_v1(): errors on non-constraint input", {
  expect_error(add.objective_v1(list(), type = "risk", name = "StdDev"),
               regexp = "constraint")
})

test_that("add.objective_v1(): indexnum update replaces objective at given index", {
  cv1 <- make_v1()
  cv1 <- add.objective_v1(cv1, type = "risk",   name = "StdDev")
  cv1 <- add.objective_v1(cv1, type = "return", name = "mean")
  # Replace the first objective (StdDev) with VaR
  cv1 <- add.objective_v1(cv1, type = "risk", name = "VaR", indexnum = 1)
  expect_equal(cv1$objectives[[1]]$name, "VaR")
  expect_equal(cv1$objectives[[2]]$name, "mean") # second unchanged
  expect_length(cv1$objectives, 2L)              # no new entry added
})


# ===========================================================================
# 3. add.objective() v2 — dispatch to v1, indexnum update, "null" and "qu"
# ===========================================================================

test_that("add.objective(): passes v1_constraint to add.objective_v1()", {
  cv1 <- make_v1()
  result <- add.objective(portfolio = NULL, constraints = cv1,
                          type = "risk", name = "StdDev")
  expect_s3_class(result$objectives[[1]], "portfolio_risk_objective")
})

test_that("add.objective(): indexnum update replaces objective at given index", {
  p <- portfolio.spec(assets = assets5)
  p <- add.objective(p, type = "risk",   name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean")
  # Replace first objective (StdDev) with ES
  p <- add.objective(p, type = "risk", name = "ES", indexnum = 1L)
  expect_equal(p$objectives[[1]]$name, "ES")
  expect_equal(p$objectives[[2]]$name, "mean")
  expect_length(p$objectives, 2L)
})

test_that("add.objective(): type='null' returns portfolio unchanged", {
  p <- portfolio.spec(assets = assets5)
  p <- add.objective(p, type = "risk", name = "StdDev")
  result <- add.objective(p, type = "null", name = "foo")
  # objectives list should be unchanged (length stays at 1)
  expect_length(result$objectives, 1L)
})

test_that("add.objective(): type='qu' adds two objectives to portfolio", {
  p <- portfolio.spec(assets = assets5)
  p <- add.objective(p, type = "quadratic_utility", risk_aversion = 2)
  expect_length(p$objectives, 2L)
  expect_s3_class(p$objectives[[1]], "return_objective")
  expect_s3_class(p$objectives[[2]], "portfolio_risk_objective")
})

test_that("add.objective(): type='qu' alias works", {
  p <- portfolio.spec(assets = assets5)
  # "qu" is not "quadratic_utility" so the name check fires; pass a dummy name
  p <- add.objective(p, type = "qu", name = "qu", risk_aversion = 1)
  expect_length(p$objectives, 2L)
})

test_that("add.objective(): errors on non-portfolio input", {
  expect_error(add.objective(portfolio = list(), type = "risk", name = "StdDev"),
               regexp = "portfolio")
})


# ===========================================================================
# 4. risk_budget_objective() — length mismatch stop
# ===========================================================================

test_that("risk_budget_objective(): length(min_prisk) != length(max_prisk) triggers stop", {
  expect_error(
    risk_budget_objective(
      assets = setNames(rep(0.2, 5), assets5),
      name   = "ES",
      min_prisk = c(0.1, 0.2),   # length 2
      max_prisk = c(0.4, 0.5, 0.6) # length 3
    ),
    regexp = "length"
  )
})

test_that("risk_budget_objective(): scalar min_prisk replicated to nassets", {
  obj <- risk_budget_objective(
    assets    = setNames(rep(0.2, 5), assets5),
    name      = "ES",
    min_prisk = 0.05,
    max_prisk = 0.40
  )
  expect_length(obj$min_prisk, 5L)
  expect_length(obj$max_prisk, 5L)
})

test_that("risk_budget_objective(): wrong-length vector min_prisk triggers stop", {
  expect_error(
    risk_budget_objective(
      assets    = setNames(rep(0.2, 5), assets5),
      name      = "ES",
      min_prisk = c(0.1, 0.2, 0.3)  # length 3 != nassets 5
    ),
    regexp = "length"
  )
})
