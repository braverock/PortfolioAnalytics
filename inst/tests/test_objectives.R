
require(testthat)
require(PortfolioAnalytics)

context("objectives")

N <- 4
init.portf <- portfolio.spec(assets=N)
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean", target=0.005)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES", arguments=list(p=0.95))
init.portf <- add.objective(portfolio=init.portf, type="risk_budget", name="ES")

test_that("return objective is consistent", {
  expect_that(init.portf$objectives[[1]]$name, equals("mean"))
  expect_that(init.portf$objectives[[1]]$target, equals(0.005))
  expect_that(init.portf$objectives[[1]]$enabled, is_true())
  expect_that(init.portf$objectives[[1]]$multiplier, equals(-1))
  expect_that(class(init.portf$objectives[[1]]), equals(c("return_objective", "objective")))
})

test_that("risk objective is consistent", {
  expect_that(init.portf$objectives[[2]]$name, equals("ES"))
  expect_that(is.null(init.portf$objectives[[2]]$target), is_true())
  expect_that(init.portf$objectives[[2]]$arguments$portfolio_method, equals("single"))
  expect_that(init.portf$objectives[[2]]$arguments$p, equals(0.95))
  expect_that(init.portf$objectives[[2]]$enabled, is_true())
  expect_that(init.portf$objectives[[2]]$multiplier, equals(1))
  expect_that(class(init.portf$objectives[[2]]), equals(c("portfolio_risk_objective", "objective")))
})

test_that("risk objective is consistent", {
  expect_that(init.portf$objectives[[3]]$name, equals("ES"))
  expect_that(is.null(init.portf$objectives[[3]]$target), is_true())
  expect_that(init.portf$objectives[[3]]$arguments$portfolio_method, equals("component"))
  expect_that(init.portf$objectives[[3]]$enabled, is_true())
  expect_that(init.portf$objectives[[3]]$multiplier, equals(1))
  expect_that(init.portf$objectives[[3]]$min_concentration, is_true())
  expect_that(init.portf$objectives[[3]]$min_difference, is_false())
  expect_that(class(init.portf$objectives[[3]]), equals(c("risk_budget_objective", "objective")))
})
