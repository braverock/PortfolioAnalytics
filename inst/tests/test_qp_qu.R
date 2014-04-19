library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(quadprog)
library(testthat)

# Test that PortfolioAnalytics with ROI.plugin.quadprog solutions equal quadprog solutions
context("Maximum Quadratic Utility Portfolios: PortfolioAnalytics with ROI.plugin.quadprog and quadprog")


##### Data #####
data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)
m <- ncol(R)

##### Parameters #####
portf <- portfolio.spec(funds)
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="box", min=-Inf, max=Inf)
portf <- add.objective(portf, type="risk", name="var")
portf <- add.objective(portf, type="return", name="mean")

# Quadratic part of objective function
objQ <- 2 * cov(R)

# Linear part of objective function
# solve.QP minimizes 1/2 b^T D b - d^T b so we do not need a negative here
objL <- colMeans(R)

# Constraints matrix
Amat <- matrix(1, nrow=1, ncol=m)

# right hand side of constraints
rhs <- 1


##### Unconstrained #####

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat=objQ, dvec=objL, Amat=t(Amat), bvec=rhs, meq=1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="quadprog")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Unconstrained: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_that(weights, equals(opt.qp$solution))
})

test_that("Unconstrained: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.qp$value))
})

##### Long Only #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(0, m)
ub <- rep(1, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat=objQ, dvec=objL, Amat=t(Amat), bvec=rhs, meq=1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="quadprog")
weights <- round(as.numeric(extractWeights(opt.pa)), 10)

test_that("Long Only: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_that(weights, equals(opt.qp$solution))
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Long Only: quadprog bounds are respected", {
  expect_that(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub), is_true())
})

test_that("Long Only: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.qp$value))
})

##### Box #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(0.05, m)
ub <- rep(0.55, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat=objQ, dvec=objL, Amat=t(Amat), bvec=rhs, meq=1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="quadprog")
weights <- round(as.numeric(extractWeights(opt.pa)), 10)


test_that("Box: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_that(weights, equals(opt.qp$solution))
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box: quadoprog bounds are respected", {
  expect_that(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub), is_true())
})

test_that("Box: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.qp$value))
})

##### Box with Shorting #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(-0.05, m)
ub <- rep(0.55, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat=objQ, dvec=objL, Amat=t(Amat), bvec=rhs, meq=1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="quadprog")
weights <- round(as.numeric(extractWeights(opt.pa)), 10)


test_that("Box with Shorting: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_that(weights, equals(opt.qp$solution))
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box with Shorting: quadprog bounds are respected", {
  expect_that(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub), is_true())
})

test_that("Box with Shorting: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.qp$value))
})


