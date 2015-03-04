library(PortfolioAnalytics)
library(Rcplex)
library(ROI)
library(ROI.plugin.cplex)
library(testthat)

# Test that PortfolioAnalytics with ROI.plugin.cplex solutions equal Rcplex solutions
context("GMV Portfolios: PortfolioAnalytics with ROI.plugin.cplex and Rcplex")

# args(Rcplex)
# ?Rcplex

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

# Quadratic part of objective function
objQ <- 2 * cov(R)

# Linear part of objective function
objL <- rep(0, m)

# Constraints matrix
Amat <- matrix(1, nrow=1, ncol=m)

# right hand side of constraints
rhs <- 1

# direction of inequality of constraints
dir <- "E"

##### Unconstrained #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
lb <- rep(-Inf, m)
ub <- rep(Inf, m)

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Unconstrained: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt))
})

test_that("Unconstrained: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

##### Long Only #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
lb <- rep(0, m)
ub <- rep(1, m)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Long Only: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt))
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Long Only: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt >= lb) & all(opt.rcplex$xopt <= ub), is_true())
})

test_that("Long Only: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

##### Box #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
lb <- rep(0.05, m)
ub <- rep(0.55, m)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))


test_that("Box: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt))
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt >= lb) & all(opt.rcplex$xopt <= ub), is_true())
})

test_that("Box: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

##### Box with Shorting #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
lb <- rep(-0.05, m)
ub <- rep(0.55, m)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))


test_that("Box with Shorting: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt))
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box with Shorting: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt >= lb) & all(opt.rcplex$xopt <= ub), is_true())
})

test_that("Box with Shorting: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

Rcplex.close()
