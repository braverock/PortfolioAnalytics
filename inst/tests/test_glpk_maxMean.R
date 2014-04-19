library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
library(testthat)

# Test that ROI.plugin.glpk solutions equal Rglpk solutions
context("Maximum Mean Return Portfolios: PortfolioAnalytics with ROI.plugin.glpk and Rglpk")


##### Data #####
data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)

##### Parameters #####
m <- ncol(R)

portf <- portfolio.spec(funds)
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="box", min=-Inf, max=Inf)
portf <- add.objective(portf, type="return", name="mean")

# Linear part of objective function
objL <- -colMeans(R)

# Constraints matrix
Amat <- matrix(1, nrow=1, ncol=m)

# right hand side of constraints
rhs <- 1

# direction of inequality of constraints
dir <- "=="

##### Long Only #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(0, m)
ub <- rep(1, m)

bnds <- list(lower = list(ind = seq.int(1L, m), val = lb),
             upper = list(ind = seq.int(1L, m), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Long Only: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Long Only: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= lb) & all(opt.glpk$solution[1:m] <= ub), is_true())
})

test_that("Long Only: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})

##### Box #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(0.05, m)
ub <- rep(0.55, m)

bnds <- list(lower = list(ind = seq.int(1L, m), val = lb),
             upper = list(ind = seq.int(1L, m), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= lb) & all(opt.glpk$solution[1:m] <= ub), is_true())
})

test_that("Box: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})

##### Box with Shorting #####
# Upper and lower bounds (i.e. box constraints)
lb <- rep(-0.05, m)
ub <- rep(0.55, m)

bnds <- list(lower = list(ind = seq.int(1L, m), val = lb),
             upper = list(ind = seq.int(1L, m), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box with Shorting: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= lb) & all(weights <= ub), is_true())
})

test_that("Box with Shorting: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= lb) & all(opt.glpk$solution[1:m] <= ub), is_true())
})

test_that("Box with Shorting: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})


