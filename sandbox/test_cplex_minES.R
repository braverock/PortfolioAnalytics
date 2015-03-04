library(PortfolioAnalytics)
library(Rcplex)
library(ROI)
library(ROI.plugin.cplex)
library(testthat)

# Test that ROI.plugin.cplex solutions equal Rcplex solutions
context("Minimum ES Portfolios: PortfolioAnalytics with ROI.plugin.cplex and Rcplex")

# args(Rcplex)
# ?Rcplex

##### Data #####
data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)

##### Parameters #####
m <- ncol(R)
n <- nrow(R)
alpha <- 0.05

portf <- portfolio.spec(funds)
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="box", min=-Inf, max=Inf)
portf <- add.objective(portf, type="risk", name="ES", arguments=list(p=1-alpha))

# Quadratic part of objective function
objQ <- NULL

# Linear part of objective function
objL <- c(rep(0, m), rep(1 / (alpha * n), n), 1)

# Constraints matrix
Amat <- cbind(rbind(1, zoo::coredata(R)), 
              rbind(0, cbind(diag(n), 1)))

# right hand side of constraints
rhs <- c(1, rep(0, n))

# direction of inequality of constraints
dir <- c("E", rep("G", n))

##### Unconstrained #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
min_box <- rep(-Inf, m)
max_box <- rep(Inf, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)


# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))


test_that("Unconstrained: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt[1:m]))
})

test_that("Unconstrained: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})


##### Long Only #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
min_box <- rep(0, m)
max_box <- rep(1, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Long Only: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt[1:m]))
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Long Only: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt[1:m] >= min_box) & all(opt.rcplex$xopt[1:m] <= max_box), is_true())
})

test_that("Long Only: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

##### Box #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
min_box <- rep(0.05, m)
max_box <- rep(0.55, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt[1:m]))
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Box: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt[1:m] >= min_box) & all(opt.rcplex$xopt[1:m] <= max_box), is_true())
})

test_that("Box: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

##### Box with Shorting #####
# Upper and lower bounds (i.e. box constraints)
# Rcplex bounds
min_box <- rep(-0.05, m)
max_box <- rep(0.55, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rcplex
opt.rcplex <- Rcplex(cvec=objL, Amat=Amat, bvec=rhs, Qmat=objQ, lb=lb, ub=ub, 
                     sense=dir, control=list(trace=0))

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="cplex")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box with Shorting: PortfolioAnalytics and Rcplex solution weights are equal", {
  expect_that(weights, equals(opt.rcplex$xopt[1:m]))
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Box with Shorting: Rcplex bounds are respected", {
  expect_that(all(opt.rcplex$xopt[1:m] >= min_box) & all(opt.rcplex$xopt[1:m] <= max_box), is_true())
})

test_that("Box with Shorting: PortfolioAnalytics and Rcplex solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.rcplex$obj))
})

Rcplex.close()

