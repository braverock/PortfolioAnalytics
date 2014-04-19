library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
library(testthat)

# Test that ROI.plugin.glpk solutions equal Rglpk solutions
context("Minimum ES Portfolios: PortfolioAnalytics with ROI.plugin.glpk and Rglpk")


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

# Linear part of objective function
objL <- c(rep(0, m), rep(1 / (alpha * n), n), 1)

# Constraints matrix
Amat <- cbind(rbind(1, zoo::coredata(R)), 
              rbind(0, cbind(diag(n), 1)))

# right hand side of constraints
rhs <- c(1, rep(0, n))

# direction of inequality of constraints
dir <- c("==", rep(">=", n))

##### Long Only #####
# Upper and lower bounds (i.e. box constraints)
min_box <- rep(0, m)
max_box <- rep(1, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

bnds <- list(lower = list(ind = seq.int(1L, m+n+1), val = lb),
             upper = list(ind = seq.int(1L, m+n+1), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Long Only: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Long Only: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= min_box) & all(opt.glpk$solution[1:m] <= max_box), is_true())
})

test_that("Long Only: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})

##### Box #####
# Upper and lower bounds (i.e. box constraints)
min_box <- rep(0.05, m)
max_box <- rep(0.55, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

bnds <- list(lower = list(ind = seq.int(1L, m+n+1), val = lb),
             upper = list(ind = seq.int(1L, m+n+1), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Box: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= min_box) & all(opt.glpk$solution[1:m] <= max_box), is_true())
})

test_that("Box: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})

##### Box with Shorting #####
# Upper and lower bounds (i.e. box constraints)
min_box <- rep(-0.05, m)
max_box <- rep(0.55, m)

lb <- c(min_box, rep(0, n), -1)
ub <- c(max_box, rep(Inf, n), 1)

bnds <- list(lower = list(ind = seq.int(1L, m+n+1), val = lb),
             upper = list(ind = seq.int(1L, m+n+1), val = ub))

# Update box constraints in portfolio
portf$constraints[[2]]$min <- min_box
portf$constraints[[2]]$max <- max_box

# Solve optimization with Rglpk
opt.glpk <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, bounds=bnds)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method="glpk")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Box with Shorting: PortfolioAnalytics and Rglpk solution weights are equal", {
  expect_that(weights, equals(opt.glpk$solution[1:m]))
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_that(all(weights >= min_box) & all(weights <= max_box), is_true())
})

test_that("Box with Shorting: Rglpk bounds are respected", {
  expect_that(all(opt.glpk$solution[1:m] >= min_box) & all(opt.glpk$solution[1:m] <= max_box), is_true())
})

test_that("Box with Shorting: PortfolioAnalytics and Rglpk solution objective values are equal", {
  expect_that(opt.pa$out, equals(opt.glpk$optimum))
})


