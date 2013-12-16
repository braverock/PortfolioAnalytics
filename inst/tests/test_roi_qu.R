
# quadratic utility
library(testthat)
library(ROI)
library(ROI.plugin.quadprog)
library(quadprog)
library(PerformanceAnalytics)

data(edhec)
R <- edhec[, 1:5]
m <- ncol(R)

constraints <- list()
constraints$min_sum <- 1
constraints$max_sum <- 1
constraints$min <- rep(0, m)
constraints$max <- rep(1, m)

moments <- list()
moments$mu <- colMeans(R)
moments$sigma <- cov(R)

lambda <- 0.5

##### ROI #####

# Box constraints
# bnds <- list(li=seq.int(1L, m), lb=as.numeric(constraints$min)),
#              ui=seq.int(1L, m), ub=as.numeric(constraints$max)))

# Constraints matrix
Amat <- rbind(rep(1, m), rep(-1, m))
dir.vec <- c(">=",">=")
rhs.vec <- c(constraints$min_sum, -constraints$max_sum)

# Add min box constraints
Amat <- rbind(Amat, diag(m))
dir.vec <- c(dir.vec, rep(">=", m))
rhs.vec <- c(rhs.vec, constraints$min)

# Add max box constraints
Amat <- rbind(Amat, -1*diag(m))
dir.vec <- c(dir.vec, rep(">=", m))
rhs.vec <- c(rhs.vec, -constraints$max)

# Quadratic objective
ROI_objective <- Q_objective(Q=2 * lambda * moments$sigma, L=-moments$mu)

# Set up the optimization problem and solve
opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec))
roi.result <- ROI_solve(x=opt.prob, solver="quadprog")

##### quadprog #####

# Constraints matrix
Amat <- rbind(rep(1, m), rep(-1, m))
rhs.vec <- c(constraints$min_sum, -constraints$max_sum)

# Box constraints
Amat <- rbind(Amat, diag(m), -1*diag(m))
rhs.vec <- c(rhs.vec, constraints$min, -constraints$max)

# Quadratic and linear bjectives
objQ <- 2 * lambda * moments$sigma
objL <- moments$mu

# Solve
result <- solve.QP(objQ, objL, t(Amat), rhs.vec)

# Check for equality
# lambda = 0.5
context("Test solve.QP and ROI_solve for quadratic utility lambda=0.5")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$value)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution, result$solution)
})


# Very small penalty term is equivalent to max return objective
ROI_objective <- Q_objective(Q=2 * 1e-6 * moments$sigma, L=-moments$mu)

# Set up the optimization problem and solve
opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec))
roi.result.maxret <- ROI_solve(x=opt.prob, solver="quadprog")

objQ <- 2 * 1e-6 * moments$sigma
result.maxret <- solve.QP(objQ, objL, t(Amat), rhs.vec, 2)

# lambda = 1e-6
context("Test solve.QP and ROI_solve for quadratic utility lambda=1e-6")

test_that("Objective values are equal", {
  expect_equal(roi.result.maxret$objval, result.maxret$value)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result.maxret$solution, result.maxret$solution)
})

# Very large penalty term is equivalent to min variance objective
ROI_objective <- Q_objective(Q=2 * 1e6 * moments$sigma, L=-moments$mu)

# Set up the optimization problem and solve
opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec))
roi.result.minvar <- ROI_solve(x=opt.prob, solver="quadprog")

objQ <- 2 * 1e6 * moments$sigma
result.minvar <- solve.QP(objQ, objL, t(Amat), rhs.vec, 2)

# lambda = 1e6
context("Test solve.QP and ROI_solve for quadratic utility lambda=1e6")

test_that("Objective values are equal", {
  expect_equal(roi.result.minvar$objval, result.minvar$value)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result.minvar$solution, result.minvar$solution)
})

