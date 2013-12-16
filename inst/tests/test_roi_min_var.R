
# minimum variance
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
moments$mu <- rep(0, m)
moments$sigma <- cov(R)

##### ROI #####

# Box constraints
# bnds <- V_bound(li=seq.int(1L, m), lb=as.numeric(constraints$min),
#                 ui=seq.int(1L, m), ub=as.numeric(constraints$max))

# Constraints matrix
Amat <- rbind(rep(1, m), rep(1, m))
dir.vec <- c(">=","<=")
rhs.vec <- c(constraints$min_sum, constraints$max_sum)

# Add min box constraints
Amat <- rbind(Amat, diag(m))
dir.vec <- c(dir.vec, rep(">=", m))
rhs.vec <- c(rhs.vec, constraints$min)

# Add max box constraints
Amat <- rbind(Amat, -1*diag(m))
dir.vec <- c(dir.vec, rep(">=", m))
rhs.vec <- c(rhs.vec, -constraints$max)

# Quadratic objective
ROI_objective <- Q_objective(Q=2 * moments$sigma, L=moments$mu)

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

# Objectives
objQ <- 2 * moments$sigma
objL <- rep(0, m)

# Solve
result <- solve.QP(objQ, objL, t(Amat), rhs.vec)

# Check for equality
context("Test solve.QP and ROI_solve for minimum variance problem")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$value)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution, result$solution)
})

