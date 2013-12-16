
# minimum ETL
library(testthat)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
library(PerformanceAnalytics)

data(edhec)
R <- edhec[, 1:5]
m <- ncol(R)
n <- nrow(R)

constraints <- list()
constraints$min_sum <- 1
constraints$max_sum <- 1
constraints$min <- rep(0, m)
constraints$max <- rep(1, m)

moments <- list()
moments$mu <- colMeans(R)

alpha <- 0.05
target <- mean(colMeans(R))

##### ROI #####

# Box constraints
LB <- c(as.numeric(constraints$min), rep(0, n), -1)
UB <- c(as.numeric(constraints$max), rep(Inf, n), 1)
bnds <- V_bound(li=seq.int(1L, m+n+1), lb=LB,
                ui=seq.int(1L, m+n+1), ub=UB)

# Constraint matrix
Amat <- cbind(rbind(1, 1, moments$mu, coredata(R)), rbind(0, 0, 0, cbind(diag(n), 1))) 
dir.vec <- c(">=", "<=", ">=", rep(">=", n))
rhs.vec <- c(constraints$min_sum, constraints$max_sum, target, rep(0, n))

# Linear objective
ROI_objective <- L_objective(c(rep(0, m), rep(1 / (alpha * n), n), 1))

# Set up the optimization problem and solve
opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
               bounds=bnds)
roi.result <- ROI_solve(x=opt.prob, solver="glpk")

##### Rglpk #####

# Box constraints
bnds <- list(lower=list(ind=seq.int(1L, m), val=as.numeric(constraints$min)),
             upper=list(ind=seq.int(1L, m), val=as.numeric(constraints$max)))

# Constraint matrix
Amat <- cbind(rbind(1, 1, moments$mu, coredata(R)), rbind(0, 0, 0, cbind(diag(n), 1))) 
dir.vec <- c(">=", "<=", ">=", rep(">=", n))
rhs.vec <- c(constraints$min_sum, constraints$max_sum, target, rep(0, n))

# Linear objective
objL <- c(rep(0, m), rep(1 / (alpha * n), n), 1)

# Solve
result <- Rglpk_solve_LP(objL, Amat, dir.vec, rhs.vec, bnds)

context("Test Rglpk_solve_LP and ROI_solve for minimum ES")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$optimum)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution[1:m], result$solution[1:m])
})

