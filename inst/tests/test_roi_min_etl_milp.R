
library(testthat)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
library(PerformanceAnalytics)

data(edhec)
R <- edhec[, 1:5]
m <- ncol(R)

constraints <- list()
constraints$min_sum <- 0.99
constraints$max_sum <- 1.01
constraints$min <- rep(0, m)
constraints$max <- rep(1, m)
constraints$max_pos <- 3

moments <- list()
moments$mean <- colMeans(R)

target <- NA
alpha <- 0.05

##### Rglpk #####
# Number of rows
n <- nrow(R)

# Number of columns
m <- ncol(R)

max_sum <- constraints$max_sum
min_sum <- constraints$min_sum
LB <- constraints$min
UB <- constraints$max
max_pos <- constraints$max_pos
min_pos <- 1
moments_mean <- as.numeric(moments$mean)

# A benchmark can be specified in the parma package. 
# Leave this in and set to 0 for now
benchmark <- 0

# Check for target return
if(!is.na(target)){
  # We have a target
  targetcon <- c(moments_mean, rep(0, n+2))
  targetdir <- "=="
  targetrhs <- target
} else {
  # No target specified, just maximize
  targetcon <- NULL
  targetdir <- NULL
  targetrhs <- NULL
}

# Set up initial A matrix
tmpAmat <- cbind(-coredata(R),
                 matrix(-1, nrow=n, ncol=1), 
                 -diag(n),
                 matrix(benchmark, nrow=n, ncol=1))

# Add leverage constraints to matrix
tmpAmat <- rbind(tmpAmat, rbind(c(rep(1, m), rep(0, n+2)),
                                c(rep(1, m), rep(0, n+2))))

# Add target return to matrix
tmpAmat <- rbind(tmpAmat, as.numeric(targetcon))

# This step just adds m rows to the matrix to accept box constraints in the next step
tmpAmat <- cbind(tmpAmat, matrix(0, ncol=m, nrow=dim(tmpAmat)[1]))

# Add lower bound box constraints
tmpAmat <- rbind(tmpAmat, cbind(-diag(m), matrix(0, ncol=n+2, nrow=m), diag(LB)))

# Add upper bound box constraints
tmpAmat <- rbind(tmpAmat, cbind(diag(m), matrix(0, ncol=n+2, nrow=m), diag(-UB)))

# Add row for max_pos cardinality constraints
tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(-1, ncol=m, nrow=1))) 
tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(1, ncol=m, nrow=1))) 

# Set up the rhs vector
rhs <- c( rep(0, n), min_sum, max_sum, targetrhs, rep(0, 2*m), -min_pos, max_pos)

# Set up the dir vector
dir <- c( rep("<=", n), ">=", "<=", targetdir, rep("<=", 2*m), "<=", "<=")

# Linear objective vector
objL <- c( rep(0, m), 1, rep(1/n, n) / alpha, 0, rep(0, m))

# Set up the types vector with continuous and binary variables
types <- c( rep("C", m), "C", rep("C", n), "C", rep("B", m))

bounds <- list( lower = list( ind = 1L:(m + n + 2 + m), val = c(LB,  -1, rep(0, n), 1, rep(0, m)) ),
                upper = list( ind = 1L:(m + n + 2 + m), val = c( UB, 1, rep(Inf, n), 1 , rep(1, m)) ) )


result <- Rglpk_solve_LP(obj=objL, mat=tmpAmat, dir=dir, rhs=rhs, types=types, bounds=bounds)

##### ROI #####
bnds <- V_bound( li = 1L:(m + n + 2 + m), lb = c(LB,  -1, rep(0, n), 1, rep(0, m)),
                 ui = 1L:(m + n + 2 + m), ub = c( UB, 1, rep(Inf, n), 1 , rep(1, m)))

ROI_objective <- L_objective(c( rep(0, m), 1, rep(1/n, n) / alpha, 0, rep(0, m)))

opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=tmpAmat, dir=dir, rhs=rhs),
               bounds=bnds, types=types)
roi.result <- ROI_solve(x=opt.prob, solver="glpk")

context("Test Rglpk_solve_LP and ROI_solve for minimum ES with cardinality constraint")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$optimum)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution[1:m], result$solution[1:m])
})
