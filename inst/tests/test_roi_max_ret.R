
library(testthat)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
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

##### ROI #####

# Box constraints
bnds <- V_bound(li=seq.int(1L, m), lb=as.numeric(constraints$min),
                 ui=seq.int(1L, m), ub=as.numeric(constraints$max))

# Constraints matrix
Amat <- rbind(rep(1, m), rep(1, m))
dir.vec <- c(">=","<=")
rhs.vec <- c(constraints$min_sum, constraints$max_sum)

# Linear objective
ROI_objective <- L_objective(L=-moments$mu)

# Set up the optimization problem and solve
opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
               bounds=bnds)
roi.result <- ROI_solve(x=opt.prob, solver="glpk")

##### Rglpk #####
# Box Constraints
bnds <- list(lower=list(ind=seq.int(1L, m), val=as.numeric(constraints$min)),
             upper=list(ind=seq.int(1L, m), val=as.numeric(constraints$max)))

# Constraints matrix
Amat <- rbind(rep(1, m), rep(1, m))
dir.vec <- c(">=","<=")
rhs.vec <- c(constraints$min_sum, constraints$max_sum)

# Linear objective
objL <- -moments$mu

# Solve
result <- Rglpk_solve_LP(objL, Amat, dir.vec, rhs.vec)


# Check equality
context("Test Rglpk_solve_LP and ROI_solve for maximimum return")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$optimum)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution, result$solution)
})
