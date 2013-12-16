
library(testthat)
library(ROI)
library(ROI.plugin.quadprog)
library(quadprog)
library(corpcor)
library(PerformanceAnalytics)

data(edhec)
R <- edhec[, 1:5]
m <- ncol(R)

constraints <- list()
constraints$min_sum <- 1
constraints$max_sum <- 1
constraints$min <- rep(0, m)
constraints$max <- rep(1, m)
constraints$turnover_target <- 5

moments <- list()
moments$mean <- colMeans(R)

lambda <- 1
target <- NA

# Modify the returns matrix. This is done because there are 3 sets of
# variables 1) w.initial, 2) w.buy, and 3) w.sell
R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
returns <- cbind(R, R0, R0)
V <- cov(returns)

# number of assets
N <- ncol(R)

# initial weights for solver
init_weights <- rep(1/ N, N)

# check for a target return constraint
if(!is.na(target)) {
  # If var is the only objective specified, then moments$mean won't be calculated
  if(all(moments$mean==0)){
    tmp_means <- colMeans(R)
  } else {
    tmp_means <- moments$mean
  }
} else {
  tmp_means <- rep(0, N)
  target <- 0
}
Amat <- c(tmp_means, rep(0, 2*N))
dir <- "=="
rhs <- target
meq <- N + 1

# Amat for initial weights
# Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
Amat <- rbind(Amat, cbind(diag(N), -1*diag(N), diag(N)))
rhs <- c(rhs, init_weights)
dir <- c(dir, rep("==", N))

# Amat for turnover constraints
Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(-1, N)))
rhs <- c(rhs, -constraints$turnover_target)
dir <- c(dir, ">=")

# Amat for positive weights
Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# Amat for negative weights
Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), diag(N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# Amat for full investment constraint
Amat <- rbind(Amat, rbind(c(rep(1, N), rep(0,2*N)), c(rep(-1, N), rep(0,2*N))))
rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
dir <- c(dir, ">=", ">=")

# Amat for lower box constraints
Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, constraints$min)
dir <- c(dir, rep(">=", N))

# Amat for upper box constraints
Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, -constraints$max)
dir <- c(dir, rep(">=", N))

d <- rep(tmp_means, 3)

Amat <- Amat[!is.infinite(rhs), ]
rhs <- rhs[!is.infinite(rhs)]

result <- solve.QP(Dmat=make.positive.definite(2*lambda*V), 
                   dvec=d, Amat=t(Amat), bvec=rhs, meq=meq)
result
wts <- result$solution
wts.final <- wts[(1:N)]

##### ROI #####
ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), 
                             L=rep(-tmp_means, 3))

opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))

roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
print.default(roi.result)
weights <- result$solution[(1:N)]

context("Test solve.QP and ROI_solve for gmv with turnover constraint")

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$value)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution[1:m], result$solution[1:m])
})

