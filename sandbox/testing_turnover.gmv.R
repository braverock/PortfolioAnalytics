
# script to solve the gmv optimization problem with turnover constraints using quadprog or ROI

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(quadprog)
library(corpcor)

# TODO Add documentation for function
# Computes optimal weights for global minimum variance portfolio with
# constraints including turnover constraint
turnover.gmv <- function(R, toc, weight.i, min, max){
  
  # number of assets in R
  p <- ncol(R)
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables w.initial, w.buy, and w.sell
  returns <- cbind(R, R, R)
  
  V <- cov(returns)
  V <- make.positive.definite(V)
  
  # A matrix for initial weights
  A2 <- cbind(rep(1, p*3), rbind(diag(p), matrix(0, nrow=2*p, ncol=p)))
  
  # A matrix for lower box constraints
  Alo <- rbind(diag(p), diag(p), diag(p))
  
  # A matrix for upper box constraints
  Aup <- rbind(-diag(p), -diag(p), -diag(p))
  
  # vector to appply turnover constraint
  A3 <- c(rep(0, p), rep(-1, p), rep(1, p))
  
  # matrix for positive weight
  A4 <- rbind(matrix(0, nrow=p, ncol=p), diag(p), matrix(0, nrow=p, ncol=p))
  
  # matrix for negative weight
  A5 <- rbind(matrix(0, nrow=p*2, ncol=p), -diag(p))
  
  # Combine the temporary A matrices
  A.c <- cbind(A2, Alo, Aup, A3, A4, A5)
  
  # b vector holding the values of the constraints
  b <- c(1, weight.i, min, -max, -toc, rep(0, 2*p))
  
  # no linear term so set this equal to 0s
  d <- rep(0, p*3)
  
  sol <- solve.QP(Dmat=V, dvec=d, Amat=A.c, bvec=b, meq=6)
  wts <- sol$solution
  wts.final <- wts[(1:p)] + wts[(1+p):(2*p)] + wts[(2*p+1):(3*p)]
  wts.final
}

data(edhec)
ret <- edhec[,1:5]

# box constraints min and max
min <- rep(0.1, 5)
max <- rep(0.6, 5)

# turnover constraint
toc <- 0.3

# Initial weights vector
weight.i <- rep(1/5,5)

opt.wts <- turnover.gmv(R=ret, toc=toc, weight.i=weight.i, min=min, max=max)
opt.wts

# calculate turnover
to <- sum(abs(diff(rbind(weight.i, opt.wts))))
to

##### ROI Turnover constraints using ROI solver #####
# Not working correctly. 
# Getting a solution now, but results are different than turnover.gmv

# library(ROI)
# library(ROI.plugin.quadprog)
# 
# 
# # Use the first 5 funds in edhec for the returns data
# ret <- edhec[, 1:5]
# returns <- cbind(ret, ret, ret)
# 
# V <- cov(returns)
# V <- corpcor:::make.positive.definite(V)
# mu <- apply(returns, 2, mean)
# # number of assets
# N <- ncol(returns)
# 
# # Set the box constraints for the minimum and maximum asset weights
# min <- rep(0.1, N/3)
# max <- rep(0.6, N/3)
# 
# # Set the bounds
# bnds <- list(lower = list(ind = seq.int(1L, N/3), val = as.numeric(min)),
#              upper = list(ind = seq.int(1L, N/3), val = as.numeric(max)))
# lambda <- 1
# ROI_objective <- ROI:::Q_objective(Q=2*lambda*V, L=-mu*0)
# 
# # Set up the Amat
# # min_sum and max_sum of weights
# A1 <- rbind(rep(1, N), rep(1, N))
# 
# # initial weight matrix
# A.iw <- cbind(diag(N/3), matrix(0, nrow=N/3, ncol=2*N/3))
# 
# # turnover vector
# A.t <- c(rep(0, N/3), rep(-1, N/3), rep(1, N/3))
# 
# A.wpos <- t(cbind(rbind(matrix(0, ncol=N/3, nrow=N/3), diag(N/3), matrix(0, ncol=N/3, nrow=N/3)),
#                   rbind(matrix(0, ncol=N/3, nrow=2*N/3), -diag(N/3))))
# 
# Amat <- rbind(A1, A.iw, A.t, A.wpos)
# 
# dir.vec <- c(">=","<=", rep("==", N/3), "<=", rep(">=", 2*N/3))
# min_sum=1
# max_sum=1
# w.init <- rep(1/5, 5)
# toc <- 0.3
# rhs.vec <- c(min_sum, max_sum, w.init, toc, rep(0, 2*N/3))
# 
# opt.prob <- ROI:::OP(objective=ROI_objective, 
#                      constraints=ROI:::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
#                      bounds=bnds)
# roi.result <- ROI:::ROI_solve(x=opt.prob, solver="quadprog")
# wts.tmp <- roi.result$solution
# wts <- wts.tmp[1:(N/3)] + wts.tmp[(N/3+1):(2*N/3)] + wts.tmp[(2*N/3+1):N]
# wts