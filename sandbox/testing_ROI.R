# # # # # # # # # # # # # #
#  OPTIMIZATION TESTING 
#

library(xts)
library(quantmod)
library(quadprog)
library(Rglpk)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(Ecdat)
library(PortfolioAnalytics)

data(edhec)
cov.mat <- var(edhec)
mu.vec <- apply(edhec, 2, mean)
n.assets <- ncol(edhec)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Sample portfolio optimization problems....

# =====================
# Arbitrage
# 
# to check the values of this example, I did the same problem with Rnupot.
# unfortunately, ROI (or Rglpk, more specifically) cannot solve this problem well.
# when using a RHS term in the constraint of Ax >= 0, the solution is trivial,
# with RHS = rep(0.001, n.assets), the solution is non-trivial, but wrong.
#
# data(edhec)
# set.seed(123)
# n.assets <- ncol(edhec)
# S <- 1 + apply(edhec, 2, sample, n.assets)
# solution <- solveQP(objL=rep(1, ncol(S)), S,
#                     cLO=rep(0, nrow(S)), cUP=rep(Inf, nrow(S)),
#                     type=minimize)
# wts <- solution$variables$x$current
# > wts
# 1           2           3           4           5           6           7 
# -0.03687089  0.34978850 -0.02893078  0.15988671  0.29418430  0.02867836 -0.29697380 
# 8           9          10          11          12          13 
# -0.45799863 -0.30662611 -0.11010508 -0.74430483 -0.02668694  0.18126218

set.seed(123)
S.mat <- 1 + apply(edhec, 2, sample, size=n.assets)
bnds <- list(lower = list(ind = seq.int(1L, as.integer(n.assets)), val = rep(-Inf,n.assets)),
             upper = list(ind = seq.int(1L, as.integer(n.assets)), val = rep(Inf,n.assets)))
q.prob <- OP(objective = L_objective(L=rep(1, n.assets)),
             constraints = L_constraint(L=S.mat,
                                        dir=rep(">=", n.assets),
                                        rhs=rep(0.001, n.assets)),
             bounds=bnds)
test <- ROI_solve(x=q.prob, solver="glpk")
# > test$solution
# [1] -0.37584904  0.00000000  0.00000000  0.18434868  0.00000000  0.00000000  0.00000000  0.32167429  0.14030506
# [10]  0.00000000 -0.41200277 -0.02153654  0.25916374




# =====================
# Mean-variance
#
mu.port <- 0.002
Amat <- cbind(rep(1,n.assets),mu.vec)
q.prob <- OP(objective=Q_objective(Q=-2*cov.mat, L=mu.vec), 
             constraints=L_constraint(L=t(Amat),
                                      dir=c("==","=="),
                                      rhs=c(1,mu.port)),
             bounds=bnds,
             maximum=TRUE)
wts <- ROI_solve(x=q.prob, solver="quadprog")$solution


# Comparing resutls wtih Guy's slides of PortfolioOptimization
# sllide number 24/70, mean-variance optimization 
# subject to fully-invested and expected portfolio return constraints
data(CRSPday)
R <- 100*CRSPday[,4:6]
mean_vect <- apply(R,2,mean)
cov_mat <- var(R)
Amat <- rbind(rep(1,3),mean_vect)
mu.port <- 0.1
bnds <- list(lower = list(ind = seq.int(1L, as.integer(3)), val = rep(-Inf,3)),
             upper = list(ind = seq.int(1L, as.integer(3)), val = rep(Inf,3)))
q.prob <- OP(objective=Q_objective(Q=2*cov_mat, L=rep(0,3)), 
             constraints=L_constraint(L=Amat,
                                      dir=c("==","=="),
                                      rhs=c(1, mu.port)),
             bounds=bnds)
wts <- ROI_solve(x=q.prob, solver="quadprog")$solution


