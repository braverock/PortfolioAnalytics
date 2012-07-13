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
bnds <- list(lower = list(ind = seq.int(1L, n.assets), val = rep(-Inf,n.assets)),
             upper = list(ind = seq.int(1L, n.assets), val = rep(Inf,n.assets)))
arb.prob <- OP(objective = L_objective(L=rep(1, n.assets)),
               constraints = L_constraint(L=S.mat,
                                          dir=rep(">=", n.assets),
                                          rhs=rep(0.001, n.assets)),
               bounds=bnds)
arb.constr <- constraint_ROI(op.problem=arb.prob, "glpk")
arb.test <- optimize.portfolio(edhec, arb.constr, "ROI")
# > test$solution with ROI, same is found via optimize.portfolio(..., method="ROI")
# [1] -0.37584904  0.00000000  0.00000000  0.18434868  0.00000000  0.00000000  0.00000000  0.32167429  0.14030506
# [10]  0.00000000 -0.41200277 -0.02153654  0.25916374
# 
# Issues that I have found:
# 1) arb.test$constraints returns:
#
# > test$constraints
# An object containing 6 constraints.
# Some constraints are of type nonlinear.
#
# which is meaningless,
#
# 2) when not passing in assets into arb.prob, this is returned:
#
# assuming equal weighted seed portfolio
# assuming equal weighted seed portfolio
# 
# only should say it once.



# =====================
# Mean-variance:  Maximize quadratic utility
#
funds <- names(edhec)
mu.port <- 0.002
Amat <- rbind(rep(1,n.assets), mu.vec)
lambda <- 1
mean.var.prob <- OP(objective=Q_objective(Q=2*lambda*cov.mat, L=-mu.vec), 
                    constraints=L_constraint(L=Amat,
                                             dir=c("==","=="),
                                             rhs=c(1,mu.port)),
                    bounds=bnds)
mean.var.constr <- constraint_ROI(assets=funds, op.problem=mean.var.prob, solver="quadprog")
wts <- ROI_solve(x=mean.var.prob, solver="quadprog")$solution
mean.var.solution <- optimize.portfolio(edhec, mean.var.constr, "ROI")
# results for this are:
#
# > mean.var.solution$weights
# Convertible Arbitrage             CTA Global  Distressed Securities       Emerging Markets 
# -0.38704286             0.08062104            -0.35410876            -0.06088908 
# Equity Market Neutral           Event Driven Fixed Income Arbitrage           Global Macro 
# 0.49829093            -0.49045547             0.74727967            -0.49173927 
# Long/Short Equity       Merger Arbitrage         Relative Value          Short Selling 
# -0.44361180             0.52390274             0.37357962            -0.04284137 
# Funds of Funds 
# 1.04701463 
# > wts
# [1] -0.38704286  0.08062104 -0.35410876 -0.06088908  0.49829093 -0.49045547  0.74727967
# [8] -0.49173927 -0.44361180  0.52390274  0.37357962 -0.04284137  1.04701463
#
# however, the constraints show up as:
# $constraints
# An object containing 6 constraints.
# Some constraints are of type nonlinear.



# ========================================================
# Mean-variance:  Maximize quadratic utility --- dollar neutral
#
funds <- names(edhec)
mu.port <- 0.002
Amat <- cbind(rep(1,n.assets), mu.vec)
dollar.neutral.prob <- OP(objective=Q_objective(Q=2*cov.mat, L=-mu.vec), 
                          constraints=L_constraint(L=t(Amat),
                                                   dir=c("==","=="),
                                                   rhs=c(0,mu.port)),
                          bounds=bnds)
dollar.neutral.constr <- constraint_ROI(assets=funds, op.problem=dollar.neutral.prob, solver="quadprog")
wts <- ROI_solve(x=dollar.neutral.prob, solver="quadprog")$solution
dollar.neutral.solution <- optimize.portfolio(edhec, dollar.neutral.constr, "ROI")
paste(funds, dollar.neutral.solution$weights)

# using integrated ROI
mean.var <- constraint(assets = colnames(edhec), min = -Inf, max = Inf, min_sum=0, max_sum=0, risk_aversion=1)
mean.var <- add.objective(constraints=mean.var, type="return", name="mean", enabled=TRUE, multiplier=0, target=mu.port)
mean.var <- add.objective(constraints=mean.var, type="risk", name="var", enabled=TRUE, multiplier=0)
solution <- optimize.portfolio(edhec, mean.var, "ROI_new")
paste(names(edhec),solution$weights)


# =====================
# Maximize return given box constraints
#
# A set of box constraints used to initialize portfolios
init.constr <- constraint(assets = colnames(edhec), min = .05, max = .3, min_sum=1, max_sum=1)
init.constr <- add.objective(constraints=init.constr, type="return", name="mean", enabled=TRUE, multiplier=0)
test <- optimize.portfolio(edhec, init.constr, "ROI_new")




# =====================
# Mean-variance:  Maximize return, with constraint on variance
# setting varaince to be the average variance between the edhec funds.
#
avg.var <- mean(apply(edhec, 2, var))
max.mean.prob <- OP(objective=L_objective(mu.vec), 
                    constraints=Q_constraint(Q=list(2*cov.mat, matrix(0, nrow=n.assets, ncol=n.assets)),
                                             L=rbind(rep(0, n.assets),rep(1,n.assets)),
                                             dir=c("==","=="),
                                             rhs=c(avg.var,1)),
                    bounds=bnds,
                    maximum=TRUE)
max.mean.constr <- constraint_ROI(assets=funds, op.problem=max.mean.prob, solver="glpk")
wts <- ROI_solve(x=max.mean.prob, solver="glpk")$solution
max.mean.solution <- optimize.portfolio(edhec, max.mean.constr, "ROI")
# 
#  As expected, attempting to set up this problem leads to failure.
#  need to implement a second order conic solver.
#  this solver is in the package CLSOCP


# ===================
# This secion works, and I am puting it aside for now
#
# # Comparing resutls wtih Guy's slides of PortfolioOptimization
# # sllide number 24/70, mean-variance optimization 
# # subject to fully-invested and expected portfolio return constraints
# data(CRSPday)
# R <- 100*CRSPday[,4:6]
# mean_vect <- apply(R,2,mean)
# cov_mat <- var(R)
# Amat <- rbind(rep(1,3),mean_vect)
# mu.port <- 0.1
# bnds <- list(lower = list(ind = seq.int(1L, as.integer(3)), val = rep(-Inf,3)),
#              upper = list(ind = seq.int(1L, as.integer(3)), val = rep(Inf,3)))
# q.prob <- OP(objective=Q_objective(Q=2*cov_mat, L=rep(0,3)), 
#              constraints=L_constraint(L=Amat,
#                                       dir=c("==","=="),
#                                       rhs=c(1, mu.port)),
#              bounds=bnds)
# wts <- ROI_solve(x=q.prob, solver="quadprog")$solution


