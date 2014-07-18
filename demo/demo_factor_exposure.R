#' ---
#' title: "Factor Exposure Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a portfolio optimization problem with
#' factor exposure constraints.

#' Load the required packages
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)

#' Load the data
data(edhec)
ret <- edhec[, 1:4]

#' Create portfolio object
pspec <- portfolio.spec(assets=colnames(ret))

#' Here we define individual constraint objects.
#' Leverage constraint.
lev_constr <- weight_sum_constraint(min_sum=1, max_sum=1)

#' Box constraint
lo_constr <- box_constraint(assets=pspec$assets, min=c(0.01, 0.02, 0.03, 0.04), max=0.65)

#' Group constraint'
grp_constr <- group_constraint(assets=pspec$assets, groups=list(1:2, 3, 4), group_min=0.1, group_max=0.4)

#' Position limit constraint
pl_constr <- position_limit_constraint(assets=pspec$assets, max_pos=4)

#' Make up a B matrix for an industry factor model.
#' dummyA, dummyB, and dummyC could be industries, sectors, etc.
B <- cbind(c(1, 1, 0, 0),
           c(0, 0, 1, 0),
           c(0, 0, 0, 1))
rownames(B) <- colnames(ret)
colnames(B) <- c("dummyA", "dummyB", "dummyC")
lower <- c(0.1, 0.1, 0.1)
upper <- c(0.4, 0.4, 0.4)

#' Industry exposure constraint.
#' The exposure constraint and group constraint are equivalent to test that 
#' they result in the same solution.
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=B, lower=lower, upper=upper)

#' Here we define objectives.
#' 
#' Objective to minimize variance.
var_obj <- portfolio_risk_objective(name="var")

#' Objective to maximize return.
ret_obj <- return_objective(name="mean")

#' Objective to minimize ETL.
etl_obj <- portfolio_risk_objective(name="ETL")

#' Run optimization on minimum variance portfolio with leverage, long only,
#' and group constraints.
opta <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(var_obj), 
                          optimize_method="ROI")
opta

#' Run optimization on minimum variance portfolio with leverage, long only,
#' and factor exposure constraints.
optb <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, exp_constr), 
                          objectives=list(var_obj), 
                          optimize_method="ROI")
optb

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(opta$weights, optb$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints.
optc <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(ret_obj), 
                          optimize_method="ROI")
optc

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints.
optd <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="ROI")
optd

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optc$weights, optd$weights)

#' Run optimization on minimum expected tail loss portfolio with leverage, 
#' long only, and group constraints.
opte <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(etl_obj), 
                          optimize_method="ROI")
opte

#' Run optimization on minimum expected tail loss portfolio with leverage, 
#' long only, and factor exposure constraints.
optf <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, exp_constr), 
                          objectives=list(etl_obj), 
                          optimize_method="ROI")
optf

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(opte$weights, optf$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints using DEoptim as the optimization engine.
set.seed(123)
optde1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)
optde1

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints using DEoptim as the optimization engine.
set.seed(123)
optde2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)
optde2

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optde1$weights, optde2$weights)

#' Run optimization on maximum return portfolio with leverage, long only,
#' and group constraints using random portfolios as the optimization
#' engine.
optrp1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)
optrp1

#' Run optimization on maximum return portfolio with leverage, long only,
#' and factor exposure constraints using random portfolios as the optimization
#' engine.
optrp2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)
optrp2

#' Note that the portfolio with the group constraint and exposure constraint 
#' should result in same solution.
all.equal(optrp1$weights, optrp2$weights)
