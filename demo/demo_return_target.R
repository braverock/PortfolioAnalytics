#' ---
#' title: "Target Return Constraint Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained portfolio optimization 
#' problem with a target return constraint.

#' Load packages and data
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
data(edhec)
ret <- edhec[, 1:4]

#' Create an initial portfolio object with basic constraints.
init.portf <- portfolio.spec(assets=colnames(ret))
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

#' Add mean return objective with target return.
ret.obj.portf <- add.objective(portfolio=init.portf, type="return", 
                         name="mean", target=0.007)

#' Add return target constraint.
ret.constr.portf <- add.constraint(portfolio=init.portf, type="return", return_target=0.007)

#' Add mean return objective to the portfolio with the target return constraint.
ret.constr.portf <- add.objective(portfolio=ret.constr.portf, type="return", name="mean")

#' The following optimization demonstrate the a target return constraint is
#' equivalent to a return objective with a target.

#' Run optimization using ROI with target return as an objective.
ret.obj.opt <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, optimize_method="ROI")
ret.obj.opt

#' Run optimization using ROI with target return as a constraint.
ret.constr.opt <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, optimize_method="ROI")
ret.constr.opt

#' Relax the leverage constraints for the sum of weights for DEoptim and 
#' random portfolios.
ret.obj.portf$constraints[[1]]$min_sum <- 0.99
ret.obj.portf$constraints[[1]]$max_sum <- 1.01

ret.constr.portf$constraints[[1]]$min_sum <- 0.99
ret.constr.portf$constraints[[1]]$max_sum <- 1.01

#' Run the optimizations using DEoptim as the optimization engine.
set.seed(123)
opt.obj.de <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, 
                                 optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.obj.de

# run optimization with DEoptim using ret.constr.portf
set.seed(123)
opt.constr.de <- optimize.portfolio(R=ret, portfolio=ret.constr.portf,
                                    optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.constr.de

#' Run the optimizations using DEoptim as the optimization engine.
opt.obj.rp <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, 
                                 optimize_method="random", search_size=2000)
opt.obj.rp

# run optimizations with random portfolios using ret.constr.portf
opt.constr.rp <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, 
                                    optimize_method="random", search_size=2000)
opt.constr.rp
