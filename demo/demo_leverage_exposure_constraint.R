#' ---
#' title: "Leverage Exposure Constraint Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a portfolio optimization problem with
#' leverage exposure constraints.
#' 

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

#' Set up an initial portfolio object with basic constraints.
init.portf <- portfolio.spec(assets=funds)

#' Add an objective to maximize mean return per unit expected shortfall.
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

#' The leverage_exposure constraint type is supported for random, DEoptim, pso,
#' and GenSA solvers. The following examples use DEoptim for solving the
#' optimization problem.

#' Dollar neutral portfolio with max 2:1 leverage constraint.
dollar.neutral.portf <- init.portf
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="weight_sum", 
                                       min_sum=-0.01, max_sum=0.01)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="box", min=-0.5, max=0.5)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="leverage_exposure", leverage=2)

#' Run optimization using DEoptim as optimization backend.
dollar.neutral.opt <- optimize.portfolio(R=R, portfolio=dollar.neutral.portf, 
                                         optimize_method="DEoptim",
                                         search_size=2000)
dollar.neutral.opt

#' Set up leveraged portfolio with max 1.6:1 leverage exposure constraint.
leveraged.portf <- init.portf
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                       type="weight_sum", 
                                       min_sum=0.99, max_sum=1.01)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                       type="box", min=-0.3, max=0.8)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                       type="leverage_exposure", leverage=1.6)

#' Run optimization using DEoptim as optimization backend.
leveraged.opt <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                                         optimize_method="DEoptim",
                                         search_size=2000)
leveraged.opt

