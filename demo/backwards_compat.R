#' ---
#' title: "Backwards Compatibility Demo"
#' author: "Ross Bennett"
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve optimization problems using what is
#' referred to as the v1 specification. The v1 specification was used in
#' before PortfolioAnalytics version 0.8.3 to define the optimization problem
#' with constraints and objectives.

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

#' Set up constraint object using v1 specification
gen.constr <- constraint(assets=funds, min=0, max=0.55, min_sum=0.99, max_sum=1.01, 
                         weight_seq=generatesequence(min=0, max=0.55, by=0.002))
class(gen.constr)

#' Add an objective to the gen.constr object
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE)

#' Here we run the optimization. Note that optimize.portfolio will detect 
#' that a v1_constraint object has been passed in and will update to the 
#' v2 specification using a portfolio object with constraints and objectives 
#' from the v1_constraint object.

#' Solve the problem using the random portfolios optimization engine
optrpv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="random", search_size=2000)
optrpv1

#' Solve the problem using the DEoption (Differential Evolution) optimization engine
optdev1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="DEoptim", search_size=2000)
optdev1

#' Solve the problem using the ROI (R Optimization Infrastructure) optimization engine
optroiv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="ROI")
optroiv1


