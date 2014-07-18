#' ---
#' title: "Maximizing Sharpe Ratio Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained
#' portfolio optimization problem to maximize Sharpe Ratio.

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf

#' Maximizing Sharpe Ratio can be formulated as a quadratic programming 
#' problem and solved very quickly using optimize_method="ROI". Although "StdDev"
#' was specified as an objective, the quadratic programming problem uses the 
#' variance-covariance matrix in the objective function.

#' The default action if "mean" and "StdDev" are specified as objectives with
#' optimize_method="ROI" is to maximize quadratic utility. If we want to maximize
#' Sharpe Ratio, we need to pass in maxSR=TRUE to optimize.portfolio.

maxSR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="ROI", 
                                   maxSR=TRUE, trace=TRUE)
maxSR.lo.ROI

#' Although the maximum Sharpe Ratio objective can be solved quickly and accurately
#' with optimize_method="ROI", it is also possible to solve this optimization
#' problem using other solvers such as random portfolios or DEoptim. These
#' solvers have the added flexibility of using different methods to calculate
#' the Sharpe Ratio (e.g. we could specify annualized measures of risk and return).

#' For random portfolios and DEoptim, the leverage constraints should be 
#' relaxed slightly.
init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

# Use random portfolios to run the optimization.
maxSR.lo.RP <- optimize.portfolio(R=R, portfolio=init.portf, 
                                  optimize_method="random",
                                  search_size=2000,
                                  trace=TRUE)
maxSR.lo.RP
chart.RiskReward(maxSR.lo.RP, risk.col="StdDev", return.col="mean")

# Use DEoptim to run the optimization.
maxSR.lo.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                  optimize_method="DEoptim",
                                  search_size=2000,
                                  trace=TRUE)
maxSR.lo.DE
chart.RiskReward(maxSR.lo.DE, risk.col="StdDev", return.col="mean")

