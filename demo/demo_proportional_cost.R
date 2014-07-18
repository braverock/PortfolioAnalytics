#' ---
#' title: "Proportional Cost Constraint Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
N <- 4
R <- edhec[, 1:N]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R)

#' Transaction costs are calculated using the optimal weights and initial set of weights.
#' The initial set of weights is specified in the portfolio object. In the case
#' below, the initial set of weights is an equally-weighted portfolio.

#' Set up initial portfolio specification object with basic constraints.
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")

#' Add the proportional transaction cost constraint.
pspec <- add.constraint(portfolio=pspec, type="transaction", ptc=0.01)

#' Add objective to minimize portfolio variance.
minvar <- add.objective(portfolio=pspec, type="risk", name="var")

#' Note that if a return target is not specified, the results may not make sense.
optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI")

#' Add a target return constraint
minvar <- add.constraint(portfolio=minvar, type="return", return_target=0.007)

#' Run the optimization.
optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI")

#' Add return and risk objective for quadratic utility.
#' Note that target return can be specified as a constraint or in the return 
#' objective as shown below.
qu <- add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=0.3)
qu <- add.objective(portfolio=qu, type="return", name="mean", target=0.007)
optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI")

#' Now use random portfolios for the optimization.
#' Set up portfolio with equally weighted portfolio for initial weights.
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="long_only")
#' Add the proportional transaction cost constraint.
pspec <- add.constraint(portfolio=pspec, type="transaction", ptc=0.01)

# There is no transaction cost, the penalty should be 0.
# constrained_objective(w=rep(1/4, 4), R=R, portfolio=pspec)
# wts <- c(0.2, 0.3, 0.25, 0.25)
# sum(abs(wts - pspec$assets)*pspec$constraints[[3]]$ptc)
# constrained_objective(w=wts, R=R, portfolio=pspec)

#' Add objective to minimize standard deviation
pspec <- add.objective(portfolio=pspec, type="risk", name="StdDev")

#' This pushes the optimal portfolio to the initial weights to limit 
#' transaction costs.
opt_rp <- optimize.portfolio(R=R, portfolio=pspec, optimize_method="random", 
                             search_size=2000, trace=TRUE)
opt_rp

#' Set up portfolio with initial weights w = {0.15 0.15 0.2 0.5}
pspec <- portfolio.spec(assets=c(0.15, 0.15, 0.2, 0.5))
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="long_only")
pspec <- add.constraint(portfolio=pspec, type="transaction", ptc=0.01)

# There is no transaction cost, the penalty should be 0.
# constrained_objective(w=rep(1/4, 4), R=R, portfolio=pspec)
# wts <- c(0.2, 0.3, 0.25, 0.25)
# sum(abs(wts - pspec$assets)*pspec$constraints[[3]]$ptc)
# constrained_objective(w=wts, R=R, portfolio=pspec)

#' Add objective to minimize standard deviation.
pspec <- add.objective(portfolio=pspec, type="risk", name="StdDev")

#' This pushes the optimal portfolio to the initial weights to limit
#' transaction costs.
opt_rp <- optimize.portfolio(R=R, portfolio=pspec, optimize_method="random", 
                             search_size=2000, trace=TRUE)
opt_rp
