#' ---
#' title: "Risk Budget Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained portfolio optimization 
#' problem with risk budget objectives.

#' Load the package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="leverage", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

#' Portfolio optimization problems with risk budget objectives must be solved
#' with DEoptim, random portfolios, pso, or GenSA.

#' Risk budget objectives can be used to place limits on component contribution
#' to risk or for equal risk contribution portfolios. Note that there are 
#' potentially many portfolios that satisfy component ES risk limits so we need
#' to have a "sub" objective such as maximizing return, minimizing ES, 
#' minimizing StdDev, etc.

#' Add objective to maximize mean with limit on component ES risk contribution.
#' The max_prisk controls the maximum percentage contribution to risk.
rbES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
rbES.portf <- add.objective(portfolio=rbES.portf, type="risk_budget", name="ES",
                            max_prisk=0.4, arguments=list(p=0.92))

#' Run optimization with DEoptim as the optimization engine.
rbES.DE <- optimize.portfolio(R=R, portfolio=rbES.portf, 
                              optimize_method="DEoptim", 
                              search_size=2000, trace=TRUE)
rbES.DE
plot(rbES.DE, xlim=c(0, 0.08), ylim=c(0, 0.01))
chart.RiskBudget(rbES.DE, risk.type="pct_contrib")

#' Add objective to maximize mean return with equal ES risk contribution.
eqES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
eqES.portf <- add.objective(portfolio=eqES.portf, type="risk_budget", 
                            name="ES", min_concentration=TRUE, 
                            arguments=list(p=0.9, clean="boudt"),
                            multiplier=10)

#' Run optimization with random portfolios as the optimization engine.
#' Use cleaned returns
R.clean <- Return.clean(R=R, method="boudt")
eqES.RP <- optimize.portfolio(R=R.clean, portfolio=eqES.portf,
                              optimize_method="random",
                              search_size=2000, trace=TRUE)

eqES.RP
plot(eqES.RP)
chart.RiskBudget(eqES.RP, risk.type="pct_contrib")

#' Add objective to maximize mean return with limits on StdDev risk contribution.
rbStdDev.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
rbStdDev.portf <- add.objective(portfolio=rbStdDev.portf, type="risk_budget",
                                name="StdDev", max_prisk=0.25)

#' Run optimization with DEoptim as the optimization engine.
rbStdDev.DE <- optimize.portfolio(R=R.clean, portfolio=rbStdDev.portf,
                                  optimize_method="DEoptim",
                                  search_size=2000, trace=TRUE)

rbStdDev.DE
plot(rbStdDev.DE, risk.col="StdDev", xlim=c(0, 0.035), ylim=c(0, 0.01))
chart.RiskBudget(rbStdDev.DE, risk.type="pct_contrib")

#' Generate random portfolios.
rp <- random_portfolios(init.portf, 5000)

#' Run out of sample backtest with yearly rebalancing.
SDRB.opt.bt <- optimize.portfolio.rebalancing(R, SDRB.portf,
                                              optimize_method="random", 
                                              rp=rp,
                                              trace=TRUE, 
                                              rebalance_on="years", 
                                              training_period=100, 
                                              trailing_periods=60)

#' Here we demonstrate print and summary methods for 
#' optimize.portfolio.rebalancing objects.
SDRB.opt.bt

tmp_summary <- summary(SDRB.opt.bt)
names(tmp_summary)
tmp_summary

#' Here we show several extractor functions for 
#' summary.optimize.portfolio.rebalancing and optimize.portfolio.rebalancing
#'  to extract useful data.
extractWeights(tmp_summary)
extractObjectiveMeasures(tmp_summary)

tmp_stats <- extractStats(SDRB.opt.bt)
head(tmp_stats[[1]])
tmp_weights <- extractWeights(SDRB.opt.bt)
tmp_obj <- extractObjectiveMeasures(SDRB.opt.bt)

#' Chart the weights and risk budgets over time for the 
#' optimize.portfolio.rebalancing object.
chart.Weights(SDRB.opt.bt)
chart.RiskBudget(SDRB.opt.bt, match.col="StdDev", risk.type="percent")
