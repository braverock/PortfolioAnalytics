#' ---
#' title: "Constrained Optimization Demo"
#' ---

#' This script demonstrates how to set up and solve constrained optimization
#' problems. Note that this script using the pre version 0.8.3 syntax.

library(PortfolioAnalytics)
require(DEoptim)

#' Load the data
data(edhec)

#' Set up the constraints and objectives to define the optimization problem
constraints <- constraint(assets = colnames(edhec[, 1:10]), min = 0.01, 
                          max = 0.4, min_sum=0.99, max_sum=1.01, 
                          weight_seq = generatesequence())

constraints <- add.objective(constraints=constraints, 
                             type="return", 
                             name="mean")

constraints <- add.objective(constraints=constraints, 
                             type="risk_budget", 
                             name="ES", arguments=list(clean="boudt", p=0.95),
                             min_prisk=.05, 
                             max_prisk=.15,
                             target=0.05)

#' We'll use a search_size parameter of 1000 for this demo, but realistic 
#' portfolios will likely require search_size parameters much larger, the 
#' default is 20000 which is almost always large enough for any realistic 
#' portfolio and constraints, but will take substantially longer to run.

#' Look for a solution using both DEoptim and random portfolios
opt_out <- optimize.portfolio(R=edhec[,1:10], 
                              constraints=constraints, 
                              optimize_method="DEoptim", 
                              search_size=1000, 
                              trace=TRUE)

opt_out_random <- optimize.portfolio(R=edhec[,1:10], 
                                     constraints=constraints, 
                                     optimize_method="random", 
                                     search_size=1000, 
                                     trace=TRUE)

#' Optimize a portfolio that rebalances quarterly
opt_out_rebalancing <- optimize.portfolio.rebalancing(R=edhec[,1:10], 
                                                      constraints=constraints, 
                                                      optimize_method="random", 
                                                      search_size=1000, 
                                                      trace=FALSE, 
                                                      rebalance_on='quarters')

#' Extract the optimal weights at each rebalance date and compute the returns
rebalancing_weights <- extractWeights(opt_out_rebalancing)
rebalancing_returns <- Return.rebalancing(R=edhec,weights=rebalancing_weights)
charts.PerformanceSummary(rebalancing_returns)

#' Optimize a portfolio that rebalances quarterly with 48 month trailing
opt_out_trailing <- optimize.portfolio.rebalancing(R=edhec[,1:10], 
                                                 constraints=constraints, 
                                                   optimize_method="random", 
                                                   search_size=1000, 
                                                   trace=FALSE,
                                                   rebalance_on='quarters',
                                                   trailing_periods=48,
                                                   training_period=48)
