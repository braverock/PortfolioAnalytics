
library(PortfolioAnalytics)
data(edhec)

# Use first four columns of edhec data set
R <- edhec[, 1:4]
funds <- colnames(R)

# Initialize portfolio and add basic constraints
init.portf <- portfolio.spec(funds, weight_seq=generatesequence(min=0, max=1, by=0.002))
init.portf <- add.constraint(init.portf, "weight_sum")#, min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(init.portf, "box", min=0, max=0.65)

# Add mean return objective with multiplier=0 so it is calculated, but does
# not affect optimization
init.portf <- add.objective(init.portf, type="return", name="mean", multiplier=0)

# Add objective to minimize portfolio standard deviation
SDRB.portf <- add.objective(init.portf, type="risk", name="StdDev")

# Add StdDev risk budget objective for maximum percentage risk 
SDRB.portf <- add.objective(SDRB.portf, type="risk_budget", name="StdDev", max_prisk=0.4)

# Generate random portfolios
rp <- random_portfolios(init.portf, 5000)

# Run out of sample backtest with yearly rebalancing
SDRB.opt.bt <- optimize.portfolio.rebalancing(R, SDRB.portf,
                                              optimize_method="random", 
                                              rp=rp,
                                              trace=TRUE, 
                                              rebalance_on="years", 
                                              training_period=100, 
                                              trailing_periods=60)

# print method for optimize.portfolio.rebalancing objects
SDRB.opt.bt

# summary method for optimize.portfolio.rebalancing objects
tmp_summary <- summary(SDRB.opt.bt)
names(tmp_summary)

# print method for summary.optimize.portfolio.rebalancing objects
tmp_summary

# Extractor functions for summary.optimize.portfolio.rebalancing objects
extractWeights(tmp_summary)
extractObjectiveMeasures(tmp_summary)

# Extractor functions for optimize.portfolio.rebalancing objects
tmp_stats <- extractStats(SDRB.opt.bt)
head(tmp_stats[[1]])
tmp_weights <- extractWeights(SDRB.opt.bt)
tmp_obj <- extractObjectiveMeasures(SDRB.opt.bt)

# chart functions for optimize.portfolio.rebalancing
chart.Weights(SDRB.opt.bt)
chart.RiskBudget(SDRB.opt.bt, match.col="StdDev", risk.type="percent")

