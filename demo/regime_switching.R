library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED")
funds <- colnames(R)

# create an xts object of regimes
# Here I just randomly samples values to create regime 1 or regime 2. In 
# practice, this could based on volatility of other regime switching models
set.seed(123)
regime <- xts(sample(1:2, nrow(R), replace=TRUE), index(R))

# portfolio for regime 1
port1 <- portfolio.spec(funds)
port1 <- add.constraint(port1, "weight_sum", min_sum=0.99, max_sum=1.01)
port1 <- add.constraint(port1, "box", min=0.1, max=0.5)
port1 <- add.objective(port1, type="risk", name="ES", arguments=list(p=0.9))
port1 <- add.objective(port1, type="risk_budget", name="ES", 
                       arguments=list(p=0.9), max_prisk=0.5)

# portfolio for regime 2
port2 <- portfolio.spec(funds)
port2 <- add.constraint(port2, "weight_sum", min_sum=0.99, max_sum=1.01)
port2 <- add.constraint(port2, "box", min=0, max=0.6)
port2 <- add.objective(port2, type="risk", name="StdDev")
port2 <- add.objective(port2, type="risk_budget", name="StdDev", max_prisk=0.5)

portfolios <- combine.portfolios(list(port1, port2))

regime.port <- regime.portfolios(regime, portfolios)

# should result in portfolio for regime 1
opt1 <- optimize.portfolio(R, regime.port, 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)

# should result in portfolio for regime 2
opt2 <- optimize.portfolio(R[1:(nrow(R)-2)], regime.port, 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)

# For optimize_method="random", which portfolio do we use and how do we 
# generate random portfolios
# - prompt the user to generate random portfolios?
# - use the first portfolio?
# - specify which portfolio?
opt.rebal <- optimize.portfolio.rebalancing(R, regime.port,
                                            optimize_method="DEoptim", 
                                            rebalance_on="quarters", 
                                            training_period=130,
                                            search_size=2000, 
                                            trace=TRUE)

opt.rebal

summary(opt.rebal)

lapply(opt.rebal$opt_rebalancing, function(x) x$regime)

# Extract the weights
wt <- extractWeights(opt.rebal)
wt

# Extract the objective measures
obj <- extractObjectiveMeasures(opt.rebal)
str(obj)
obj

# Extract the stats
xt <- extractStats(opt.rebal)
str(xt)

chart.Weights(opt.rebal, colorset=bluemono)

chart.RiskBudget(opt.rebal, match.col="ES", risk.type="percentage", regime=1, colorset=bluemono)
chart.RiskBudget(opt.rebal, match.col="StdDev", risk.type="percentage", regime=2, colorset=bluemono)
