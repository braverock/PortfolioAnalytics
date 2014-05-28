library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED")
funds <- colnames(R)

# create an xts object of regimes
# Here I just randomly samples values to create regime 1 or regime 2. In 
# practice, this could based on volatility or other regime switching models
set.seed(123)
regime <- xts(sample(1:2, nrow(R), replace=TRUE, prob=c(0.3, 0.7)), index(R))

# Portfolio for regime 1
port1 <- portfolio.spec(funds)
port1 <- add.constraint(port1, "weight_sum", min_sum=0.99, max_sum=1.01)
port1 <- add.constraint(port1, "box", min=0.1, max=0.5)
port1 <- add.objective(port1, type="risk", name="ES", arguments=list(p=0.9))
port1 <- add.objective(port1, type="risk_budget", name="ES", 
                       arguments=list(p=0.9), max_prisk=0.5)

# Portfolio for regime 2
port2 <- portfolio.spec(funds)
port2 <- add.constraint(port2, "weight_sum", min_sum=0.99, max_sum=1.01)
port2 <- add.constraint(port2, "box", min=0, max=0.6)
port2 <- add.objective(port2, type="risk", name="StdDev")
port2 <- add.objective(port2, type="risk_budget", name="StdDev", max_prisk=0.5)

# Combine the portfolios
portfolios <- combine.portfolios(list(port1, port2))

# with the regime and portfolios
regime.port <- regime.portfolios(regime, portfolios)
regime.port

# Should result in portfolio for regime 2
opt1 <- optimize.portfolio(R, regime.port, 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)
opt1
opt1$regime

# Should result in portfolio for regime 1
opt2 <- optimize.portfolio(R[1:(nrow(R)-1)], regime.port, 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)
opt2
opt2$regime

# For optimize_method="random", which portfolio do we use and how do we 
# generate random portfolios
# - prompt the user to generate random portfolios?
# - use the first portfolio?
# - specify which portfolio?
# This is important because the constraints may be very different
# This only impacts optimize.portfolio.rebalancing because we do not know
# the portfolio specification at the time we generate the random portfolios
opt.rebal <- optimize.portfolio.rebalancing(R, regime.port,
                                            optimize_method="DEoptim", 
                                            rebalance_on="quarters", 
                                            training_period=130,
                                            search_size=2000, 
                                            trace=TRUE)

# The print and summary methods work the same as they do for optimizations 
# without regime switching
opt.rebal
summary(opt.rebal)

# Get the regime at each rebalance date
lapply(opt.rebal$opt_rebalancing, function(x) x$regime)

# Extract the weights
wt <- extractWeights(opt.rebal)
wt

# Extract the objective measures*
obj <- extractObjectiveMeasures(opt.rebal)
str(obj)
obj

# Extract the stats*
xt <- extractStats(opt.rebal)
str(xt)

# *
# Note that this returns a list of N elements for N regimes. We may have 
# different objectives and/or a different number of objectives which makes
# returning a single xts object difficult

chart.Weights(opt.rebal, colorset=bluemono)

# Chart the risk contribution for regime 1
chart.RiskBudget(opt.rebal, match.col="ES", risk.type="percentage", 
                 regime=1, colorset=bluemono)

# Chart the risk contribution for regime 2
chart.RiskBudget(opt.rebal, match.col="StdDev", risk.type="percentage", 
                 regime=2, colorset=bluemono)

