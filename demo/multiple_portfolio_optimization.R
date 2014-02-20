
library(PortfolioAnalytics)

# Examples of passing a list portfolio objects to optimize.portfolio and
# optimize.portfolio.rebalancing

data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum",
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# Minimize portfolio standard deviation
minSD.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")

# Maximize mean return per unit portfolio standard deviation
meanSD.portf <- add.objective(portfolio=minSD.portf, type="return", name="mean")

# Minimize expected shortfall
minES.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

# Maximize mean return per unit portfolio expected shortfall
meanES.portf <- add.objective(portfolio=minES.portf, type="return", name="mean")

# Combine the portfolios
mult.portf <- combine.portfolios(list(minSD.portf, meanSD.portf, minES.portf, meanES.portf))
mult.portf

# run the optimization for mult.portf
mult.opt <- optimize.portfolio(R, mult.portf, optimize_method="random", 
                               search_size=2000, trace=TRUE, message = TRUE)

class(mult.opt)
mult.opt

# This combines the weights for each portfolio optimized
extractWeights(mult.opt)

# This combines the objective measures for each portfolio
extractObjectiveMeasures(mult.opt)

# For N portfolios, this returns a list of length N with the stats
# for each portfolio
opt.xtract <- extractStats(mult.opt)

# Run the rebalancing optimization for mult.portf
mult.opt.rebal <- optimize.portfolio.rebalancing(R, mult.portf, 
                                                 optimize_method="random", 
                                                 search_size=2000, 
                                                 trace=TRUE, 
                                                 message=TRUE,
                                                 rebalance_on="quarters", 
                                                 training_period=140)

class(mult.opt.rebal)
mult.opt.rebal

# For N portfolios, this returns a list of length N with the optimal weights
# at each rebalancing date
extractWeights(mult.opt.rebal)

# For N portfolios, this returns a list of length N with the objective measures
# at each rebalancing date
extractObjectiveMeasures(mult.opt.rebal)

# For N portfolios, this returns a list of length N with the stats
# for each portfolio
opt.rebal.xtract <- extractStats(mult.opt.rebal)


