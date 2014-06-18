
# Demonstrate multi layer portfolio optimization
# The top level (i.e. layer) optimization problem is to minimize modified ES
# with equal component contribution to modified ES of the two portfolios in 
# the lower layer.
# The sub portfolios consist of different assets and different objectives
# relative to each other. The out of sample returns for each sub portfolio
# are calculated based on their respective constraints, objectives, and
# optimization parameters. The out of sample returns are then used as the
# returns input for the top level optimization.

library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# The first sub-portfolio, portf1, will contain assets 1:5 of edhec
# with an objective to minimize standard deviation.
portf1 <- portfolio.spec(assets=funds[1:5])
portf1 <- add.constraint(portfolio=portf1, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf1 <- add.constraint(portfolio=portf1, type="long_only")
portf1 <- add.objective(portfolio=portf1, type="risk", name="StdDev")

# The second sub-portfolio, portf2, will contain assets 6:10 of edhec
# with an objective to minimize expected shortfall.
portf2 <- portfolio.spec(assets=funds[6:10])
# portf2 <- portfolio.spec(assets=5)
portf2 <- add.constraint(portfolio=portf2, type="weight_sum", 
                         min_sum=0.99, max_sum=1.01)
portf2 <- add.constraint(portfolio=portf2, type="long_only")
portf2 <- add.objective(portfolio=portf2, type="risk", name="ES",
                            arguments=list(p=0.9))

# portf1 and portf2 have the same constraints so they can used the same
# set of random portfolios
set.seed(123)
rp <- random_portfolios(portf2, 2000)


# The 'top level' portfolio has objectives for equal contribution to risk
# where modified ES is the risk measure
portf <- portfolio.spec(assets=paste("proxy",1:2, sep="."))
portf <- add.constraint(portfolio=portf, type="weight_sum", 
                        min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portfolio=portf, type="long_only")
portf <- add.objective(portfolio=portf, type="risk", name="ES",
                        arguments=list(p=0.9))
portf <- add.objective(portfolio=portf, type="risk_budget", name="ES",
                       arguments=list(p=0.9), min_concentration=TRUE)

# Specify a mult-layer portfolio
mult.portf <- mult.portfolio.spec(portf)

# Add portf1 as a sub portfolio with optimization parameters specific to 
# running optimize.portfolio.rebalancing with portf1
mult.portf <- add.sub.portfolio(mult.portf, portf1, rp=rp, 
                                optimize_method="random",
                                rebalance_on="quarters", 
                                training_period=136)

# Add portf2 as a sub portfolio with optimization parameters specific to 
# running optimize.portfolio.rebalancing with portf2
mult.portf <- add.sub.portfolio(mult.portf, portf2, rp=rp,
                                optimize_method="random",
                                rebalance_on="months", 
                                training_period=136, 
                                trailing_periods=48)

# Generate random portfolios for the top layer optimization
set.seed(123)
rp.top <- random_portfolios(portf, 1000)

# Run the multi layer optimization
opt.mult <- optimize.portfolio(R, mult.portf,
                               optimize_method="random", 
                               trace=TRUE, rp=rp.top)

