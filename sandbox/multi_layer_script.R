library(PortfolioAnalytics)

# Script for multilayer optimization

# We need to support the different arguments/parameters for 
# optimize.portfolio.rebalancing for each sub-portfolio
# * R
# * optimize_method
# * search_size
# * trace
# * ...
# * rp
# * rebalance_on
# * training_period
# * trailings_period

# The returns need to have the same periodicity

# Each sub-portfolio may have a different rebalancing frequency, training, and
# trailing parameters, as well as optimization method

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# The first sub-portfolio, portf1, will contain assets 1:5 of the edhec
# with an objective to minimize standard deviation.
portf1 <- portfolio.spec(assets=funds[1:5])
portf1 <- add.constraint(portfolio=portf1, type="full_investment")
portf1 <- add.constraint(portfolio=portf1, type="long_only")
portf1 <- add.objective(portfolio=portf1, type="risk", name="StdDev")

# The second sub-portfolio, portf2, will contain assets 6:10 of the edhec
# with an objective to minimize expected shortfall.
portf2 <- portfolio.spec(assets=funds[6:10])
portf2 <- add.constraint(portfolio=portf2, type="full_investment")
portf2 <- add.constraint(portfolio=portf2, type="long_only")
portf2 <- add.objective(portfolio=portf2, type="risk", name="ES",
                            arguments=list(p=0.9))

# Run optimize.portfolio.rebalancing for each sub-portfolio to get proxy 
# returns 
proxy1 <- optimize.portfolio.rebalancing(R[,1:5], 
                                         portf1, 
                                         optimize_method="ROI", 
                                         rebalance_on="quarters", 
                                         training_period=60)
proxy1
proxy1.ret <- summary(proxy1)$portfolio_returns

proxy2 <- optimize.portfolio.rebalancing(R[,6:10], 
                                         portf2, 
                                         optimize_method="ROI", 
                                         rebalance_on="quarters", 
                                         training_period=48)
proxy2
proxy2.ret <- summary(proxy2)$portfolio_returns

# A different training period was used so the returns do not exactly align
ret <- cbind(proxy1.ret, proxy2.ret)
head(ret, 14)

# Get rid of the NAs
ret <- na.omit(ret)
colnames(ret) <- c("proxy1", "proxy2")
head(ret)

# Construct portfolio for the top level optimization of the proxy portfolios
portf <- portfolio.spec(assets=colnames(ret))
portf <- add.constraint(portfolio=portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portfolio=portf, type="long_only")
portf <- add.objective(portfolio=portf, type="risk", name="ES",
                        arguments=list(p=0.9))
portf <- add.objective(portfolio=portf, type="risk_budget", name="ES",
                       arguments=list(p=0.9), min_concentration=TRUE)

opt <- optimize.portfolio(ret, portf, 
                          optimize_method="random", 
                          search_size=4000, 
                          trace=TRUE)
opt

opt.bt <- optimize.portfolio.rebalancing(ret, portf, 
                                      optimize_method="random", 
                                      search_size=4000, 
                                      trace=TRUE,
                                      rebalance_on="months",
                                      training_period=48)
opt.bt
opt.ret <- summary(opt.bt)$portfolio_returns
charts.PerformanceSummary(cbind(opt.ret, ret))
