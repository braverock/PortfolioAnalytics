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

# Maximize sharpe ratio
sharpe.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
sharpe.portf <- add.objective(portfolio=sharpe.portf, type="return", name="mean")

# Optimization to maximize Sharpe Ratio
max_sharpe_opt <- optimize.portfolio(R=R, portfolio=sharpe.portf, optimize_method="ROI", maxSR=TRUE)
max_sharpe_opt

# calculate sharpe ratio from efficient frontier and optimization
ef1 <- create.EfficientFrontier(R=R, portfolio=sharpe.portf, type="mean-sd", n.portfolios=100)
max(ef1$frontier[,"mean"] / ef1$frontier[,"StdDev"])
max_sharpe_opt$objective_measures$mean / max_sharpe_opt$objective_measures$StdDev

# Maximize starr
starr.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")
starr.portf <- add.objective(portfolio=starr.portf, type="return", name="mean")

max_starr_opt <- optimize.portfolio(R=R, portfolio=starr.portf, optimize_method="ROI", maxSTARR=TRUE)
max_starr_opt

# calculate starr ratio from efficient frontier and optimization
ef2 <- create.EfficientFrontier(R=R, portfolio=starr.portf, type="mean-ES", n.portfolios=100)
max(ef2$frontier[,"mean"] / ef2$frontier[,"ES"])
max_starr_opt$objective_measures$mean / max_starr_opt$objective_measures$ES

