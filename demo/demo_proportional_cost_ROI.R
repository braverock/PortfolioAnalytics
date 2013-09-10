library(PortfolioAnalytics)
library(quadprog)

data(edhec)
N <- 4
R <- edhec[, 1:N]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R)

# set up initial portfolio specification object
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")
pspec <- add.constraint(portfolio=pspec, type="transaction", ptc=0.01)

# add var objective to minimize portfolio variance
minvar <- add.objective(portfolio=pspec, type="risk", name="var")

# Note that if a return target is not specified, the results may not make sense
optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI")

# Add a target return constraint
minvar <- add.constraint(portfolio=minvar, type="return", return_target=0.007)
optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI")

# Add return and risk objective for quadratic utility
# Note that target return can be specified as a constraint or in the return 
# objective as shown below
qu <- add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=0.3)
qu <- add.objective(portfolio=qu, type="return", name="mean", target=0.007)
optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI")

