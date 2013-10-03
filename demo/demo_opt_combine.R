library(PortfolioAnalytics)
library(DEoptim)
data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)

# Test different portfolios to test combining optimizations

# Add some basic constraints
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="leverage", min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# Objective to maximize portfolio mean return per unit ES
MeanES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
MeanES.portf <- add.objective(portfolio=MeanES.portf, type="risk", name="ES")

# Objective to maximize mean with risk budget percent contribution limit
MeanSD.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
MeanSD.portf <- add.objective(portfolio=MeanSD.portf, type="risk_budget", name="StdDev", max_prisk=0.35)

# Objective to minimize portfolio expected shortfall with equal ES component contribution
ESRB.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")
ESRB.portf <- add.objective(portfolio=ESRB.portf, type="risk_budget", name="ES", min_concentration=TRUE)


# MeanES optimization
MeanES.opt <- optimize.portfolio(R=R, portfolio=MeanES.portf, optimize_method="DEoptim", search_size=2000,, trace=TRUE)

# MeanSD optimization
MeanSD.opt <- optimize.portfolio(R=R, portfolio=MeanSD.portf, optimize_method="DEoptim", search_size=2000, trace=TRUE)

# ESRB optimization
ESRB.opt <- optimize.portfolio(R=R, portfolio=ESRB.portf, optimize_method="DEoptim", search_size=2000, trace=TRUE)

# Combine the optimizations
opt <- combine.optimizations(list(MeanES=MeanES.opt, MeanSD=MeanSD.opt, ESRB=ESRB.opt))

# Extract the objective measures from each optimize.portfolio object evaluated at all objectives
obj <- extractObjectiveMeasures(opt)

# Extract the optimal weights from each optimize.portfolio object
weights <- extractWeights(opt)

# Chart the risk contributions for StdDev and ES
chart.RiskBudget(opt, match.col="StdDev", risk.type="percentage", ylim=c(0,1), legend.loc="topright")
chart.RiskBudget(opt, match.col="ES", risk.type="percentage", ylim=c(-0.2,1), legend.loc="topright")
chart.RiskBudget(opt, match.col="ES", risk.type="percentage", plot.type="bar", ylim=c(-0.2,1), legend.loc="topright")

# Chart the optimal weights from each optimize.portfolio object
chart.Weights(opt, ylim=c(0,1))
chart.Weights(opt, plot.type="bar", ylim=c(0,1))

# Chart the optimal portfolios in risk-reward space
chart.RiskReward(opt, main="Optimal Portfolios")
chart.RiskReward(opt, risk.col="StdDev", main="Optimal Portfolios")


