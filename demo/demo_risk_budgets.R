
library(PortfolioAnalytics)

# Examples of solving optimization problems using risk budget objectives

data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="leverage", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# Portfolio optimization problems with risk budget objectives must be solved
# with DEoptim, random portfolios, pso, or GenSA.

# Risk budget objectives can be used to place limits on component contribution
# to risk or for equal risk contribution portfolios. Note that there are 
# potentially many portfolios that satisfy component ES risk limits so we need
# to have a "sub" objective such as maximizing return, minimizing ES, 
# minimizing StdDev, etc.

# Add objective to maximize mean with limit on component ES risk contribution
# The max_prisk controls the maximum percentage contribution to risk
rbES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
rbES.portf <- add.objective(portfolio=rbES.portf, type="risk_budget", name="ES",
                            max_prisk=0.4, arguments=list(p=0.92))

# Use DEoptim for optimization
rbES.DE <- optimize.portfolio(R=R, portfolio=rbES.portf, 
                              optimize_method="DEoptim", 
                              search_size=5000, trace=TRUE)
print(rbES.DE)
plot(rbES.DE, xlim=c(0, 0.08), ylim=c(0, 0.01))
chart.RiskBudget(rbES.DE, risk.type="pct_contrib")

# Add objective to maximize mean return with equal ES risk contribution
eqES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
eqES.portf <- add.objective(portfolio=eqES.portf, type="risk_budget", 
                            name="ES", min_concentration=TRUE, arguments=list(p=0.9))

# Use random portfolios for optimization
# Use cleaned returns
R.clean <- Return.clean(R=R, method="boudt")
eqES.RP <- optimize.portfolio(R=R.clean, portfolio=eqES.portf,
                              optimize_method="random",
                              search_size=2500, trace=TRUE)

print(eqES.RP)
plot(eqES.RP)
chart.RiskBudget(eqES.RP, risk.type="pct_contrib")

# Add objective to maximize mean return with limits on StdDev risk contribution
rbStdDev.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
rbStdDev.portf <- add.objective(portfolio=rbStdDev.portf, type="risk_budget",
                                name="StdDev", max_prisk=0.25)

# Use DEoptim for optimization
rbStdDev.DE <- optimize.portfolio(R=R, portfolio=rbStdDev.portf,
                                  optimize_method="DEoptim",
                                  search_size=5000, trace=TRUE)

print(eqES.RP)
plot(eqES.RP)
chart.RiskBudget(eqES.RP, risk.type="pct_contrib")



