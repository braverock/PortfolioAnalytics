# demo/max_STARR.R

library(PortfolioAnalytics)

# Examples of solving optimization problems to maximize mean return per unit ES

data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES",
                            arguments=list(p=0.925))
init.portf

# Maximizing STARR Ratio can be formulated as a linear programming 
# problem and solved very quickly using optimize_method="ROI". 

# The default action if "mean" and "ES" are specified as objectives with
# optimize_method="ROI" is to maximize STARR. If we want to use
# both mean and ES in the objective function, but only minimize ES, we need to 
# pass in maxSTARR=FALSE to optimize.portfolio.

maxSTARR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                      optimize_method="ROI",
                                      trace=TRUE)
maxSTARR.lo.ROI

# Although the maximum STARR Ratio objective can be solved quickly and accurately
# with optimize_method="ROI", it is also possible to solve this optimization
# problem using other solvers such as random portfolios or DEoptim. These
# solvers have the added flexibility of using different methods to calculate
# the Sharpe Ratio (e.g. we could specify annualized measures of risk and 
# return or use modified, guassian, or historical ES).

# For random portfolios and DEoptim, the leverage constraints should be 
# relaxed slightly.
init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

# Use random portfolios
maxSTARR.lo.RP <- optimize.portfolio(R=R, portfolio=init.portf, 
                                  optimize_method="random",
                                  search_size=2000,
                                  trace=TRUE)
maxSTARR.lo.RP

chart.RiskReward(maxSTARR.lo.RP, risk.col="ES", return.col="mean")

# Use DEoptim
maxSTARR.lo.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                     optimize_method="DEoptim",
                                     search_size=2000,
                                     trace=TRUE)
maxSTARR.lo.DE
chart.RiskReward(maxSTARR.lo.DE, risk.col="ES", return.col="mean",
                 xlim=c(0.01, 0.08), ylim=c(0.004,0.008))
