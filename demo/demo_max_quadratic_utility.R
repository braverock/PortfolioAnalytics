
library(PortfolioAnalytics)

# Examples of solving optimization problems to maximize quadratic utility

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
# Here we can set the risk_aversion parameter to control how much risk
# is penalized
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev",
                            risk_aversion=4)
init.portf

# Maximizing quadratic utility can be formulated as a quardratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.
maxQU.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                    optimize_method="ROI", trace=TRUE)
maxQU.lo.ROI

plot(maxQU.lo.ROI, risk.col="StdDev", main=expression("Long Only Max Quadratic Utility" ~ lambda ~"=0.25"))

# A risk aversion parameter that is very small, will effectively make the term
# that penalizes risk zero and approximates the maximum return. Note that the
# risk_aversion parameter must be non-zero
init.portf$objectives[[2]]$risk_aversion <- 1e-6

maxQU.maxret.ROI <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", trace=TRUE)
maxQU.maxret.ROI

plot(maxQU.maxret.ROI, risk.col="StdDev", main=expression("Long Only Max Quadratic Utility" ~ lambda ~"= 1e-6"))

# A risk aversion parameter that is very large will heavily penalize the risk 
# term in the objective function and approximates the minimum variance portfolio.
init.portf$objectives[[2]]$risk_aversion <- 1e6

maxQU.minvol.ROI <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", trace=TRUE)
maxQU.minvol.ROI

plot(maxQU.minvol.ROI, risk.col="StdDev", main=expression("Long Only Max Quadratic Utility" ~ lambda ~"= 1e6"))

