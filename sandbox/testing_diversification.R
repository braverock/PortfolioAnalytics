library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

init <- portfolio.spec(assets=funds)
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="long_only")
init <- add.constraint(portfolio=init, type="diversification", 
                       conc_aversion=1, enabled=FALSE)

minvar <- add.objective(portfolio=init, type="risk", name="var")

qu <- add.objective(portfolio=init, type="risk", name="var", risk_aversion=1e6)
qu <- add.objective(portfolio=qu, type="return", name="mean")

# minimum variance optimization with full investment and long only constraints
opt_mv <- optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI", trace=TRUE)

# minimum variance optimization with full investment, long only, and diversification constraints
minvar$constraints[[3]]$enabled=TRUE
minvar$constraints[[3]]$conc_aversion=0
opt_mv_div1 <- optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI", trace=TRUE)

# The concentration aversion parameter is zero so we should have the same
# result as opt_mv
all.equal(opt_mv$weights, opt_mv_div1$weights)

# Making the conc_aversion parameter high enough should result in an equal
# weight portfolio.
minvar$constraints[[3]]$conc_aversion=20
opt_mv_div2 <- optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI", trace=TRUE)

# Now using quadratic utility
opt_qu <- optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI", trace=TRUE)
# equal to the minvar portfolio to 4 significant digits
all.equal(signif(opt_qu$weights, 4), signif(opt_mv$weights, 4))

# both the risk aversion and concentration aversion parameters will have to be
# adjusted to result in an equal weight portfolio
qu$constraints[[3]]$enabled=TRUE
qu$constraints[[3]]$conc_aversion=1e6
qu$objectives[[1]]$risk_aversion=1
opt_mv_qu <- optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI", trace=TRUE)
opt_mv_qu$weights

