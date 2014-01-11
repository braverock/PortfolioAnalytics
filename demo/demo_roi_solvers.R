
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)

data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

# Set up initial portfolio with basic constraints
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# Add objectives
maxret.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
mines.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")
minsd.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
qu.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev", 
                          risk_aversion=0.25)
qu.portf <- add.objective(portfolio=qu.portf, type="return", name="mean")

# Maximize return
opt.maxret.roi <- optimize.portfolio(R, maxret.portf, optimize_method="ROI")
opt.maxret.glpk <- optimize.portfolio(R, maxret.portf, optimize_method="glpk")
opt.maxret.symphony <- optimize.portfolio(R, maxret.portf, optimize_method="symphony")
all.equal(extractStats(opt.maxret.roi), extractStats(opt.maxret.glpk))
all.equal(extractStats(opt.maxret.roi), extractStats(opt.maxret.symphony))
# This fails because an optimization problem with a linear objective cannot
# be solved with a quadratic programming solver
# opt.maxret.qp <- optimize.portfolio(R, maxret.portf, optimize_method="quadprog")

# Minimize ES
opt.mines.roi <- optimize.portfolio(R, mines.portf, optimize_method="ROI")
opt.mines.glpk <- optimize.portfolio(R, mines.portf, optimize_method="glpk")
opt.mines.symphony <- optimize.portfolio(R, mines.portf, optimize_method="symphony")
all.equal(extractStats(opt.mines.roi), extractStats(opt.mines.glpk))
all.equal(extractStats(opt.mines.roi), extractStats(opt.mines.symphony))

# Minimize standard deviation
opt.minsd.roi <- optimize.portfolio(R, minsd.portf, optimize_method="ROI")
opt.minsd.qp <- optimize.portfolio(R, minsd.portf, optimize_method="quadprog")
all.equal(extractStats(opt.minsd.roi), extractStats(opt.minsd.qp))
# This fails because an optimization problem with a quadratic objective cannot
# be solved with a linear programming solver
# opt.minsd.glpk <- optimize.portfolio(R, minsd.portf, optimize_method="glpk")

# Maximize quadratic utility
opt.qu.roi <- optimize.portfolio(R, qu.portf, optimize_method="ROI")
opt.qu.qp <- optimize.portfolio(R, qu.portf, optimize_method="quadprog")
all.equal(extractStats(opt.qu.roi), extractStats(opt.qu.qp))
