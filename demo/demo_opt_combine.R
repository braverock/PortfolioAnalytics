
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

# We should be able to compare portfolios with different constraints, 
# objectives, and number of assets

data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

##### Construct Portfolios #####
# GMV long only
port.gmv.lo <- portfolio.spec(assets=funds)
port.gmv.lo <- add.constraint(portfolio=port.gmv.lo, type="full_investment")
port.gmv.lo <- add.constraint(portfolio=port.gmv.lo, type="long_only")
port.gmv.lo <- add.objective(portfolio=port.gmv.lo, type="risk", name="var")

# GMV with shorting
port.gmv.short <- portfolio.spec(assets=funds)
port.gmv.short <- add.constraint(portfolio=port.gmv.short, type="full_investment")
port.gmv.short <- add.constraint(portfolio=port.gmv.short, type="box", min=-0.3, max=1)
port.gmv.short <- add.objective(portfolio=port.gmv.short, type="risk", name="var")

# QU box constraints
port.qu <- portfolio.spec(assets=funds)
port.qu <- add.constraint(portfolio=port.qu, type="full_investment")
port.qu <- add.constraint(portfolio=port.qu, type="box", min=0.05, max=0.6)
port.qu <- add.objective(portfolio=port.qu, type="risk", name="var", risk_aversion=0.25)
port.qu <- add.objective(portfolio=port.qu, type="return", name="mean")

##### Run Optimizations #####
opt.gmv.lo <- optimize.portfolio(R=R, portfolio=port.gmv.lo, optimize_method="ROI", trace=TRUE)
opt.gmv.short <- optimize.portfolio(R=R, portfolio=port.gmv.short, optimize_method="ROI", trace=TRUE)
opt.qu <- optimize.portfolio(R=R, portfolio=port.qu, optimize_method="ROI", trace=TRUE)


opt <- combine.optimizations(list(GMV.LO=opt.gmv.lo, GMV.SHORT=opt.gmv.short, QU=opt.qu))
class(opt)

chart.Weights(opt, legend.loc="topleft", cex.legend=0.8, ylim=c(-0.3, 1))

chart.Weights(opt, plot.type="bar", cex.lab=0.8, legend.loc="topleft", cex.legend=0.8, ylim=c(-0.3, 1))

extractWeights(opt)
