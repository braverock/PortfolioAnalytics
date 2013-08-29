# Script to test efficient frontiers

# Efficient frontiers can be plotted two ways
# 1. Run optimize.portfolio with trace=TRUE and then chart that object
# 2. create an efficient frontier and then chart that object

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)

rm(list=ls())

data(edhec)
R <- edhec[, 1:5]
# change the column names for better legends in plotting
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

# initial portfolio object
init <- portfolio.spec(assets=funds)
# initial constraints
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="box", min=0, max=1)

# initial objective
init <- add.objective(portfolio=init, type="return", name="mean")

# create mean-etl portfolio
meanetl.portf <- add.objective(portfolio=init, type="risk", name="ES")

# create mean-var portfolio
meanvar.portf <- add.objective(portfolio=init, type="risk", name="var", risk_aversion=1e6)

# create efficient frontiers

# mean-var efficient frontier
meanvar.ef <- create.EfficientFrontier(R=R, portfolio=meanvar.portf, type="mean-StdDev")
print(meanvar.ef)
summary(meanvar.ef, digits=2)
meanvar.ef$frontier
# The RAR.text argument can be used for the risk-adjusted-return name on the legend,
# by default it is 'Modified Sharpe Ratio'
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", RAR.text="Sharpe Ratio", pch=4)
# The tangency portfolio and line are plotted by default, these can be ommitted
# by setting rf=NULL
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", rf=NULL)
chart.Weights.EF(meanvar.ef, colorset=bluemono, match.col="StdDev")

# If you have a lot of assets and they don't fit with the default legend, you
# can set legend.loc=NULL and customize the plot.
par(mar=c(8, 4, 4, 2)+0.1, xpd=TRUE)
chart.Weights.EF(meanvar.ef, colorset=bluemono, match.col="StdDev", legend.loc=NULL)
legend("bottom", legend=colnames(R), inset=-1, fill=bluemono, bty="n", ncol=3, cex=0.8)
par(mar=c(5, 4, 4, 2)+0.1, xpd=FALSE)

# run optimize.portfolio and chart the efficient frontier for that object
opt_meanvar <- optimize.portfolio(R=R, portfolio=meanvar.portf, optimize_method="ROI", trace=TRUE)

# The efficient frontier is created from the 'opt_meanvar' object by getting
# The portfolio and returns objects and then passing those to create.EfficientFrontier
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=25)

# Rerun the optimization with a new risk aversion parameter to change where the
# portfolio is along the efficient frontier. The 'optimal' portfolio plotted on
# the efficient frontier is the optimal portfolio returned by optimize.portfolio.
meanvar.portf$objectives[[2]]$risk_aversion=0.25
opt_meanvar <- optimize.portfolio(R=R, portfolio=meanvar.portf, optimize_method="ROI", trace=TRUE)
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=25)

# The weights along the efficient frontier can be plotted by passing in the
# optimize.portfolio output object
chart.Weights.EF(opt_meanvar, match.col="StdDev")

# Extract the efficient frontier and then plot it
# Note that if you want to do multiple charts of the efficient frontier from
# the optimize.portfolio object, it is best to extractEfficientFrontier as shown
# below
ef <- extractEfficientFrontier(object=opt_meanvar, match.col="StdDev", n.portfolios=15)
print(ef)
summary(ef, digits=5)
chart.Weights.EF(ef, match.col="StdDev", colorset=bluemono)

# mean-etl efficient frontier
meanetl.ef <- create.EfficientFrontier(R=R, portfolio=meanetl.portf, type="mean-ES")
print(meanetl.ef)
summary(meanetl.ef)
chart.EfficientFrontier(meanetl.ef, match.col="ES", main="mean-ETL Efficient Frontier", type="l", col="blue", RAR.text="STARR")
chart.Weights.EF(meanetl.ef, colorset=bluemono, match.col="ES")

# mean-etl efficient frontier using random portfolios
meanetl.rp.ef <- create.EfficientFrontier(R=R, portfolio=meanetl.portf, type="random", match.col="ES")
chart.EfficientFrontier(meanetl.rp.ef, match.col="ES", main="mean-ETL RP Efficient Frontier", type="l", col="blue", rf=0)
chart.Weights.EF(meanetl.rp.ef, colorset=bluemono, match.col="ES")

# mean-etl efficient frontier with optimize.portfolio output
opt_meanetl <- optimize.portfolio(R=R, portfolio=meanetl.portf, optimize_method="random", search_size=2000, trace=TRUE)
chart.EfficientFrontier(meanetl.rp.ef, match.col="ES", main="mean-ETL RP Efficient Frontier", type="l", col="blue", rf=0, RAR.text="STARR")

##### overlay efficient frontiers of multiple portfolios #####
# Create a mean-var efficient frontier for multiple portfolios and overlay the efficient frontier lines
# set up an initial portfolio with the full investment constraint and mean and var objectives
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="var")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

# long only constraints
lo.portf <- add.constraint(portfolio=init.portf, type="long_only")

# box constraints
box.portf <- add.constraint(portfolio=init.portf, type="box", min=0.05, max=0.65)

# group constraints (also add long only constraints to the group portfolio)
group.portf <- add.constraint(portfolio=init.portf, type="group", 
                              groups=list(groupA=c(1, 3),
                                          groupB=c(2, 4, 5)),
                              group_min=c(0.25, 0.15), 
                              group_max=c(0.75, 0.55))
group.portf <- add.constraint(portfolio=group.portf, type="long_only")
# optimize.portfolio(R=R, portfolio=group.portf, optimize_method="ROI")

portf.list <- list(lo.portf, box.portf, group.portf)
legend.labels <- c("Long Only", "Box", "Group + Long Only")
chart.EfficientFrontierOverlay(R=R, portfolio_list=portf.list, type="mean-StdDev", 
                               match.col="StdDev", legend.loc="topleft", 
                               legend.labels=legend.labels, cex.legend=0.6)

