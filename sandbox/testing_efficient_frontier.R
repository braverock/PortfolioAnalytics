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
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="b")
chart.Weights.EF(meanvar.ef, colorset=bluemono, match.col="StdDev")

# run optimize.portfolio and chart the efficient frontier for that object
opt_meanvar <- optimize.portfolio(R=R, portfolio=meanvar.portf, optimize_method="ROI", trace=TRUE)
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=50)
# The weights along the efficient frontier can be plotted by passing in the
# optimize.portfolio output object
chart.Weights.EF(opt_meanvar, match.col="StdDev")
# or we can extract the efficient frontier and then plot it
ef <- extractEfficientFrontier(object=opt_meanvar, match.col="StdDev", n.portfolios=15)
chart.Weights.EF(ef, match.col="var", colorset=bluemono)

# mean-etl efficient frontier
meanetl.ef <- create.EfficientFrontier(R=R, portfolio=meanetl.portf, type="mean-ES")
chart.EfficientFrontier(meanetl.ef, match.col="ES", main="mean-ETL Efficient Frontier", type="l", col="blue")
chart.Weights.EF(meanetl.ef, colorset=bluemono, match.col="ES")

# mean-etl efficient frontier using random portfolios
meanetl.rp.ef <- create.EfficientFrontier(R=R, portfolio=meanetl.portf, type="random", match.col="ES")
chart.EfficientFrontier(meanetl.rp.ef, match.col="ES", main="mean-ETL RP Efficient Frontier", type="l", col="blue")
chart.Weights.EF(meanetl.rp.ef, colorset=bluemono, match.col="ES")


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
                              groups=c(2, 3), 
                              group_min=c(0.25, 0.15), 
                              group_max=c(0.75, 0.55))
group.portf <- add.constraint(portfolio=group.portf, type="long_only")
# optimize.portfolio(R=R, portfolio=group.portf, optimize_method="ROI")

portf.list <- list(lo.portf, box.portf, group.portf)
legend.labels <- c("Long Only", "Box", "Group + Long Only")
chart.EfficientFrontierOverlay(R=R, portfolio_list=portf.list, type="mean-StdDev", match.col="StdDev", 
                               legend.loc="right", legend.labels=legend.labels)

# TODO add the following methods for objects of class efficient.frontier
# - print
# - summary
