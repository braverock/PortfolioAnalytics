#' ---
#' title: "Sortino Demo"
#' author: Brian Peterson
#' date: "7/17/2014"
#' ---

#' Load the necessary packages
# Include optimizer and multi-core packages
library(PortfolioAnalytics)
library(PerformanceAnalytics)
require(xts)
require(DEoptim)
require(TTR)

#' Register parallel backend.
#' note: these may not be appropriate on Windows
require(doMC)
registerDoMC()
# for Windows
#require(doParallel)
#registerDoParallel()


#' Load the data. Here we use monthly total returns of four asset-class indexes.
data(indexes)
#only look at 2000 onward
#indexes<-indexes["2000::"]


#' Set the MAR parameter
MAR =.005 #~6%/year

#' Example 1 maximize Sortino Ratio
SortinoConstr <- constraint(assets = colnames(indexes[,1:4]), min = 0.05, max = 1, min_sum=.99, max_sum=1.01, weight_seq = generatesequence(by=.001))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="SortinoRatio",  enabled=TRUE, arguments = list(MAR=MAR))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="mean",  enabled=TRUE, multiplier=0) # multiplier 0 makes it availble for plotting, but not affect optimization

#' Use random portfolio engine
SortinoResult<-optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='random', search_size=2000, trace=TRUE, verbose=TRUE)
plot(SortinoResult, risk.col='SortinoRatio')

#' Alternately, Use DEoptim engine
#SortinoResultDE<-optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='DEoptim', search_size=2000, trace=TRUE, verbose=FALSE,strategy=6, parallel=TRUE) #itermax=55, CR=0.99, F=0.5,
#plot(SortinoResultDE, risk.col='SortinoRatio')

#' Now rebalance quarterly
SortinoRebalance <- optimize.portfolio.rebalancing(R=indexes[,1:4], constraints=SortinoConstr, optimize_method="random", trace=TRUE, rebalance_on='quarters', trailing_periods=NULL, training_period=36, search_size=2000)

###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################