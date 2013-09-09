library(PortfolioAnalytics)
library(foreach)
library(iterators)
library(quadprog)
library(Rglpk)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

##### Method 1 #####
# Set up portfolio object with constraints and objectives to maximize return
# using the portfolio object to add constraints and objectives
pspec1 <- portfolio.spec(assets=funds)
pspec1 <- add.constraint(portfolio=pspec1, type="full_investment")
pspec1 <- add.constraint(portfolio=pspec1, type="box", min=0, max=0.65)
pspec1 <- add.objective(portfolio=pspec1, type="return", name="mean")

opt1 <- optimize.portfolio(R=ret, portfolio=pspec1, optimize_method="ROI")

##### Method 2 #####
# Set up portfolio object with constraints and objective to maximize return
# using separate constraint and objective objects
pspec2 <- portfolio.spec(assets=funds)
weight_constr <- weight_sum_constraint(min_sum=1, max_sum=1)
box_constr <- box_constraint(assets=pspec2$assets, min=0, max=0.65)
ret_obj <- return_objective(name="mean")
cset <- list(weight_constr, box_constr)
obj <- list(ret_obj)

opt2 <- optimize.portfolio(R=ret, portfolio=pspec2, constraints=cset, 
                           objectives=obj, optimize_method="ROI")

all.equal(extractWeights(opt1), extractWeights(opt2))

##### Method 1 Backtesting #####
opt_rebal1 <- optimize.portfolio.rebalancing(R=ret, portfolio=pspec1, 
                                             optimize_method="ROI", 
                                             rebalance_on="months")
class(opt_rebal1)
inherits(opt_rebal1, "optimize.portfolio.rebalancing")
wts1 <- extractWeights(opt_rebal1)

##### Method 2 Backtesting #####
opt_rebal2 <- optimize.portfolio.rebalancing(R=ret, portfolio=pspec2, 
                                             constraints=cset, 
                                             objectives=obj,
                                             optimize_method="ROI", 
                                             rebalance_on="months")
wts2 <- extractWeights(opt_rebal2)
all.equal(wts1, wts2)

