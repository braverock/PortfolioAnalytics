library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)

data(edhec)
ret <- edhec[, 1:4]

# Create portfolio object
pspec <- portfolio.spec(assets=colnames(ret))
# Leverage constraint
lev_constr <- weight_sum_constraint(min_sum=1, max_sum=1)
# box constraint
lo_constr <- box_constraint(assets=pspec$assets, min=c(0.01, 0.02, 0.03, 0.04), max=0.65)
# group constraint
grp_constr <- group_constraint(assets=pspec$assets, groups=list(1:2, 3, 4), group_min=0.1, group_max=0.4)
# position limit constraint
pl_constr <- position_limit_constraint(assets=pspec$assets, max_pos=4)

# Make up a B matrix for an industry factor model
# dummyA, dummyB, and dummyC could be industries, sectors, etc.
B <- cbind(c(1, 1, 0, 0),
           c(0, 0, 1, 0),
           c(0, 0, 0, 1))
rownames(B) <- colnames(ret)
colnames(B) <- c("dummyA", "dummyB", "dummyC")
print(B)
lower <- c(0.1, 0.1, 0.1)
upper <- c(0.4, 0.4, 0.4)

# Industry exposure constraint
# The exposure constraint and group constraint are equivalent to test that they
# result in the same solution
exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=B, lower=lower, upper=upper)

# objective to minimize variance 
var_obj <- portfolio_risk_objective(name="var")
# objective to maximize return
ret_obj <- return_objective(name="mean")
# objective to minimize ETL
etl_obj <- portfolio_risk_objective(name="ETL")

# group constraint and exposure constraint should result in same solution

##### minimize var objective #####
opta <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(var_obj), 
                          optimize_method="ROI")
opta

optb <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, exp_constr), 
                          objectives=list(var_obj), 
                          optimize_method="ROI")
optb

all.equal(opta$weights, optb$weights)

##### maximize return objective #####
optc <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(ret_obj), 
                          optimize_method="ROI")
optc

optd <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="ROI")
optd

all.equal(optc$weights, optd$weights)

##### minimize ETL objective #####
opte <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, grp_constr), 
                          objectives=list(etl_obj), 
                          optimize_method="ROI")
opte

optf <- optimize.portfolio(R=ret, portfolio=pspec, 
                          constraints=list(lev_constr, lo_constr, exp_constr), 
                          objectives=list(etl_obj), 
                          optimize_method="ROI")
optf

all.equal(opte$weights, optf$weights)

##### maximize return objective with DEoptim #####
set.seed(123)
optde1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)
optde1

set.seed(123)
optde2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="DEoptim", 
                           search_size=2000, 
                           trace=TRUE)
optde2

all.equal(optde1$weights, optde2$weights)

##### maximize return objective with random #####
optrp1 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, grp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)
optrp1

optrp2 <- optimize.portfolio(R=ret, portfolio=pspec, 
                           constraints=list(lev_constr, lo_constr, exp_constr), 
                           objectives=list(ret_obj), 
                           optimize_method="random", 
                           search_size=2000, 
                           trace=TRUE)
optrp2

all.equal(optrp1$weights, optrp2$weights)