# ROI examples

# The following objectives can be solved with optimize_method=ROI
# maximize return
# minimum variance
# maximize quadratic utility
# minimize ETL

library(PortfolioAnalytics)
library(ROI)
library(Rglpk)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

# Load the returns data
data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# Create portfolio specification
pspec <- portfolio.spec(assets=funds)

##### Constraints #####
# Constraints will be specified as separate objects, but could also be added to
# the portfolio object (see the portfolio vignette for examples of specifying 
# constraints)

# Full investment constraint
fi_constr <- weight_sum_constraint(min_sum=1, max_sum=1)

# Long only constraint
lo_constr <- box_constraint(assets=pspec$assets, min=0, max=1)

# Box constraints
box_constr <- box_constraint(assets=pspec$assets, 
                             min=c(0.05, 0.04, 0.1, 0.03),
                             max=c(0.65, 0.45, 0.7, 0.6))

# Position limit constraint
pl_constr <- position_limit_constraint(assets=pspec$assets, max_pos=2)

# Target mean return constraint
ret_constr <- return_constraint(return_target=0.007)

# Group constraint
group_constr <- group_constraint(assets=pspec$assets, groups=list(1, 2:3, 4), 
                                 group_min=0, group_max=0.5)

# Factor exposure constraint
# Industry exposures are used in this example, but other factors could be used as well
# Note that exposures to industry factors are similar to group constraints
facexp_constr <- factor_exposure_constraint(assets=pspec$assets, 
                                            B=cbind(c(1, 0, 0, 0),
                                                    c(0, 1, 1, 0),
                                                    c(0, 0, 0, 1)), 
                                            lower=c(0.1, 0.15, 0.05), 
                                            upper=c(0.45, 0.65, 0.60))

##### Objectives #####
# Return objective
ret_obj <- return_objective(name="mean")

# Variance objective
var_obj <- portfolio_risk_objective(name="var")

# ETL objective
etl_obj <- portfolio_risk_objective(name="ETL")

##### Maximize Return Optimization #####
# The ROI solver uses the glpk plugin to interface to the Rglpk package for 
# objectives to maximimize mean return

# Full investment and long only constraints
opt_maxret <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr), 
                                 objectives=list(ret_obj), 
                                 optimize_method="ROI")
opt_maxret

# Full investment, box, and target return constraints
opt_maxret <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, box_constr, ret_constr), 
                                 objectives=list(ret_obj), 
                                 optimize_method="ROI")
opt_maxret

# Full investment, box, and position_limit constraints
opt_maxret <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, box_constr, pl_constr), 
                                 objectives=list(ret_obj), 
                                 optimize_method="ROI")
opt_maxret

# Full investment, box, and group constraints
opt_maxret <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, box_constr, group_constr), 
                                 objectives=list(ret_obj), 
                                 optimize_method="ROI")
opt_maxret

# Full investment, box, and factor exposure constraints
opt_maxret <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, box_constr, facexp_constr), 
                                 objectives=list(ret_obj), 
                                 optimize_method="ROI")
opt_maxret

##### Minimize Variance Optimization #####
# The ROI solver uses the quadprog plugin to interface to the quadprog package for 
# objectives to minimize variance

# Global minimum variance portfolio. Only specify the full investment constraint
opt_minvar <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr),
                                 objectives=list(var_obj),
                                 optimize_method="ROI")
opt_minvar

# Full investment, box, and target mean_return constraints
opt_minvar <- optimize.portfolio(R=ret, portfolio=pspec,
                                 constraints=list(fi_constr, box_constr, ret_constr),
                                 objectives=list(var_obj),
                                 optimize_method="ROI")
opt_minvar

# Full investment, box, and group constraints
opt_minvar <- optimize.portfolio(R=ret, portfolio=pspec,
                                 constraints=list(fi_constr, box_constr, group_constr),
                                 objectives=list(var_obj),
                                 optimize_method="ROI")
opt_minvar

# Full investment, box, and exposure constraints
opt_minvar <- optimize.portfolio(R=ret, portfolio=pspec,
                                 constraints=list(fi_constr, box_constr, facexp_constr),
                                 objectives=list(var_obj),
                                 optimize_method="ROI")
opt_minvar

##### Maximize Quadratic Utility Optimization #####
# The ROI solver uses the quadprog plugin to interface to the guadprog package for 
# objectives to maximimize quadratic utility

# Create the variance objective with a large risk_aversion paramater
# A large risk_aversion parameter will approximate the global minimum variance portfolio
var_obj <- portfolio_risk_objective(name="var", risk_aversion=1e4)

# Full investment and box constraints
opt_qu <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(fi_constr, box_constr),
                             objectives=list(ret_obj, var_obj),
                             optimize_method="ROI")
opt_qu

# Change the risk_aversion parameter in the variance objective to a small number
# A small risk_aversion parameter will approximate the maximum portfolio
var_obj$risk_aversion <- 1e-4

# Full investment and long only constraints
opt_qu <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(fi_constr, lo_constr),
                             objectives=list(ret_obj, var_obj),
                             optimize_method="ROI")
opt_qu

# Change the risk_aversion parameter to a more reasonable value
var_obj$risk_aversion <- 0.25
# Full investment, long only, and factor exposure constraints
opt_qu <- optimize.portfolio(R=ret, portfolio=pspec, 
                             constraints=list(fi_constr, lo_constr, facexp_constr),
                             objectives=list(ret_obj, var_obj),
                             optimize_method="ROI")
opt_qu

# Full investment, long only, target return, and group constraints
# opt_qu <- optimize.portfolio(R=ret, portfolio=pspec, 
#                              constraints=list(fi_constr, lo_constr, ret_constr, group_constr),
#                              objectives=list(ret_obj, var_obj),
#                              optimize_method="ROI")
# opt_qu

##### Minimize ETL Optimization #####
# The ROI solver uses the glpk plugin to interface to the Rglpk package for 
# objectives to minimimize expected tail loss

# Full investment and long only constraints
opt_minetl <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr),
                                 objectives=list(etl_obj),
                                 optimize_method="ROI")
opt_minetl

# Full investment, box, and target return constraints
opt_minetl <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr, ret_constr),
                                 objectives=list(etl_obj),
                                 optimize_method="ROI")
opt_minetl

# Full investment, long only, and position limit constraints
opt_minetl <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr, pl_constr),
                                 objectives=list(etl_obj),
                                 optimize_method="ROI")
opt_minetl

# Full investment, long only, and group constraints
opt_minetl <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr, group_constr),
                                 objectives=list(etl_obj),
                                 optimize_method="ROI")
opt_minetl

# Full investment, long only, and factor exposure constraints
opt_minetl <- optimize.portfolio(R=ret, portfolio=pspec, 
                                 constraints=list(fi_constr, lo_constr, facexp_constr),
                                 objectives=list(etl_obj),
                                 optimize_method="ROI")
opt_minetl

