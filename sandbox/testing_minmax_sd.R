# Script to test specifying a min and max instead of a target for an objective

library(PortfolioAnalytics)

data(edhec)

ret <- edhec[, 1:10]

constraints=constraint(assets = colnames(ret), 
                       min = 0, 
                       max = 1, 
                       min_sum=0.99, 
                       max_sum=1.01, 
                       weight_seq = generatesequence())

# add objective to maximize return
constraints <- add.objective(constraints, type="return", name="mean", multiplier=-1, enabled=TRUE)

# add "tmp_minmax" type objective with "sd" as the name of the function. # This is matched to StdDev
# Penalize if sd is outside of the range of min and max
constraints <- add.objective(constraints, type="tmp_minmax", name="sd", 
                             min=0.005, max=0.02, multiplier=0, enabled=TRUE)

# Maximize return subject to a range portfolio volatility values

##### test 1: just calculate portfolio sd #####
set.seed(123)
opt_out_rp1 <- optimize.portfolio(R=ret, constraints, optimize_method="random", search_size=2000, trace=FALSE)

# sd = 0.01385472 when when multiplier = 0
# This calculates the portfolio sd, but does not use it in the objective function
opt_out_rp1$objective_measures$sd

##### test 2: within range #####
# Change the multiplier to 1 and set the min and max outside of the calculated
# portfolio sd. 
constraints$objectives[[2]]$multiplier=1
constraints$objectives[[2]]$min=0.011
constraints$objectives[[2]]$max=0.015

set.seed(123)
opt_out_rp2 <- optimize.portfolio(R=ret, constraints, optimize_method="random", search_size=2000, trace=TRUE)

# portfolio sd should be unchanged
opt_out_rp2$objective_measures$sd

##### test 3: lower max #####
# Lower the max below 0.01385472
constraints$objectives[[2]]$min=0.005
constraints$objectives[[2]]$max=0.011

set.seed(123)
opt_out_rp3 <- optimize.portfolio(R=ret, constraints, optimize_method="random", search_size=2000, trace=TRUE)

# Portfolio sd should now be less than 0.011
opt_out_rp3$objective_measures$sd

##### test 4: raise min #####
# Raise the min above 0.01385472
constraints$objectives[[2]]$min=0.015
constraints$objectives[[2]]$max=0.02

set.seed(123)
opt_out_rp4 <- optimize.portfolio(R=ret, constraints, optimize_method="random", search_size=2000, trace=TRUE)

# Portfolio sd should now be greater than 0.15
opt_out_rp4$objective_measures$sd

# constraints <- add.objective(constraints, type="risk", name="sd", target=0.012, multiplier=1, enabled=TRUE)
# constraints$objectives[[2]]$enabled=FALSE
# opt_out_rp <- optimize.portfolio(R=ret, constraints, optimize_method="random", search_size=2000, trace=FALSE)
# opt_out_rp$objective_measures
