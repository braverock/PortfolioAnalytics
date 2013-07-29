
# testing insert_constraints and insert_objectives

library(PortfolioAnalytics)
data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# We still need a portfolio object, but it does not need constraints and objectives
pspec <- portfolio.spec(assets=funds)

# Get the assets from the portfolio object
assets <- pspec$assets

# Note that box constraints are 'portfolio-aware' so we need to know the assets.
# The assets should be a named vector of seed weights.
box1 <- box_constraint(assets=assets, min=0, max=0.65)
# We can specify type="long_only"
box2 <- box_constraint(type="long_only", assets=assets)
# If min_sum and max_sum are not specified, we default to long only
box3 <- box_constraint(assets=assets)

# We default to min_sum=0.99 and max_sum=1.01
weight1 <- weight_sum_constraint()
# By specifying type="full_investment", min_sum=1 and max_sum=1.
weight2 <- weight_sum_constraint(type="full_investment")
# By specifying type="active" or type="dollar_neutral", min_sum=0 and max_sum=0.
weight3 <- weight_sum_constraint(type="active")
weight4 <- weight_sum_constraint(type="dollar_neutral")

# Note that group constraints are 'portfolio-aware' so we need to know the assets.
# The assets should be a named vector of seed weights.
# These are the required arguments to specify group constraints
group1 <- group_constraint(assets=assets, groups=c(2, 2), group_min=0, group_max=0.8)

# Alternatively, we can also specify labels for the groups with the 
# group_labels arg as well as group position limits with group_pos
group2 <- group_constraint(assets=assets, groups=c(2, 2), group_labels=c("Small", "Large"), 
                 group_min=0, group_max=0.8, group_pos=c(2, 1))

# We can specify a target turnover as a constraint 
to1 <- turnover_constraint(turnover_target=0.3)

# We can specify a target diversification as a constraint 
div1 <- diversification_constraint(div_target=0.8)

# We can specify the maximum number of positions (non-zero asset weights)
pl1 <- position_limit_constraint(assets=assets, max_pos=3)

# We can also specify the maximum number of long positions and maximum number
# of short positions
pl2 <- position_limit_constraint(assets=assets, max_pos_long=2, max_pos_short=2)

foo <- 4
bar <- "fubar"

tmp1 <- list(weight1, box1, group1)

tmp2 <- list(weight2, box2, group2)

tmp3 <- list(weight1, box1, foo, bar, div1)

tmp4 <- c(weight3, box1, div1)
tmp5 <- list(weight3, box1, div1)
tmp6 <- list(weight2, box2, group2)

print.default(insert_constraints(portfolio=pspec, constraints=tmp1))
print.default(insert_constraints(constraints=tmp1))
print.default(insert_constraints(portfolio=pspec, constraints=tmp2))
print.default(insert_constraints(portfolio=pspec, constraints=tmp3))
print.default(insert_constraints(portfolio=pspec, constraints=tmp4))
print.default(insert_constraints(portfolio=pspec, constraints=tmp5))
print.default(insert_constraints(portfolio=pspec, constraints=tmp6))

# objective functions
ret1 <- return_objective(name="mean")
ret2 <- return_objective(name="mean", target=0.008)

risk1 <- portfolio_risk_objective(name="var")
# risk_budget_objective()
to1 <- turnover_objective(name="turnover")
minmax1 <- minmax_objective(name="var", min=0.003, max=0.005)
minmax2 <- minmax_objective(name="mean", min=0.06, max=0.08)

obj1 <- list(ret1)
obj2 <- list(risk1)
obj3 <- list(ret2, risk1)
obj4 <- list(ret1, foo, risk1)
obj5 <- c(ret1, risk1)
obj6 <- list(ret1, risk1, to1, minmax2)

print.default(insert_objectives(portfolio=pspec, objectives=obj1))
print.default(insert_objectives(objectives=obj1))
print.default(insert_objectives(portfolio=pspec, objectives=obj2))
print.default(insert_objectives(portfolio=pspec, objectives=obj3))
print.default(insert_objectives(portfolio=pspec, objectives=obj4))
print.default(insert_objectives(portfolio=pspec, objectives=obj5))
print.default(insert_objectives(portfolio=pspec, objectives=obj6))


