library(PortfolioAnalytics)
library(DEoptim)
data(indexes)
indexes <- indexes[,1:4]
# Clean the returns first rather than passing in as an argument
R.clean <- Return.clean(R=indexes[, 1:4], method="boudt")

# Create the portfolio specification object
init.portf <- portfolio.spec(assets=colnames(indexes[,1:4]))
# Add box constraints
init.portf <- add.constraint(portfolio=init.portf, type='box', min = 0, max=1)
# Add the full investment constraint that specifies the weights must sum to 1.
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")

# Add objective for min CVaR concentration
min_conc <- add.objective(portfolio=init.portf, type="risk_budget_objective",
                         name="CVaR", arguments=list(p=0.95),
                         min_concentration=TRUE)

# Add objective for min CVaR difference
min_diff <- add.objective(portfolio=init.portf, type="risk_budget_objective",
                          name="CVaR", arguments=list(p=0.95),
                          min_difference=TRUE)

# min concentration
set.seed(1234)
opt_min_conc <- optimize.portfolio(R=R.clean, portfolio=min_conc, 
                                   optimize_method="DEoptim", search_size=5000)
# near equal risk pct_contrib portfolio
print(opt_min_conc)

# min difference
set.seed(1234)
opt_min_diff <- optimize.portfolio(R=indexes, portfolio=min_diff, 
                                   optimize_method="DEoptim", search_size=5000)
# Not getting an equal risk contrib using min_difference in risk_budget_objective
# US Bonds have a approx a 2.5% contrib and the rest are 30%-35%
print(opt_min_diff)

# min difference with cleaned returns
set.seed(1234)
opt_min_diff_clean <- optimize.portfolio(R=R.clean, portfolio=min_diff, 
                                   optimize_method="DEoptim", search_size=5000)
# near equal risk pct_contrib with cleaned returns
print(opt_min_diff_clean)

# demonstrate error with clean="boudt" in arguments in objective
# Add objective for min CVaR concentration
min_conc_clean <- add.objective(portfolio=init.portf, type="risk_budget_objective",
                                name="CVaR", arguments=list(p=0.95, clean="boudt"),
                                min_concentration=TRUE)

# If I pass in arguments for dots such as itermax=50, then I get an error with
# clean.boudt.
# Error in clean.boudt(na.omit(R[, column, drop = FALSE]), alpha = alpha,  : 
#                       unused argument(s) (itermax = 50)
set.seed(1234)
opt <- optimize.portfolio(R=R.clean, portfolio=min_conc_clean, 
                          optimize_method="DEoptim", search_size=5000,
                          itermax=50)
traceback()

# Upon insepecting traceback(), it looks like the error is due to
# Return.clean(R, method = objective$arguments.clean, ...) where the dots
# are picking up the dots arguments from optimize.portfolio. Is there a way
# to correct this? I suppose one way is to clean the returns first and not 
# specify clean in the arguments list in the objective. The dots argument in
# optimize.portfolio can then be used to control the parameters for the solvers.

# library(iterators)
# set.seed(1234)
# out <- optimize.portfolio.rebalancing(R=indexes, portfolio=ObjSpec, 
#                                       optimize_method="DEoptim", search_size=5000,
#                                       rebalance_on="months", 
#                                       training_period=nrow(indexes)-1)
# 
# ES(indexes[,1:4], weights=out$weights, p=0.95, clean="boudt", 
#    portfolio_method="component")
# constrained_objective(w=rep(1/4, 4), R=indexes[,1:4], portfolio=ObjSpec)
