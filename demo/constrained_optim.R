require("PerformanceAnalytics")
require("PortfolioAnalytics")
require("DEoptim")

# Load the data
data(edhec)

#constraints
constraints <- constraint(assets = colnames(edhec[, 1:10]), min = 0.01, max = 0.4, min_sum=1, max_sum=1, weight_seq = generatesequence())
# note that if you wanted to do a random portfolio optimization, mun_sum of .99 and max_sum of 1.01 might be more appropriate
constraints <- add.objective(constraints=constraints, type="return", name="mean", arguments=list(), enabled=TRUE)
constraints <- add.objective(constraints=constraints, type="risk_budget", name="ES", arguments=list(), enabled=TRUE, p=.95, min_prisk=.05, max_prisk=.15)

#now set some additional bits
# I should have set the multiplier for returns to negative
constraints$objectives[[1]]$multiplier=-1
# and let's set a portfolio risk target in the risk budget objective too
constraints$objectives[[2]]$target=.05
# and clean the returns
constraints$objectives[[2]]$clean="boudt"

print("We'll use a search_size parameter of 1000 for this demo, but realistic portfolios will likely require search_size parameters much larger, the default is 20000 which is almost always large enough for any realistic portfolio and constraints, but will take substantially longer to run.")
# look for a solution using both DEoptim and random portfolios
opt_out <- optimize.portfolio(R=edhec[,1:10], constraints=constraints, optimize_method="DEoptim", search_size=1000, trace=TRUE)

#we need a little more wiggle in min/max sum for random portfolios or it takes too long to converge
constraints$min_sum <- 0.99
constraints$max_sum <- 1.01
opt_out_random <- optimize.portfolio(R=edhec[,1:10], constraints=constraints, optimize_method="random", search_size=1000, trace=TRUE)

# now lets try a portfolio that rebalances quarterly
opt_out_rebalancing <- optimize.portfolio.rebalancing_v1(R=edhec[,1:10], constraints=constraints, optimize_method="DEoptim", search_size=1000, trace=FALSE,rebalance_on='quarters')
rebalancing_weights <- matrix(nrow=length(opt_out_rebalancing),ncol=length(opt_out_rebalancing[[1]]]$weights))
rownames(rebalancing_weights) <- names(opt_out_rebalancing)
colnames(rebalancing_weights) <- names(opt_out_rebalancing[[1]]$weights)
for(period in 1:length(opt_out_rebalancing)) rebalancing_weights[period,] <- opt_out_rebalancing[[period]]$weights
rebalancing_returns <- Return.rebalancing(R=edhec,weights=rebalancing_weights)
charts.PerformanceSummary(rebalancing_returns)

# and now lets rebalance quarterly with 48 mo trailing
opt_out_trailing<-optimize.portfolio.rebalancing(R=edhec[,1:10], constraints=constraints, optimize_method="DEoptim", search_size=1000, trace=FALSE,rebalance_on='quarters',trailing_periods=48,training_period=48)
