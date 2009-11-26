require("optimizer")
require("DEoptim")
data(edhec)
constraints=constraint(assets = colnames(edhec[, 1:10]), min = 0.01, max = 0.4, weight_seq = generatesequence())
constraints<-add.objective(constraints, type="return", name="mean", enabled=TRUE)
constraints<-add.objective(constraints, type="risk_budget", name="ES", enabled=TRUE, p=.95, min_prisk=.05, max_prisk=.15)
constraints
#now set some additional bits
# I should have set the multiplier for returns to negative
constraints$objectives[[1]]$multiplier=-10
# and let's set a portfolio risk target in the risk budget objective too
constraints$objectives[[2]]$target=.05
# and clean the returns
constraints$objectives[[2]]$clean="boudt"
optimize.portfolio(R=edhec[,1:10], constraints, optimize_method=c("DEoptim"), search_size=1000, trace=TRUE)
