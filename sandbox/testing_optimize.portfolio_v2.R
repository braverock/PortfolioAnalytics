library(PortfolioAnalytics)
library(DEoptim)
library(ROI)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# Set up constraints and objectives using old interface
gen.constr <- constraint(assets=funds, min=0, max=1, min_sum=0.99, max_sum=1.01, 
                         weight_seq = generatesequence(min=0, max=1, by=0.002))
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE, multiplier=-1)

# Set up constraints and objectives using new interface
pspec <- portfolio.spec(assets=funds, weight_seq = generatesequence(min=0, max=1, by=0.002))
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", multiplier=-1, enabled=TRUE)

# tmp1 <- set.portfolio.moments(R=ret, constraints=gen.constr)
# tmp2 <- set.portfolio.moments_v2(R=ret, portfolio=pspec)
# all.equal(tmp1, tmp2)

##### Simple test for DEoptim method with optimize.portfolio_v2 #####
# generate an initial population with random_portfolios
rp <- random_portfolios_v2(portfolio=pspec, permutations=40)

set.seed(123)
opt_out_de <- optimize.portfolio(R=ret, gen.constr, optimize_method="DEoptim", search_size=1000, trace=FALSE, rpseed=rp)

set.seed(123)
opt_de <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="DEoptim", search_size=1000, trace=FALSE, rpseed=rp)

# The results should be the same using the same initial population and set.seed
all.equal(opt_out_de$weights, opt_de$weights)
all.equal(opt_out_de$objective_measures, opt_de$objective_measures)

# Note that values are now different since I added fnMap=fn_map to DEoptim in optimize.portfolio_v2
# This is likely due to how normalization/transformation is handled

##### Simple test for random method with optimize.portfolio_v2 #####

set.seed(123)
opt_out_rp <- optimize.portfolio(R=ret, gen.constr, optimize_method="random", search_size=2000, trace=FALSE)

set.seed(123)
opt_rp <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="random", search_size=2000, trace=FALSE)

# The results should be the same
all.equal(opt_out_rp$weights, opt_rp$weights)
all.equal(opt_out_rp$objective_measures, opt_rp$objective_measures)

##### Simple test for pso method with optimize.portfolio_v2 #####

set.seed(123)
opt_out_pso <- optimize.portfolio(R=ret, gen.constr, optimize_method="pso", search_size=2000, trace=FALSE)

set.seed(123)
opt_pso <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="pso", search_size=2000, trace=FALSE)

# The results should be the same
all.equal(opt_out_pso$weights, opt_pso$weights)
all.equal(opt_out_pso$objective_measures, opt_pso$objective_measures)

##### Simple test for GenSA method with optimize.portfolio_v2 #####

set.seed(123)
opt_out_gensa <- optimize.portfolio(R=ret, gen.constr, optimize_method="GenSA", search_size=2000, trace=FALSE)

set.seed(123)
opt_gensa <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="GenSA", search_size=2000, trace=FALSE)

# The results should be the same
all.equal(opt_out_gensa$weights, opt_gensa$weights)
all.equal(opt_out_gensa$objective_measures, opt_gensa$objective_measures)

##### Simple test for ROI method with optimize.portfolio_v2 #####
# specify CVaR with old interface and ETL with new interface

# Set up constraints and objectives using old interface
gen.constr <- constraint(assets=funds, min=0, max=1, min_sum=0.99, max_sum=1.01, 
                         weight_seq = generatesequence(min=0, max=1, by=0.002))
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="CVaR", enabled=TRUE, multiplier=-1)

# Set up constraints and objectives using new interface
pspec <- portfolio.spec(assets=funds, weight_seq = generatesequence(min=0, max=1, by=0.002))
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="ETL", multiplier=-1, enabled=TRUE)

opt_out_roi <- optimize.portfolio(R=ret, gen.constr, optimize_method="ROI", search_size=2000, trace=FALSE)

opt_roi <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="ROI", search_size=2000, trace=FALSE)

# The results should be the same
all.equal(opt_out_roi$weights, opt_roi$weights)
all.equal(opt_out_roi$objective_measures, opt_roi$objective_measures)

##### Test version of random_portfolios #####
tmp <- random_portfolios(gen.constr)
tmp1 <- random_portfolios_v2(pspec)
all(rowSums(tmp1) <= 1.01) & all(rowSums(tmp1) >= 0.99)

