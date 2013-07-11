library(PortfolioAnalytics)
library(DEoptim)
library(ROI)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

gen.constr <- constraint(assets=funds, min=0, max=1, min_sum=0.99, max_sum=1.01, 
                         weight_seq = generatesequence(min=0, max=1, by=0.002))
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE, multiplier=-1)
# gen.constr <- add.objective(constraints=gen.constr, type="risk", name="CVaR", enabled=FALSE, multiplier=0)

pspec <- portfolio.spec(assets=funds, weight_seq = generatesequence(min=0, max=1, by=0.002))
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", multiplier=-1, enabled=TRUE)
# pspec <- add.objective_v2(portfolio=pspec, type="risk", name="CVaR", multiplier=0, enabled=TRUE)

# tmp1 <- set.portfolio.moments(R=ret, constraints=gen.constr)
# tmp2 <- set.portfolio.moments_v2(R=ret, portfolio=pspec)
# all.equal(tmp1, tmp2)

##### Simple test for DEoptim with optimize.portfolio_v2 #####
# generate an initial population with random_portfolios
rp <- random_portfolios_v2(portfolio=pspec, permutations=40)

set.seed(123)
opt_out <- optimize.portfolio(R=ret, gen.constr, optimize_method="DEoptim", search_size=1000, trace=FALSE, rpseed=rp)

set.seed(123)
opt <- optimize.portfolio_v2(R=ret, portfolio=pspec, optimize_method="DEoptim", search_size=1000, trace=FALSE, rpseed=rp)

all.equal(opt_out$weights, opt$weights)
all.equal(opt_out$objective_measures, opt$objective_measures)


##### Test version of random_portfolios #####
tmp <- random_portfolios(gen.constr)
tmp1 <- random_portfolios_v2(pspec)
all(rowSums(tmp1) <= 1.01) & all(rowSums(tmp1) >= 0.99)

