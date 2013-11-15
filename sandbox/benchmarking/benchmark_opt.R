

library(PortfolioAnalytics)
library(rbenchmark)

data(edhec)
returns <- edhec[,1:10]
funds <- colnames(returns)

# Add basic constraints and objectives
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box", min=0, max=0.45)
# init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

n_portfolios <- 1000
rp <- random_portfolios(portfolio=init.portf, 
                        permutations=n_portfolios, 
                        rp_method="sample", 
                        eliminate=FALSE)

opt1 <- optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=FALSE, trace=TRUE)
opt2 <- optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=TRUE, trace=TRUE)

all.equal(opt1, opt2)
# Component 6, 10, and 11 do not match
# Component 6 is the call
# Component 10 the elapsed time
# Component 11 the end_t

# Make sure the results of opt1 and opt2 are equal
all.equal(extractStats(opt1), extractStats(opt2))

# benchmark different ways of passing the moments to constrained_objective
benchmark(
  reuse=optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=TRUE),
  no_reuse=optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=FALSE),
  replications=1
)[,1:4]

# Rprof runs
# new uses modify.args to evaluate arguments
Rprof(filename="rp_profile_reuse.txt")
optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=TRUE)
Rprof(NULL)

Rprof(filename="rp_profile_no_reuse.txt")
optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, reuse_moments=FALSE)
Rprof(NULL)

out_reuse <- summaryRprof("rp_profile_reuse.txt")
out_no_reuse <- summaryRprof("rp_profile_no_reuse.txt")

lapply(out_reuse, head)
lapply(out_no_reuse, head)