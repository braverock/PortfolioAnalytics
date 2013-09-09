library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

data(edhec)
ret <- edhec[, 1:4]

# set up portfolio specification object target in the return object
pspec1 <- portfolio.spec(assets=colnames(ret))
pspec1 <- add.constraint(portfolio=pspec1, type="leverage", min_sum=1, max_sum=1)
pspec1 <- add.constraint(portfolio=pspec1, type="box")
pspec1 <- add.objective(portfolio=pspec1, type="return", name="mean", target=0.007)

# run optimization using ROI with pspec1
opt1 <- optimize.portfolio(R=ret, portfolio=pspec1, optimize_method="ROI")
opt1
summary(opt1)
wts1 <- extractWeights(opt1)

# set up portfolio specification object target with the return constraint
pspec2 <- portfolio.spec(assets=colnames(ret))
pspec2 <- add.constraint(portfolio=pspec2, type="leverage", min_sum=1, max_sum=1)
pspec2 <- add.constraint(portfolio=pspec2, type="box")
pspec2 <- add.constraint(portfolio=pspec2, type="return", return_target=0.007)
pspec2 <- add.objective(portfolio=pspec2, type="return", name="mean")

# run optimization using ROI with pspec2
opt2 <- optimize.portfolio(R=ret, portfolio=pspec2, optimize_method="ROI")
opt2
summary(opt2)
wts2 <- extractWeights(opt2)

# run optimization with DEoptim using pspec1
set.seed(123)
opt_de1 <- optimize.portfolio(R=ret, portfolio=pspec1, optimize_method="DEoptim", search_size=4000, traceDE=5)
opt_de1

# run optimization with DEoptim using pspec2
set.seed(123)
opt_de2 <- optimize.portfolio(R=ret, portfolio=pspec2, optimize_method="DEoptim", search_size=4000, traceDE=5)
opt_de2

# run optimizations with random portfolios using pspec1
opt_rp1 <- optimize.portfolio(R=ret, portfolio=pspec1, optimize_method="random", search_size=4000)
opt_rp1

# run optimizations with random portfolios using pspec2
opt_rp2 <- optimize.portfolio(R=ret, portfolio=pspec2, optimize_method="random", search_size=4000)
opt_rp2
