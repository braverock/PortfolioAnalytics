library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

data(edhec)
ret <- edhec[, 1:4]

# Create an initial portfolio object
init.portf <- portfolio.spec(assets=colnames(ret))
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

# Add mean return objective with target return
ret.obj.portf <- add.objective(portfolio=init.portf, type="return", 
                         name="mean", target=0.007)

# Add return target constraint
ret.constr.portf <- add.constraint(portfolio=init.portf, type="return", return_target=0.007)
ret.constr.portf <- add.objective(portfolio=ret.constr.portf, type="return", name="mean")


# Run optimization using ROI with target return as an objective
ret.obj.opt <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, optimize_method="ROI")
ret.obj.opt

# Run optimization using ROI with target return as a constraint
ret.constr.opt <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, optimize_method="ROI")
ret.constr.opt

# Relaxe constraints for the sum of weights for DEoptim and random portfolios
ret.obj.portf$constraints[[1]]$min_sum <- 0.99
ret.obj.portf$constraints[[1]]$max_sum <- 1.01

ret.constr.portf$constraints[[1]]$min_sum <- 0.99
ret.constr.portf$constraints[[1]]$max_sum <- 1.01

# run optimization with DEoptim using ret.obj.portf
set.seed(123)
opt.obj.de <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.obj.de

# run optimization with DEoptim using ret.constr.portf
set.seed(123)
opt.constr.de <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, optimize_method="DEoptim", search_size=2000, traceDE=5)
opt.constr.de

# run optimizations with random portfolios using ret.obj.portf
opt.obj.rp <- optimize.portfolio(R=ret, portfolio=ret.obj.portf, optimize_method="random", search_size=2000)
opt.obj.rp

# run optimizations with random portfolios using ret.constr.portf
opt.constr.rp <- optimize.portfolio(R=ret, portfolio=ret.constr.portf, optimize_method="random", search_size=2000)
opt.constr.rp
