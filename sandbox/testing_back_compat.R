library(PortfolioAnalytics)
library(DEoptim)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# Set up constraint object using v1
gen.constr <- constraint(assets=funds, min=0, max=0.55, min_sum=0.99, max_sum=1, weight_seq=generatesequence(min=0, max=0.55, by=0.002))

# Add an objective. Note the use of add.objective_v1
gen.constr <- add.objective_v1(constraints=gen.constr, type="return", name="mean", enabled=TRUE)

# Random
optrpv1 <- optimize.portfolio_v1(R=ret, constraints=gen.constr, optimize_method="random", search_size=2000)
optrpv1

# DEoptim
optdev1 <- optimize.portfolio_v1(R=ret, constraints=gen.constr, optimize_method="DEoptim", search_size=2000)
optdev1

# When using optimize.portfolio, the user will see that he needs to pass in a
# portfolio object, so the user will likely just create a portfolio object and
# then pass in the v1_constraint object
pspec <- portfolio.spec(assets=funds)

# This uses the new portfolio object and 'v2' of optimize.portfolio. The user 
# can pass a v1_constraint object in for the constraints arg, but still needs to
# pass in a portfolio object so that it can be updated with the constraints and
# objectives from the v1_constraint object
optrp <- optimize.portfolio(R=ret, portfolio=pspec, constraints=gen.constr, optimize_method="random", search_size=2000)
optrp

optde <- optimize.portfolio(R=ret, portfolio=pspec, constraints=gen.constr, optimize_method="DEoptim", search_size=2000, traceDE=5)
optde
