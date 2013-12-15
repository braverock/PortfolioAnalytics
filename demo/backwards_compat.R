library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# Set up constraint object using v1 specification
gen.constr <- constraint(assets=funds, min=0, max=0.55, min_sum=0.99, max_sum=1.01, 
                         weight_seq=generatesequence(min=0, max=0.55, by=0.002))
class(gen.constr)

# Add an objective to the gen.constr object
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE)

# Run the optimization
# optimize.portfolio will detect that a v1_constraint object has been passed in
# and will update to the v2 specification using a portfolio object with 
# constraints and objectives from the v1_constraint object.

# Random Portfolios
optrpv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="random", search_size=2000)
optrpv1

# DEoptim
optdev1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="DEoptim", search_size=2000)
optdev1

# ROI
optroiv1 <- optimize.portfolio(R=ret, constraints=gen.constr, optimize_method="ROI")
optroiv1


