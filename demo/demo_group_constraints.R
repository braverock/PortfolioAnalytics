
# Examples of solving optimization problems with group constraints

library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

# Set up portfolio with objectives and constraints
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")
# Add  group constraints such that assets 1, 3, and 5 are in a group called
# GroupA and assets 2 and 4 are in a group called Group B. The sum of the 
# weights in GroupA must be between 0.05 and 0.7. The sum of the weights in
# GroupB must be between 0.15 and 0.5.
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=list(groupA=c(1, 3, 5),
                                    groupB=c(2, 4)),
                        group_min=c(0.05, 0.15),
                        group_max=c(0.7, 0.5))
print(pspec)


# Add an objective to minimize portfolio standard deviation
pspec <- add.objective(portfolio=pspec, type="risk", name="StdDev")

# The examples here use the obective to minimize standard deviation, but any
# supported objective can also be used.

# Minimizing standard deviation can be formulated as a quadratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.
minStdDev.ROI <- optimize.portfolio(R=R, portfolio=pspec, optimize_method="ROI")
print(minStdDev.ROI)
extractGroups(minStdDev.ROI)

# The leverage constraints should be relaxed slightly for random portfolios 
# and DEoptim
pspec$constraints[[1]]$min_sum=0.99
pspec$constraints[[1]]$max_sum=1.01

# Solve with random portfolios
# By construction, the random portfolios will be generated to satisfy the
# group constraint.
minStdDev.RP <- optimize.portfolio(R=R, portfolio=pspec, 
                                   optimize_method="random", search_size=2500)
print(minStdDev.RP)
extractGroups(minStdDev.RP)

# Solve with DEoptim
minStdDev.DE <- optimize.portfolio(R=R, portfolio=pspec, 
                                   optimize_method="DEoptim", search_size=2500)
print(minStdDev.DE)
extractGroups(minStdDev.DE)
