
library(PortfolioAnalytics)

# Examples of solving optimization problems to minimize portfolio standard deviation

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
print(init.portf)

# Minimizing standard deviation can be formulated as a quadratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.
minStdDev.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                       optimize_method="ROI", 
                                       trace=TRUE)
print(minStdDev.lo.ROI)

plot(minStdDev.lo.ROI, risk.col="StdDev", main="Long Only Minimize Portfolio StdDev")

# It is more practical to impose box constraints on the weights of assets.
# Update the second constraint element with box constraints.
init.portf <- add.constraint(portfolio=init.portf, type="box", 
                             min=0.05, max=0.3, indexnum=2)

minStdDev.box.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                        optimize_method="ROI", 
                                        trace=TRUE)
print(minStdDev.box.ROI)

chart.Weights(minStdDev.box.ROI, main="Minimize StdDev with Box Constraints")

# Although the maximum return objective can be solved quickly and accurately
# with optimize_method="ROI", it is also possible to solve this optimization
# problem using other solvers such as random portfolios or DEoptim.

# For random portfolios, the leverage constraints should be relaxed slightly.
init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

# Add mean as an object with multiplier=0. The multiplier=0 argument means 
# that it will not be used in the objective function, but will be calculated
# for each portfolio so that we can plot the optimal portfolio in 
# mean-StdDev space.
init.portf <- add.objective(portfolio=init.portf, type="return", 
                            name="mean", multiplier=0)

# First run the optimization with a wider bound on the box constraints that 
# also allows shorting. Then use more restrictive box constraints. This is 
# useful to visualize impact of the constraints on the feasible space

# create a new portfolio called 'port1' by using init.portf and modify the
# box constraints
port1 <- add.constraint(portfolio=init.portf, type="box", 
                        min=-0.3, max=0.8, indexnum=2)

minStdDev.box1.RP <- optimize.portfolio(R=R, portfolio=port1, 
                                        optimize_method="random", 
                                        search_size=5000, 
                                        trace=TRUE)
print(minStdDev.box1.RP)
ploy(minStdDev.box1.RP, risk.col="StdDev")

# create a new portfolio called 'port2' by using init.portf and modify the 
# box constraints
port2 <- add.constraint(portfolio=init.portf, type="box", 
                        min=0.05, max=0.3, indexnum=2)

minStdDev.box2.RP <- optimize.portfolio(R=R, portfolio=port2, 
                                        optimize_method="random", 
                                        search_size=5000, 
                                        trace=TRUE)
print(minStdDev.box2.RP)
plot(minStdDev.box2.RP, risk.col="StdDev")

# Now solve the problem with DEoptim
minStdDev.box.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                       optimize_method="DEoptim", 
                                       search_size=5000, 
                                       trace=TRUE)
print(minStdDev.box.DE)
plot(minStdDev.box.DE, risk.col="StdDev", return.col="mean")
