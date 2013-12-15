
library(PortfolioAnalytics)

# Examples of solving optimization problems to minimize expected shortfall (ES)
# The objective can also be specified as "CVaR" and "ETL".

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
# Add objective to minimize expected shortfall with a confidence level of
# 0.9.
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES",
                            arguments=list(p=0.9))
print(init.portf)

# Minimizing expected shortfall can be formulated as a linear programming 
# problem and solved very quickly using optimize_method="ROI". The linear
# programming problem is formulated to minimize sample ES.
minES.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="ROI", 
                                   trace=TRUE)
print(minES.lo.ROI)

plot(minES.lo.ROI, risk.col="ES", return.col="mean", 
     main="Long Only Minimize Expected Shortfall")

# It is more practical to impose box constraints on the weights of assets.
# Update the second constraint element with box constraints.
init.portf <- add.constraint(portfolio=init.portf, type="box", 
                             min=0.05, max=0.3, indexnum=2)

minES.box.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                    optimize_method="ROI", 
                                    trace=TRUE)
print(minES.box.ROI)

chart.Weights(minES.box.ROI, main="Minimize ES with Box Constraints")

# Although the minimum ES objective can be solved quickly and accurately
# with optimize_method="ROI", it is also possible to solve this optimization
# problem using other solvers such as random portfolios or DEoptim. These
# solvers have the added flexibility of using different methods to calculate
# ES (e.g. gaussian, modified, or historical). The default is to calculate
# modified ES.

# For random portfolios and DEoptim, the leverage constraints should be 
# relaxed slightly.
init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

# Add mean as an objective with multiplier=0. The multiplier=0 argument means 
# that it will not be used in the objective function, but will be calculated
# for each portfolio so that we can plot the optimal portfolio in 
# mean-ES space.
init.portf <- add.objective(portfolio=init.portf, type="return", 
                            name="mean", multiplier=0)

# First run the optimization with a wider bound on the box constraints that 
# also allows shorting. Then use more restrictive box constraints. This is 
# useful to visualize impact of the constraints on the feasible space

# create a new portfolio called 'port1' by using init.portf and modify the
# box constraints
port1 <- add.constraint(portfolio=init.portf, type="box", 
                        min=-0.3, max=0.8, indexnum=2)

minES.box1.RP <- optimize.portfolio(R=R, portfolio=port1, 
                                    optimize_method="random", 
                                    search_size=2000, 
                                    trace=TRUE)
print(minES.box1.RP)
plot(minES.box1.RP, risk.col="ES", return.col="mean")

# create a new portfolio called 'port2' by using init.portf and modify the 
# box constraints
port2 <- add.constraint(portfolio=init.portf, type="box", 
                        min=0.05, max=0.3, indexnum=2)

minES.box2.RP <- optimize.portfolio(R=R, portfolio=port2, 
                                    optimize_method="random", 
                                    search_size=2000, 
                                    trace=TRUE)
print(minES.box2.RP)
plot(minES.box2.RP, risk.col="ES", return.col="mean")

# Now solve the problem with DEoptim
minES.box.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="DEoptim", 
                                   search_size=2000, 
                                   trace=TRUE)
print(minES.box.DE)
plot(minES.box.DE, risk.col="ES", return.col="mean")
