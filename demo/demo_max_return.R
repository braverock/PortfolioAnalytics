
library(PortfolioAnalytics)

# Examples of solving optimization problems to maximize mean return

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf

# Maximizing return can be formulated as a linear programming problem and
# solved very quickly using optimize_method="ROI". We are using long_only
# constraints so it is expected that allocation is to the portfolio with the
# highest mean return.
maxret.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                    optimize_method="ROI", trace=TRUE)
maxret.lo.ROI

chart.Weights(maxret.lo.ROI, main="Long Only Maximize Return")

# It is more practical to impose box constraints on the weights of assets.
# Update the second constraint element with box constraints
init.portf <- add.constraint(portfolio=init.portf, type="box", 
                             min=0.05, max=0.3, indexnum=2)

maxret.box.ROI <- optimize.portfolio(R=R, portfolio=init.portf, 
                                     optimize_method="ROI", trace=TRUE)
maxret.box.ROI

chart.Weights(maxret.box.ROI, main="Box Maximize Return")

# Although the maximum return objective can be solved quickly and accurately
# with optimize_method="ROI", it is also possible to solve this optimization
# problem using other solvers such as random portfolios or DEoptim.

# For random portfolios, the leverage constraints should be relaxed slightly.
init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

# Add StdDev as an object with multiplier=0. The multiplier=0 argument means 
# that it will not be used in the objective function, but will be calculated
# for each portfolio so that we can plot the optimal portfolio in 
# mean-StdDev space.
init.portf <- add.objective(portfolio=init.portf, type="risk", 
                            name="StdDev", multiplier=0)

# First run the optimization with a wider bound on the box constraints that 
# also allows shorting. Then use more restrictive box constraints. This is 
# useful to visualize impact of the constraints on the feasible space

# create a new portfolio called 'port1' by using init.portf and modify the
# box constraints
port1 <- add.constraint(portfolio=init.portf, type="box", 
                             min=-0.3, max=0.8, indexnum=2)

maxret.box1.RP <- optimize.portfolio(R=R, portfolio=port1, 
                                    optimize_method="random", 
                                    search_size=2000, 
                                    trace=TRUE)
maxret.box1.RP
ploy(maxret.box1.RP, risk.col="StdDev")

# create a new portfolio called 'port2' by using init.portf and modify the 
# box constraints
port2 <- add.constraint(portfolio=init.portf, type="box", 
                             min=0.05, max=0.3, indexnum=2)

maxret.box2.RP <- optimize.portfolio(R=R, portfolio=port2, 
                                    optimize_method="random", 
                                    search_size=2000, 
                                    trace=TRUE)
maxret.box2.RP
plot(maxret.box2.RP, risk.col="StdDev")

# Now solve the problem with DEoptim
maxret.box.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                    optimize_method="DEoptim", 
                                    search_size=2000, 
                                    trace=TRUE)
maxret.box.DE
plot(maxret.box.DE, risk.col="StdDev", return.col="mean")
