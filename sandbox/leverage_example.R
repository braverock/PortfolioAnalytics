
# Examples demonstrating the difference of specifying a leverage constraint 
# using max_sum compared to leverage_exposure.

library(PortfolioAnalytics)

data(edhec)
R <- edhec
funds <- colnames(R)

# Set up an initial portfolio object with basic constraints
init.portf <- portfolio.spec(assets=funds)

# Add an objective to maximize mean return per unit expected shortfall
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

# dollar neutral portfolio
dollar.neutral.portf <- init.portf
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="weight_sum", 
                                       min_sum=-0.01, max_sum=0.01)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="box", min=-0.5, max=0.5)

# Here is a dollar neutral portfolio with no constraint on leverage
opt1 <- optimize.portfolio(R=R, portfolio=dollar.neutral.portf, 
                           optimize_method="DEoptim", search_size=2000, 
                           trace=TRUE)
sum(opt1$weights)

# Total portfolio leverage is actually greater than 4
sum(abs(opt1$weights))

# now add the leverage exposure constraint for 2:1 leverage
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="leverage_exposure", leverage=2)
# Run optimization
opt2 <- optimize.portfolio(R=R, portfolio=dollar.neutral.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt2$weights)
sum(abs(opt2$weights))

# Leveraged portfolio
leveraged.portf <- init.portf

# Add a "leverage" constraint using max_sum
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="leverage", 
                                  min_sum=1.29, max_sum=1.31)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="box", min=-0.3, max=0.6)

opt3 <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt3$weights)

# total portfolio leverage is approximately 3.9
sum(abs(opt3$weights))

# add a leverage exposure constraint
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="leverage_exposure", leverage=1.5)

# change min_sum and max_sum such that the weights sum to 1
leveraged.portf$constraints[[1]]$min_sum <- 0.99
leveraged.portf$constraints[[1]]$max_sum <- 1.01

# Run optimization
opt4 <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt4$weights)
# total portfolio leverage is less than 1.5
sum(abs(opt4$weights))

