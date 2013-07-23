# Testing for the new portfolio specification

# Load necessary packages
library(PortfolioAnalytics)

# Load the edhec data
data(edhec)

# Use the first 5 columns of edhec as returns
ret <- edhec[, 1:5]
funds <- colnames(ret)

# Specify a portfolio object
pspec <- portfolio.spec(assets=funds)

# pspec is an object of class "portfolio" that holds information about the
# assets, constraints, and objectives.
# The constraints will be stored as objects in the $constraints list
# The objectives will be stored as objects in the $constraints list. Note that
# this is just like how they are currently stored in the constraints object.
class(pspec)
str(pspec)

# Add a constraint object to pspec for the sum of the weights
pspec <- add.constraint(portfolio=pspec, type="weight_sum",
                        min_sum=0.99, max_sum=1.01)
print(pspec)

# Forgot to enable the weight_sum constraint
pspec <- add.constraint(portfolio=pspec, type="weight_sum",
                        min_sum=0.99, max_sum=1.01, enabled=TRUE,
                        indexnum=1)
print(pspec)

# Add box constraints to the pspec object
pspec <- add.constraint(portfolio=pspec, type="box", min=0.1, max=0.4, enabled=TRUE)
print(pspec)

# Update the box constraints to pass in a vector for min and max. Updates the
# object in place with the indexnum argument
pspec <- add.constraint(portfolio=pspec, type="box", 
                        min=c(0.1, 0.05, 0.1, 0.15, 0.2), 
                        max=c(0.4, 0.4, 0.5, 0.45, 0.6), 
                        indexnum=2)
print(pspec)

# Add objectives to the pspec object
pspec <- add.objective(portfolio=pspec, type="return", name="mean", 
                       enabled=FALSE, multiplier=0)
print(pspec)

pspec <- add.objective(portfolio=pspec, type="risk", name="var", 
                       enabled=FALSE, multiplier=0, risk_aversion=10)
print(pspec)

pspec <- add.objective(portfolio=pspec, type="risk", name="CVaR", 
                       enabled=FALSE, multiplier=0)
print(pspec)

str(pspec)
summary(pspec)