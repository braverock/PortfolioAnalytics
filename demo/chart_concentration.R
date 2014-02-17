
library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, 
                             type="leverage", 
                             min_sum=0.99, 
                             max_sum=1.01)

init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0, 
                             max=1)

init.portf <- add.objective(portfolio=init.portf, 
                            type="return", 
                            name="mean", 
                            multiplier=0)

init.portf <- add.objective(portfolio=init.portf, 
                            type="risk", 
                            name="ES")

rb.portf <- add.objective(portfolio=init.portf, 
                          type="risk_budget", 
                          name="ES",
                          max_prisk=0.4, 
                          arguments=list(p=0.92))

# Use DEoptim for optimization
opt <- optimize.portfolio(R=R, 
                          portfolio=init.portf, 
                          optimize_method="random", 
                          search_size=2000, 
                          trace=TRUE)

opt_rb <- optimize.portfolio(R=R, 
                             portfolio=rb.portf, 
                             optimize_method="random", 
                             search_size=2000, 
                             trace=TRUE)

# This won't work because opt is not a risk budget optimization
# This should result in an error and not plot anything
chart.Concentration(opt, conc.type="pct_contrib")

# opt is minimum ES optimization so we can still chart it using weights as
# the measure of concentration
chart.Concentration(opt, conc.type="weights", chart.assets=TRUE, col=heat.colors(10))
chart.Concentration(opt, conc.type="weights", chart.assets=TRUE, col=bluemono)

# The concentration is based on the HHI of the percentage component 
# contribution to risk
chart.Concentration(opt_rb, conc.type="pct_contrib")

# The concentration is based on the HHI of the weights
chart.Concentration(opt_rb, conc.type="weights")

