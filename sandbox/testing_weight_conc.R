library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# data(edhec)
# R <- edhec[, 1:4]
# colnames(R) <- c("CA", "CTAG", "DS", "EM")
# funds <- colnames(R)

load("~/Desktop/Testing/crsp.short.Rdata")
R <- cbind(microcap.ts[, 1:2],
           smallcap.ts[, 1:2],
           midcap.ts[, 1:2],
           largecap.ts[, 1:2])

funds <- colnames(R)

cap_labels <- c(rep("MICRO", 2), rep("SMALL", 2), 
                rep("MID", 2), rep("LARGE", 2))

# Create initial portfolio object with category_labels
init <- portfolio.spec(assets=funds, category_labels=cap_labels)
# Add some weight constraints
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="long_only")
# Add objective to minimize variance
init <- add.objective(portfolio=init, type="risk", name="var")

# Run the optimization with var as the only objective
opt1 <- optimize.portfolio(R=R, portfolio=init, optimize_method="ROI", trace=TRUE)
opt1

# Add the weight_concentration objective
# Set the conc_aversion values to 0 so that we should get the same value as min var
conc <- add.objective(portfolio=init, type="weight_concentration", name="HHI", 
                       conc_aversion=0, conc_groups=init$category_labels)

opt2 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt2
all.equal(opt1$weights, opt2$weights)

# Now change the conc_aversion values to give highest penalty to small cap stocks
conc$objectives[[2]]$conc_aversion <- c(0.05, 1, 0.1, 0)
opt3 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt3

# If all the conc_aversion values are very high, this should result in an equal weight portfolio
conc$objectives[[2]]$conc_aversion <- rep(1e6, 4)
opt4 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt4
