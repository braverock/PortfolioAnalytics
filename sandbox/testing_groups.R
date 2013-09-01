library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)


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
minvar <- add.objective(portfolio=init, type="risk", name="var")

# Specify group constraints by passing in category_labels from initial
# portfolio object
group1 <- add.constraint(portfolio=init, type="group",
                        groups=init$category_labels,
                        group_min=c(0.15, 0.25, 0.15, 0.2),
                        group_max=c(0.4, 0.4, 0.6, 0.6))

# Alternative way by specifying a list for group constraints
group2 <- add.constraint(portfolio=init, type="group",
                        groups=list(MICRO=c(1, 2),
                                    SMALL=c(3, 4),
                                    MID=c(5, 6),
                                    LARGE=c(7, 8)),
                        group_min=c(0.2, 0.1, 0.2, 0.2),
                        group_max=c(0.4, 0.4, 0.4, 0.45))
group2$category_labels <- NULL

all.equal(group1$constraints[[3]]$groups, group2$constraints[[3]]$groups)

opt_group1 <- optimize.portfolio(R=R, portfolio=group1, optimize_method="ROI")
extractGroups(opt_group1)
chart.GroupWeights(opt_group1, type="b", col="blue", pch=15, lty=2)

opt_group2 <- optimize.portfolio(R=R, portfolio=group2, optimize_method="ROI")
extractGroups(opt_group2)
chart.GroupWeights(opt_group2, type="b", col="black", pch=21, bg="gray")


  