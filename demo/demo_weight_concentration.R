#' ---
#' title: "Weight Concentration Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained portfolio optimization 
#' problem to minimize weight concentration.

#' Load the packages and data
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
data(edhec)
R <- edhec[, 1:8]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM")
funds <- colnames(R)

#' Set labels based on category (e.g. market capitalization)
cap_labels <- c(rep("Group1", 2), rep("Group2", 2), 
                rep("Group3", 2), rep("Group4", 2))


#' Create initial portfolio object with category_labels.
init.portf <- portfolio.spec(assets=funds, category_labels=cap_labels)

#' Add basic constraints.
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")

#' Add objective to minimize variance.
init.portf <- add.objective(portfolio=init.portf, type="risk", name="var")

#' Run the optimization with var as the only objective.
opt1 <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", trace=TRUE)
opt1

#' Add the weight_concentration objective.
#' Set the conc_aversion values to 0 so that we should get the same result as
#' the minimum variance portfolio.
conc.portf <- add.objective(portfolio=init.portf, type="weight_concentration", name="HHI", 
                            conc_aversion=0, conc_groups=init.portf$category_labels)

#' Run the optimization with minimum concentration objective.
opt2 <- optimize.portfolio(R=R, portfolio=conc.portf, optimize_method="ROI", trace=TRUE)
opt2
all.equal(opt1$weights, opt2$weights)
chart.Weights(opt2)

#' Now change the conc_aversion values to control the concentration aversion
#' parameters.
conc.portf$objectives[[2]]$conc_aversion <- c(0.03, 0.03, 0.06, 0.02)
opt3 <- optimize.portfolio(R=R, portfolio=conc.portf, optimize_method="ROI", trace=TRUE)
opt3
chart.Weights(opt3)

#' We do not have a group constraint, but we can plot the groups based on
#' category labels in the portfolio object.
chart.GroupWeights(opt3, grouping="category", plot.type="barplot", col=bluemono)

#' High conc_aversion values should result in an equal weight portfolio.
conc.portf$objectives[[2]]$conc_aversion <- rep(1e6, 4)
opt4 <- optimize.portfolio(R=R, portfolio=conc.portf, optimize_method="ROI", trace=TRUE)
opt4
chart.Weights(opt4)
