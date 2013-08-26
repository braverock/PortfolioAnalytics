
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.quadprog)


data(edhec)
R <- edhec[, 1:4]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R)

# set up portfolio with objectives and constraints
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")
pspec <- add.constraint(portfolio=pspec, type="group",
                        groups=list(groupA=c(1, 3),
                                    groupB=c(2, 4),
                                    geoA=c(1, 2, 4),
                                    geoB=3),
                        group_min=c(0.15, 0.25, 0.15, 0.2),
                        group_max=c(0.4, 0.7, 0.8, 0.62))
pspec

maxret <- add.objective(portfolio=pspec, type="return", name="mean")
opt_maxret <- optimize.portfolio(R=R, portfolio=maxret, optimize_method="ROI")
summary(opt_maxret)

minvar <- add.objective(portfolio=pspec, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=R, portfolio=minvar, optimize_method="ROI")
summary(opt_minvar)

minetl <- add.objective(portfolio=pspec, type="risk", name="ETL")
opt_minetl <- optimize.portfolio(R=R, portfolio=minetl, optimize_method="ROI")
summary(opt_minetl)

maxqu <- add.objective(portfolio=pspec, type="return", name="mean")
maxqu <- add.objective(portfolio=maxqu, type="risk", name="var", risk_aversion=0.25)
opt_maxqu <- optimize.portfolio(R=R, portfolio=maxqu, optimize_method="ROI")
summary(opt_maxqu)
