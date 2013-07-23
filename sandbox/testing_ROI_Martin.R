# Testing for replicating professor Martin's examples
# The numbered examples corresopond to 1. theory review weights constrained mvo v5.pdf

# data = crsp.short.Rdata
# returns = midcap.ts[, 1:10]

rm(list=ls())

# Load packages
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

# Use edhec data set from PerformanceAnalytics for reproducing if user does not
# have the crsp.short.Rdata data
# data(edhec)
# returns <- edhec[, 1:10]

# Use crsp.short.Rdata from Prof Martin
# data file should be in working directory or specify path
# Can we include this as a data set in the PortfolioAnalytics package?
load("/Users/rossbennett/Desktop/Testing/crsp.short.Rdata")

returns <- midcap.ts[, 1:10]
funds <- colnames(returns)


##### Example 1.1: Global Minimum Variance (GMV) Portfolio #####

# GMV portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=-Inf, max=Inf, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.2: Long Only GMV Portfolio #####

# GMV long only portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.3: GMV Box Constraints #####

# GMV box constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.4: GMV long only with Group Constraints #####
# Combine returns from different market cap groups
returns.cap <- cbind(microcap.ts[, 1:2],
                     smallcap.ts[, 1:2],
                     midcap.ts[, 1:2],
                     largecap.ts[, 1:2])

funds.cap <- colnames(returns.cap)

# GMV group constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds.cap)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="group", enabled=TRUE,
                        groups=c(2, 2, 2, 2),
                        group_min=c(0.1, 0.15, 0, 0), 
                        group_max=c(0.25, .35, 0.35, 0.45),
                        group_labels=c("MICRO", "SMALL", "MID", "LARGE"))
pspec <- add.objective(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio(R=returns.cap, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Get the group weights
# This is something I will work to include in the summary.optimize.portfolio.ROI
groups <- pspec$constraints[[3]]$groups
group_labels <- pspec$constraints[[3]]$group_labels
n.groups <- length(groups)
group_weights <- rep(0, n.groups)
k <- 1
l <- 0
for(i in 1:n.groups){
  j <- groups[i]
  group_weights[i] <- sum(opt$weights[k:(l+j)])
  k <- k + j
  l <- k - 1
}
names(group_weights) <- group_labels
group_weights

# Portfolio standard deviation
sqrt(opt$out)

# In the previous examples, we were solving global minimum variance with optmize_method="ROI". 
# The solve.QP plugin is selected automatically by optimize.portfolio when "var" is the objective

##### Example 1.6: Maximize mean-return with box constraints #####

# Maximize mean return with box constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="return", name="mean", enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.7 Maximize mean-return Long Only with Group Constraints #####

# GMV group constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds.cap)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="group", enabled=TRUE,
                        groups=c(2, 2, 2, 2),
                        group_min=c(0.1, 0.15, 0, 0), 
                        group_max=c(0.25, .35, 0.35, 0.45),
                        group_labels=c("MICRO", "SMALL", "MID", "LARGE"))
pspec <- add.objective(portfolio=pspec, type="return", name="mean", enabled=TRUE)

opt <- optimize.portfolio(R=returns.cap, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Get the group weights
# This is something I will work to include in the summary.optimize.portfolio.ROI
groups <- pspec$constraints[[3]]$groups
group_labels <- pspec$constraints[[3]]$group_labels
group_weights <- rep(0, n.groups)
n.groups <- length(groups)
k <- 1
l <- 0
for(i in 1:n.groups){
  j <- groups[i]
  group_weights[i] <- sum(opt$weights[k:(l+j)])
  k <- k + j
  l <- k - 1
}
names(group_weights) <- group_labels
group_weights

# Portfolio standard deviation
sqrt(opt$out)

# Check results for quadratic utility with manual code
p <- ncol(returns)
V <- var(returns)
mu <- colMeans(returns)
lambda <- 20
min_wt <- 0
max_wt <- 1

# parameters for solve.QP
A <- cbind(rep(1, p), diag(p), -diag(p))
b <- c(1, rep(min_wt, p), rep(-max_wt, p))
d <- mu
res <- quadprog:::solve.QP(Dmat=2*lambda*V, dvec=d, Amat=A, bvec=b, meq=1)
wts2 <- round(res$solution, 4)
names(wts2) <- colnames(returns)
wts2

# Note that target mean return CANNOT be specified as a constraint currently
# It is specified as a target in the return objective
# Can do quadratic utility optimization with target return

##### Example X: Mean Variance Optimization (MVO) with target mean return constraint #####

# MVO with target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=-Inf, max=Inf, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)


##### Example X: Mean Variance Optimization (MVO) with target mean return and long only constraints #####

# MVO with long only and target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)

##### Example X: Mean Variance Optimization (MVO) with target mean return and box constraints #####

# MVO with box constraints and target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)

##### Example X: ETL Long Only #####

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
# This can be specified with ETL, ES, or CVaR for name
pspec <- add.objective(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

##### Example X: ETL with box constraints #####

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
# This can be specified with ETL, ES, or CVaR for name
pspec <- add.objective(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

##### Example X: ETL long only with group constraints #####

# GMV group constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds.cap)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="group", enabled=TRUE,
                        groups=c(2, 2, 2, 2),
                        group_min=c(0.1, 0.15, 0, 0), 
                        group_max=c(0.25, .35, 0.35, 0.45),
                        group_labels=c("MICRO", "SMALL", "MID", "LARGE"))
pspec <- add.objective(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio(R=returns.cap, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Get the group weights
# This is something I will work to include in the summary.optimize.portfolio.ROI
groups <- pspec$constraints[[3]]$groups
group_labels <- pspec$constraints[[3]]$group_labels
group_weights <- rep(0, n.groups)
n.groups <- length(groups)
k <- 1
l <- 0
for(i in 1:n.groups){
  j <- groups[i]
  group_weights[i] <- sum(opt$weights[k:(l+j)])
  k <- k + j
  l <- k - 1
}
names(group_weights) <- group_labels
group_weights

