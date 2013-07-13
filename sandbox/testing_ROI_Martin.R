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

# Set up initial constraint object
# Here we specify the minimum weight of any asset is -Inf and the maximum
# weight of any asset is Inf. This is essentially an unconstrained GMV portfolio
# We specify the full investment constraint (w' 1 = 1) by setting min_sum=1 
# and max_sum=1.
gen.constr <- constraint(assets=funds, min=-Inf, max=Inf, min_sum=1, max_sum=1)

# Add objective to minimize variance
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="var", enabled=TRUE)

##### Example 1.1: Global Minimum Variance (GMV) Portfolio #####
# Global Minimum variance portfolio
gmv.constr <- gen.constr

# Call the optimizer to minimize portfolio variance
gmv.opt <- optimize.portfolio(R=returns, constraints=gmv.constr, optimize_method="ROI")

# Optimal weights
round(gmv.opt$weights, 3)

# Portfolio standard deviation
sqrt(gmv.opt$out)

# GMV portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=-Inf, max=Inf, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.2: Long Only GMV Portfolio #####
gmv.longonly.constr <- gen.constr

# Set the min and max vectors for long only constraints
min <- rep(0, length(funds))
max <- rep(1, length(funds))

# Modify the min and max vectors in gmv.longonly.constr
gmv.longonly.constr$min <- min
gmv.longonly.constr$max <- max

# Call the optimizer
gmv.longonly.opt <- optimize.portfolio(R=returns, constraints=gmv.longonly.constr, optimize_method="ROI")

# Optimal weights
round(gmv.longonly.opt$weights, 3)

# Portfolio standard deviation
sqrt(gmv.longonly.opt$out)

# GMV long only portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.3: GMV Box Constraints #####
gmv.box.constr <- gen.constr

# Set the min and max vectors for box constraints
# The box constraints are such that the minimum weight of any asset is 0.03
# and the maximum weight of any asset is 0.25
min <- rep(0.03, length(funds))
max <- rep(0.25, length(funds))

# Modify the min and max vectors in gmv.longonly.constr
gmv.box.constr$min <- min
gmv.box.constr$max <- max

# Call the optimizer
gmv.box.opt <- optimize.portfolio(R=returns, constraints=gmv.box.constr, optimize_method="ROI")

# Optimal weights
round(gmv.box.opt$weights, 3)

# Portfolio standard deviation
sqrt(gmv.box.opt$out)

# GMV box constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.3a: GMV Box Constraints #####
gmv.box.constr <- gen.constr

# As an alternative to box constriants, we can also linear inequality
# constraints for the minimum and maximum asset weights

# Set the min and max vectors for box constraints
# The box constraints are such that the minimum weight of any asset is 0.03
# and the maximum weight of any asset is 0.25
min <- c(0.02, 0.02, 0.02, 0.04, 0.05, 0.05, 0.02, 0, 0, 0.1)
max <- c(0.2, 0.4, 0.4, 0.45, 0.3, 0.5, 0.4, 0.4, 0.4, 0.4)

# Modify the min and max vectors in gmv.longonly.constr
gmv.box.constr$min <- min
gmv.box.constr$max <- max

# Mean variance optimization (MVO) seeks to minimize portfolio variance
gmv.box.opt <- optimize.portfolio(R=returns, constraints=gmv.box.constr, optimize_method="ROI")

# Optimal weights
round(gmv.box.opt$weights, 3)

# Portfolio standard deviation
sqrt(gmv.box.opt$out)

##### Example 1.4: GMV long only with Group Constraints #####
# Combine returns from different market cap groups
returns.cap <- cbind(microcap.ts[, 1:2],
                     smallcap.ts[, 1:2],
                     midcap.ts[, 1:2],
                     largecap.ts[, 1:2])

funds.cap <- colnames(returns.cap)

# Set up constraints object for the market caps
lo.group.constr <- constraint(assets=funds.cap, min=0, max=1, min_sum=1, max_sum=1)

# Add group constraints to gmv.box.group.constr
# Market cap constraints
# At least 10% and no more than 25% in micro-caps
# At least 15% and no more than 35% in small-caps
# At least 0% and no more than 35% in mid-caps
# At least 0% and no more than 45% in large-caps
lo.group.constr$groups <- c(2, 2, 2, 2)
lo.group.constr$cLO <- c(0.1, 0.15, 0, 0)
lo.group.constr$cUP <- c(0.25, .35, 0.35, 0.45)

# Add objective to minimize variance
gmv.lo.group.constr <- add.objective(constraints=lo.group.constr, type="risk", name="var", enabled=TRUE)

# Call optimizer
gmv.lo.group.opt <- optimize.portfolio(R=returns.cap, constraints=gmv.lo.group.constr, optimize_method="ROI")

# Optimal weights
round(gmv.lo.group.opt$weights, 3)

# Group weights
gmv.lo.group.opt$weights[c(1, 3, 5, 7)] + gmv.lo.group.opt$weights[c(2, 4, 6, 8)]

# GMV group constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds.cap)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="group", enabled=TRUE,
                        groups=c(2, 2, 2, 2),
                        group_min=c(0.1, 0.15, 0, 0), 
                        group_max=c(0.25, .35, 0.35, 0.45),
                        group_labels=c("MICRO", "SMALL", "MID", "LARGE"))
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns.cap, portfolio=pspec, optimize_method="ROI")

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

# In the previous examples, we were solving global minimum variance with optmize_method="ROI". 
# The solve.QP plugin is selected automatically by optimize.portfolio when "var" is the objective

##### Example 1.6: Maximize mean-return with box constraints #####
# Set up initial constraint object
# Here we specify the minimum weight of any asset is 0.03 and the maximum weight of any asset is 0.25
# We specify the full investment constraint (w' 1 = 1) by setting min_sum=1 and max_sum=1
gen.constr <- constraint(assets=funds, min=0.03, max=0.25, min_sum=1, max_sum=1)

# Add objective to maximize return
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=TRUE)

maxret.constr <- gen.constr

# Call optimizer to maximize return subject to given constraints
maxret.opt <- optimize.portfolio(R=returns, constraints=maxret.constr, optimize_method="ROI")

# Optimal weights
maxret.opt$weights

# Maximize mean return with box constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio standard deviation
sqrt(opt$out)

##### Example 1.7 Maximize mean-return Long Only with Group Constraints #####
# Re-use lo.group.constr from Example 1.5
maxret.lo.group.constr <- lo.group.constr

maxret.lo.group.constr <- add.objective(constraints=maxret.lo.group.constr, type="return", name="mean", enabled=TRUE)

maxret.lo.group.opt <- optimize.portfolio(R=returns.cap, constraints=maxret.lo.group.constr, optimize_method="ROI")

# Optimal weights
maxret.lo.group.opt$weights

# Group weights
maxret.lo.group.opt$weights[c(1, 3, 5, 7)] + maxret.lo.group.opt$weights[c(2, 4, 6, 8)]

# GMV group constraints portfolio using new interface
pspec <- portfolio.spec(assets=funds.cap)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="group", enabled=TRUE,
                        groups=c(2, 2, 2, 2),
                        group_min=c(0.1, 0.15, 0, 0), 
                        group_max=c(0.25, .35, 0.35, 0.45),
                        group_labels=c("MICRO", "SMALL", "MID", "LARGE"))
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns.cap, portfolio=pspec, optimize_method="ROI")

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

##### Example 1.X: Maximize Quadratic Utility #####
# Quadratic utility maximize return penalizing variance
qu.constr <- constraint(assets=funds, min=0, max=1, min_sum=1, max_sum=1)

# Add mean return as an objective
qu.constr <- add.objective(constraints=qu.constr, type="return", name="mean", enabled=TRUE)

# Add variance as an objective
qu.constr <- add.objective(constraints=qu.constr, type="risk", name="var", enabled=TRUE, risk_aversion=20)

qu.opt <- optimize.portfolio(R=returns, constraints=qu.constr, optimize_method="ROI")

wts1 <- round(qu.opt$weights, 4)
wts1

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

all.equal(wts1, wts2)

# Note that target mean return CANNOT be specified as a constraint currently
# It is specified as a target in the return objective
# Can do quadratic utility optimization with target return

##### Example 1.X: Maximize Quadratic Utility #####
# Quadratic utility maximize return penalizing variance
qu.constr <- constraint(assets=funds, min=0, max=1, min_sum=1, max_sum=1)

# Add mean return as an objective
qu.constr <- add.objective(constraints=qu.constr, type="return", name="mean", target=0.025, enabled=TRUE)

# Add variance as an objective
# Set risk aversion parameter high to approximate mvo
qu.constr <- add.objective(constraints=qu.constr, type="risk", name="var", enabled=TRUE, risk_aversion=1e6)

qu.opt <- optimize.portfolio(R=returns, constraints=qu.constr, optimize_method="ROI")

round(qu.opt$weights, 4)

##### Example X: Mean Variance Optimization (MVO) with target mean return constraint #####

# MVO with target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=-Inf, max=Inf, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)


##### Example X: Mean Variance Optimization (MVO) with target mean return and long only constraints #####

# MVO with long only and target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)

##### Example X: Mean Variance Optimization (MVO) with target mean return and box constraints #####

# MVO with box constraints and target mean return
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="var", risk_aversion=1e6, enabled=TRUE)
pspec <- add.objective_v2(portfolio=pspec, type="return", name="mean", target=0.014, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

# Portfolio return
t(opt$weights) %*% colMeans(returns)

##### Example X: ETL Long Only #####

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0, max=1, enabled=TRUE)
# This can be specified with ETL, ES, or CVaR for name
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

# Optimal weights
round(opt$weights, 3)

##### Example X: ETL with box constraints #####

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=TRUE)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.03, max=0.25, enabled=TRUE)
# This can be specified with ETL, ES, or CVaR for name
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns, portfolio=pspec, optimize_method="ROI")

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
pspec <- add.objective_v2(portfolio=pspec, type="risk", name="ETL", alpha=0.05, enabled=TRUE)

opt <- optimize.portfolio_v2(R=returns.cap, portfolio=pspec, optimize_method="ROI")

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

