# # # # # # # # # # # # # # # # #
#  OPTIMIZATION TESTING: ROI
#

library(xts)
library(quadprog)
library(Rglpk)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(Ecdat)
library(PortfolioAnalytics)


# General Parameters for sample code
data(edhec)
funds <- names(edhec)
mu.port <- mean(colMeans(edhec))
N <- length(funds)

gen.constr <- constraint(assets = colnames(edhec), min=-Inf, max =Inf, min_sum=1, max_sum=1, risk_aversion=1)
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=FALSE, multiplier=0, target=mu.port)
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="var", enabled=FALSE, multiplier=0, risk_aversion=10)
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="CVaR", enabled=FALSE, multiplier=0)


# =====================
# Max return under box constraints, fully invested
#
max.port <- gen.constr
max.port$min <- rep(0.01,N)
max.port$max <- rep(0.30,N)
max.port$objectives[[1]]$enabled <- TRUE
max.port$objectives[[1]]$target <- NULL
max.solution <- optimize.portfolio(edhec, max.port, "ROI")


# =====================
# Mean-variance:  Fully invested, Global Minimum Variance Portfolio
#
gmv.port <- gen.constr
gmv.port$objectives[[2]]$enabled <- TRUE
gmv.port$objectives[[2]]$risk_aversion <- 1
gmv.solution <- optimize.portfolio(edhec, gmv.port, "ROI")


# ========================
# Mean-variance:  Maximize quadratic utility, fully invested, target portfolio return
#
target.port <- gen.constr
target.port$objectives[[1]]$enabled <- TRUE
target.port$objectives[[2]]$enabled <- TRUE
target.solution <- optimize.portfolio(edhec, target.port, "ROI")


# ========================
# Mean-variance:  Maximize quadratic utility, dollar-neutral, target portfolio return
#
dollar.neu.port <- gen.constr
dollar.neu.port$min_sum <- 0
dollar.neu.port$max_sum <- 0
dollar.neu.port$objectives[[1]]$enabled <- TRUE
dollar.neu.port$objectives[[2]]$enabled <- TRUE
dollar.neu.solution <- optimize.portfolio(edhec, dollar.neu.port, "ROI")


# ========================
# Minimize CVaR with target return
#
cvar.port <- gen.constr
cvar.port$objectives[[1]]$enabled <- TRUE
cvar.port$objectives[[3]]$enabled <- TRUE
cvar.solution <- optimize.portfolio(edhec, cvar.port, "ROI")


# =====================
# Mean-variance:  Fully invested, Global Minimum Variance Portfolio, Groups Constraints
#
groups.port <- gen.constr
groups <- c(3,3,3,4)
groups.port$groups <- groups 
groups.port$cLO <- rep(0.15,length(groups))
groups.port$cUP <- rep(0.30,length(groups)) 
groups.port$objectives[[2]]$enabled <- TRUE
groups.port$objectives[[2]]$risk_aversion <- 1
groups.solution <- optimize.portfolio(edhec, groups.port, "ROI")


# ========================
# Minimize CVaR with target return and group constraints
#
group.cvar.port <- gen.constr
groups <- c(3,3,3,4)
group.cvar.port$groups <- groups
group.cvar.port$cLO <- rep(0.15,length(groups))
group.cvar.port$cUP <- rep(0.30,length(groups))
group.cvar.port$objectives[[1]]$enabled <- TRUE
group.cvar.port$objectives[[3]]$enabled <- TRUE
group.cvar.solution <- optimize.portfolio(edhec, group.cvar.port, "ROI")

