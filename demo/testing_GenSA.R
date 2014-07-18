#' ---
#' title: "GenSA Demo"
#' date: "7/17/2014"
#' ---

#' This script demonstrates running optimizations using GenSA as the 
#' optimization backend. Note that this script uses the v1 specification
#' previous to version 0.8.3.

#' Load packages
library(PortfolioAnalytics)
library(GenSA)

#' Load data and set general Parameters for sample code
data(edhec)
N <- 4
R <- edhec[,1:N]
funds <- names(R)
mu.port <- mean(colMeans(R))

#' Define problem with constraints and objectives
gen.constr <- constraint(assets = funds, min=-2, max=2, min_sum=0.99, max_sum=1.01, risk_aversion=1)
gen.constr <- add.objective(constraints=gen.constr, type="return", name="mean", enabled=FALSE, target=mu.port)
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="var", enabled=FALSE, risk_aversion=10)
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="CVaR", enabled=FALSE)
gen.constr <- add.objective(constraints=gen.constr, type="risk", name="sd", enabled=FALSE)

#' Max return under box constraints, fully invested
max.port <- gen.constr
max.port$min <- rep(0.01,N)
max.port$max <- rep(0.30,N)
max.port$objectives[[1]]$enabled <- TRUE
max.port$objectives[[1]]$target <- NULL
max.port$objectives[[1]]$multiplier  <- -1
max.solution <- optimize.portfolio(R=R, constraints=max.port, optimize_method="GenSA", trace=TRUE)

#' Mean-variance:  Fully invested, Global Minimum Variance Portfolio
gmv.port <- gen.constr
gmv.port$objectives[[4]]$enabled <- TRUE
gmv.solution <- optimize.portfolio(R=R, constraints=gmv.port, optimize_method="GenSA", trace=TRUE)

#' Minimize CVaR with target return
cvar.port <- gen.constr
cvar.port$min <- rep(0,N)
cvar.port$max <- rep(1,N)
cvar.port$objectives[[3]]$enabled <- TRUE
cvar.port$objectives[[3]]$arguments <- list(p=0.95, clean="boudt")
cvar.solution <- optimize.portfolio(R=R, constraints=cvar.port, optimize_method="GenSA")

