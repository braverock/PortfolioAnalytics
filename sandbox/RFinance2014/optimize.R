# script used to run the portfolio optimizations

# Examples to consider
# Example 1: Consider a portfolio of stocks. Full investment and long 
# only (or box) constraints. Objective to minimize portfolio variance. 
# Demonstrate a custom moments function to compare a sample covariance 
# matrix estimate and a robust covariance matrix estimate. An alternative 
# to a MCD estimate is ledoit-wolf shrinkage, DCC GARCH model, 
# factor model, etc.

# Example 2: Consider a portfolio of stocks. Dollar neutral, beta
# neutral, box constraints, and leverage_exposure constraints. Objective
# to minimize portfolio StdDev. This will demonstrate some of the 
# more advanced constraint types. Could also introduce position limit
# constraints here in this example. 

# Example 3: Consider an allocation to hedge funds using the 
# EDHEC-Risk Alternative Index as a proxy. This will be an extended
# example starting with an objective to minimize portfolio expected
# shortfall, then risk budget percent contribution limit, then equal 
# risk contribution limit. 

# Example 4: Consider an allocation to hedge funds using the 
# EDHEC-Risk Alternative Index as a proxy. 

# Option 1 for example 4
# Objective to maximize a risk adjusted return measure 
# (e.g.Calmar Ratio, Sterling Ratio, Sortino Ratio, or Upside Potential 
# Ratio)

# I prefer doing this option
# Option 2 for example 4
# Objective to maximize the
# fourth order expansion of the Constant Relative Risk Aversion (CRRA)
# expected utility function. Demonstrate a custom moment function and
# a custom objective function.

# Load the packages
library(PortfolioAnalytics)
library(foreach)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# Source in the lwShrink function
source("R/lwShrink.R")

# Example 1 and Example 2 will use the crsp_weekly data
# load the CRSP weekly data
load("data/crsp_weekly.rda")

# Example 3 and Example 4 will use the edhec data
# Load the updated edhec dataset
load("data/edhec.rda")


# Prep data for Examples 1 and 2
# use the first 10 stocks in largecap_weekly, midcap_weekly, and smallcap_weekly
N <- 10
equity.data <- cbind(largecap_weekly[,1:N], 
                     midcap_weekly[,1:N], 
                     smallcap_weekly[,1:N])
market <- largecap_weekly[,21]
Rf <- largecap_weekly[,22]
stocks <- colnames(equity.data)

# Specify an initial portfolio
portf.init <- portfolio.spec(stocks)

##### Example 1 #####
# Add constraints
# weights sum to 1
portf.minvar <- add.constraint(portf.init, type="full_investment")
# box constraints such that no stock has weight less than 1% or greater than 20%
portf.minvar <- add.constraint(portf.minvar, type="box", min=0.01, max=0.2)

# Add objective
# objective to minimize portfolio variance
portf.minvar <- add.objective(portf.minvar, type="risk", name="var")

# Rebalancing parameters
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 400
# Trailing Period
trailing <- 250

# Run optimization
# Sample Covariance Matrix Estimate

# By default, momentFUN uses set.portfolio.moments which computes the sample
# moment estimates

opt.minVarSample <- optimize.portfolio.rebalancing(equity.data, portf.minvar, 
                                                   optimize_method="ROI", 
                                                   rebalance_on=rebal.freq, 
                                                   training_period=training, 
                                                   trailing_periods=trailing)

# Custom moment function to use Ledoit-Wolf shinkage covariance matrix estimate
lw.sigma <- function(R, ...){
  out <- list()
  # estimate covariance matrix via robust covariance matrix estimate, 
  # ledoit-wolf shrinkage, GARCH, factor model, etc.
  # set.seed(1234)
  # out$sigma <- MASS::cov.rob(R, method="mcd", ...)$cov
  out$sigma <- lwShrink(R)$cov
  #print(index(last(R)))
  return(out)
}

# Using Ledoit-Wolf Shrinkage Covariance Matrix Estimate
opt.minVarLW <- optimize.portfolio.rebalancing(equity.data, portf.minvar, 
                                               optimize_method="ROI", 
                                               momentFUN=lw.sigma,
                                               rebalance_on=rebal.freq, 
                                               training_period=training, 
                                               trailing_periods=trailing)

# Chart the weights through time
chart.Weights(opt.minVarSample, main="minVarSample Weights")
chart.Weights(opt.minVarLW, main="minVarLW Weights")

# Compute and chart the performance summary
ret.minVarSample <- summary(opt.minVarSample)$portfolio_returns
ret.minVarRobust <- summary(opt.minVarLW)$portfolio_returns
ret.minVar <- cbind(ret.minVarSample, ret.minVarRobust)
colnames(ret.minVar) <- c("Sample", "LW")
charts.PerformanceSummary(ret.minVar)

##### Example 2 #####
portf.init <- portfolio.spec(stocks, 
                             weight_seq=generatesequence(min=-0.2, max=0.2, by=0.001))

# weights sum to 0
portf.dn <- add.constraint(portf.init, type="weight_sum", 
                                  min_sum=-0.01, max_sum=0.01)

# Add box constraints such that no stock has weight less than -20% or 
# greater than 20%
portf.dn <- add.constraint(portf.dn, type="box", 
                                  min=-0.2, max=0.2)
# Add position limit constraint such that the portfolio has a maximum
# of 20 non-zero positions
portf.dn <- add.constraint(portf.dn, type="position_limit", max_pos=20)

# Compute the betas of each stock
betas <- t(CAPM.beta(equity.data, market, Rf))

# Add factor exposure constraint to limit portfolio beta
portf.dn <- add.constraint(portf.dn, type="factor_exposure", B=betas, 
                           lower=-0.5, upper=0.5)
# portf.dn <- add.constraint(portf.dn, type="leverage_exposure", leverage=2)

rp <- random_portfolios(portf.dn, 10000, eliminate=TRUE)
dim(rp)

# Add objective to target return of 0.001
portf.dn.StdDev <- add.objective(portf.dn, type="return", name="mean", 
                                 target=0.001)
# Add objective to minimize portfolio variance
portf.dn.StdDev <- add.objective(portf.dn.StdDev, type="risk", name="StdDev")

# Run optimization
opt <- optimize.portfolio(equity.data, portf.dn.StdDev, 
                          optimize_method="random", rp=rp,
                          trace=TRUE)

plot(opt, risk.col="StdDev", neighbors=10)

# chart.RiskReward(opt, risk.col="StdDev", neighbors=25)
# chart.Weights(opt, plot.type="bar", legend.loc=NULL)
# wts <- extractWeights(opt)
# t(wts) %*% betas
# sum(abs(wts))
# sum(wts[wts > 0])
# sum(wts[wts < 0])
# sum(wts != 0)

# Prep data for Examples 3 and 4
# For now, use the first 8 
R <- edhec[,1:8]
# Abreviate column names for convenience and plotting
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQN", "ED", "FA", "GM")
funds <- colnames(R)

##### Example 3 #####
# Example 3 will consider three portfolios
# - minES
# - minES with 30% component contribution limit
# - minES with equal risk contribution

portf.init <- portfolio.spec(funds)
portf.init <- add.constraint(portf.init, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)

portf.init <- add.constraint(portf.init, type="box", 
                             min=0.05, max=0.4)

# Set multiplier=0 so that it is calculated, but does not affect the optimization
portf.init <- add.objective(portf.init, type="return", 
                            name="mean", multiplier=0)

# Add objective to minimize expected shortfall
portf.minES <- add.objective(portf.init, type="risk", name="ES")

# Add risk budget objective with upper limit on percentage contribution
portf.minES.RB <- add.objective(portf.minES, type="risk_budget", 
                                name="ES", max_prisk=0.2)

# Relax the box constraint
portf.minES.RB$constraints[[2]]$max <- rep(1,ncol(R))
# print.default(portf.minES.RB$constraints[[2]])

# Add risk budget objective to minimize concentration of percentage component
# contribution to risk. Concentration is defined as the Herfindahl-Hirschman
# Index (HHI). $\sum_{i=1}^n x_i^2$
portf.minES.EqRB <- add.objective(portf.minES, type="risk_budget", 
                                  name="ES", min_concentration=TRUE)
# relax the box constraint
portf.minES.EqRB <- add.constraint(portf.minES.EqRB, type="box", 
                                   min=0.05, max=1, indexnum=2)
# portf.minES.RB$constraints[[2]]$max <- rep(1,ncol(R))
# print.default(portf.minES.EqRB$constraints[[2]])

# Combine the portfolios so we can make a single call to 
# optimize.portfolio
portf <- combine.portfolios(list(minES=portf.minES, 
                                 minES.RB=portf.minES.RB, 
                                 minES.EqRB=portf.minES.EqRB))

# Run the optimization
opt.minES <- optimize.portfolio(R, portf, optimize_method="DEoptim", 
                                search_size=2000, trace=TRUE, traceDE=0,
                                message=TRUE)

# ES(R, portfolio_method="component", weights=extractWeights(opt.minES[[1]]))
# extractObjectiveMeasures(opt.minES)

# extract objective measures, out, and weights 
xtract <- extractStats(opt.minES)

# get the 'mean' and 'ES' columns from each element of the list
xtract.mean <- unlist(lapply(xtract, function(x) x[,"mean"]))
xtract.ES <- unlist(lapply(xtract, function(x) x[,"ES"]))

# plot the feasible space
par(mar=c(7,4,4,1)+0.1)
plot(xtract.ES, xtract.mean, col="gray", 
     xlab="ES", ylab="Mean",
     xlim=c(0, max(xtract.ES)))

# min ES
points(x=opt.minES[[1]]$objective_measures$ES,
       y=opt.minES[[1]]$objective_measures$mean,
       pch=15, col="purple")
text(x=opt.minES[[1]]$objective_measures$ES,
     y=opt.minES[[1]]$objective_measures$mean,
     labels="Min ES", pos=4, col="purple", cex=0.8)

# min ES with risk budget upper limit on component contribution to risk
points(x=opt.minES[[2]]$objective_measures$ES$MES,
       y=opt.minES[[2]]$objective_measures$mean,
       pch=15, col="black")
text(x=opt.minES[[2]]$objective_measures$ES$MES,
     y=opt.minES[[2]]$objective_measures$mean,
     labels="Min ES RB", pos=4, col="black", cex=0.8)

# min ES with equal (i.e. min concentration) component contribution to risk
points(x=opt.minES[[3]]$objective_measures$ES$MES,
       y=opt.minES[[3]]$objective_measures$mean,
       pch=15, col="darkgreen")
text(x=opt.minES[[3]]$objective_measures$ES$MES,
     y=opt.minES[[3]]$objective_measures$mean,
     labels="Min ES EqRB", pos=4, col="darkgreen", cex=0.8)

# Chart the risk contribution
chart.RiskBudget(opt.minES[[1]], risk.type="percentage", neighbors=10)
chart.RiskBudget(opt.minES[[2]], risk.type="percentage", neighbors=10)
chart.RiskBudget(opt.minES[[3]], risk.type="percentage", neighbors=10)

# Now we want to evaluate portfolio through time

# Add risk budget objective to minES portfolio with multiplier=0 so that it
# is calculated, but does not affect optimization
portf.minES <- add.objective(portf.minES, type="risk_budget", 
                             name="ES", multiplier=0)

# Rebalancing parameters
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 120
# Trailing Period
trailing <- 72

# Backtest
bt.opt.minES <- optimize.portfolio.rebalancing(R, portf,
                                               optimize_method="DEoptim", 
                                               rebalance_on=rebal.freq, 
                                               training_period=training, 
                                               trailing_periods=trailing,
                                               traceDE=0, message=TRUE)

# Plot the risk contribution through time
chart.RiskBudget(bt.opt.minES[[1]], risk.type="percentage")
chart.RiskBudget(bt.opt.minES[[2]], risk.type="percentage")
chart.RiskBudget(bt.opt.minES[[3]], risk.type="percentage")

# Extract the returns from each element and chart the performance summary
ret.bt.opt <- do.call(cbind, lapply(bt.opt.minES, function(x) summary(x)$portfolio_returns))
colnames(ret.bt.opt) <- c("min ES", "min ES RB", "min ES Eq RB")
head(ret.bt.opt)
charts.PerformanceSummary(ret.bt.opt)

##### Example 4 #####

# CRRA 4th order expansion expected utility
# PerformanceAnalytics for moments
# M3.MM
# M4.MM
# StdDev.MM
# skewness.MM
# kurtosis.MM

# Simple function to compute the moments used in CRRA
custom.moments <- function(R, ...){
  out <- list()
  out$sigma <- cov(R)
  out$m3 <- PerformanceAnalytics:::M3.MM(R)
  out$m4 <- PerformanceAnalytics:::M4.MM(R)
  out
}


# Fourth order expansion of CRRA expected utility
CRRA <- function(R, weights, lambda, sigma, m3, m4){
  M2.w <- t(weights) %*% sigma %*% weights
  M3.w <- t(weights) %*% m3 %*% (weights %x% weights)
  M4.w <- t(weights) %*% m4 %*% (weights %x% weights %x% weights)
  term1 <- 0.5 * lambda * M2.w
  term2 <- (1 / 6) * lambda * (lambda + 1) * M3.w
  term3 <- (1 / 24) * lambda * (lambda + 1) * (lambda + 2) * M4.w
  out <- -term1 + term2 - term3
  out
}

# test the CRRA function
portf.tmp <- portfolio.spec(funds)
portf.tmp <- add.constraint(portf.tmp, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)

portf.tmp <- add.constraint(portf.tmp, type="box", 
                             min=0.05, max=0.4)

# Set multiplier=0 so that it is calculated, but does not affect the optimization
portf.tmp <- add.objective(portf.tmp, type="return", 
                            name="CRRA", arguments=list(lambda=5))

momentargs <- custom.moments(R)
constrained_objective(weights, R, portf.tmp, env=momentargs)


# # Calculate the turnover per period
# turnover.rebalancing <- function(object){
#   weights <- extractWeights(object)
#   n <- nrow(weights)
#   out <- vector("numeric", n)
#   out[1] <- NA
#   for(i in 2:n){
#     out[i] <- out[i] <- sum(abs(as.numeric(weights[i,]) - as.numeric(weights[i-1,])))
#   }
#   xts(out, index(weights))
# }
# 
# # Calculate the diversification per period
# diversification.rebalancing <- function(object){
#   weights <- extractWeights(object)
#   n <- nrow(weights)
#   out <- vector("numeric", n)
#   for(i in 1:n){
#     out[i] <- 1 - sum(weights[i,]^2)
#   }
#   xts(out, index(weights))
# }
