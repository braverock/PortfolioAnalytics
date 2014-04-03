# script used to run the portfolio optimizations

library(PortfolioAnalytics)
# Load the updated edhec dataset
load("data/edhec.rda")

# For now, use the first 8
R <- edhec[,1:8]
# Abreviate column names for convenience and plotting
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQN", "ED", "FA", "GM")
funds <- colnames(R)

# Example 1
# Box constraints, minimum variance portfolio
# specify portfolio
init <- portfolio.spec(funds)

# Add constraints
port1 <- add.constraint(init, type="full_investment")
port1 <- add.constraint(port1, type="box", min=0.05, max=0.6)

# Add objective
port1 <- add.objective(port1, type="risk", name="var")

# Custom moment function to use 
robust.sigma <- function(R, ...){
  out <- list()
  set.seed(1234)
  out$sigma <- MASS::cov.rob(R, method="mcd", ...)$cov
  return(out)
}

# Rebalancing parameters
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 120
# Trailing Period
trailing <- 72

# Run optimization
# Sample Covariance Matrix Estimate
opt.minVarSample <- optimize.portfolio.rebalancing(R, port1, 
                                                   optimize_method="ROI", 
                                                   rebalance_on=rebal.freq, 
                                                   training_period=training, 
                                                   trailing_periods=trailing)
ret.minVarSample <- summary(opt.minVarSample)$portfolio_returns


# MCD Covarinace Matrix Estimate
opt.minVarRobust <- optimize.portfolio.rebalancing(R, port1, 
                                                   optimize_method="ROI", 
                                                   momentFUN=robust.sigma,
                                                   rebalance_on=rebal.freq, 
                                                   training_period=training, 
                                                   trailing_periods=trailing)

# Chart the weights
chart.Weights(opt.minVarSample, main="minVarSample Weights")
chart.Weights(opt.minVarRobust, main="minVarRobust Weights")

# Calculate the turnover per period
turnover.rebalancing <- function(object){
  weights <- extractWeights(object)
  n <- nrow(weights)
  out <- vector("numeric", n)
  out[1] <- NA
  for(i in 2:n){
    out[i] <- out[i] <- sum(abs(as.numeric(weights[i,]) - as.numeric(weights[i-1,])))
  }
  xts(out, index(weights))
}

# Compute the average turnover
to.minVarSample <- mean(turnover.rebalancing(opt.minVarSample), na.rm=TRUE)
to.minVarRobust <- mean(turnover.rebalancing(opt.minVarRobust), na.rm=TRUE)


# Calculate the diversification per period
diversification.rebalancing <- function(object){
  weights <- extractWeights(object)
  n <- nrow(weights)
  out <- vector("numeric", n)
  for(i in 1:n){
    out[i] <- 1 - sum(weights[i,]^2)
  }
  xts(out, index(weights))
}

# Compute the average diversification
div.minVarSample <- mean(diversification.rebalancing(opt.minVarSample))
div.minVarRobust <- mean(diversification.rebalancing(opt.minVarRobust))

# Compute the returns
ret.minVarSample <- summary(opt.minVarSample)$portfolio_returns
ret.minVarRobust <- summary(opt.minVarRobust)$portfolio_returns
ret.minVar <- cbind(ret.minVarSample, ret.minVarRobust)
colnames(ret.minVar) <- c("Sample", "Robust")
charts.PerformanceSummary(ret.minVar)

## Example 2

# Example 2 will consider three portfolios
# - meanES
# - meanES with 30% component contribution limit
# - meanES equal risk contribution

# meanES
# Add constraints
port2 <- add.constraint(init, type="full_investment")
port2 <- add.constraint(port2, type="box", min=0, max=0.6)

# Add objectives
port2 <- add.objective(port2, type="return", name="mean")
port2 <- add.objective(port2, type="risk", name="ES", 
                       arguments=list(p=0.92, clean="boudt"))

opt.MeanES.ROI <- optimize.portfolio(R, port2, optimize_method="ROI", trace=TRUE)
plot(opt.MeanES.ROI)

# relax the constraints for random portfolio
port2$constraints[[1]]$min_sum <- 0.99
port2$constraints[[1]]$max_sum <- 1.01

search.size <- 20000

set.seed(123)
rp <- random_portfolios(port2, permutations=search.size)

#set.seed(123)
#rp1 <- random_portfolios(port2, permutations=search.size)
#all.equal(rp, rp1)


opt.MeanES.RP <- optimize.portfolio(R, port2, optimize_method="random", 
                                    rp=rp, trace=TRUE)
#extractObjectiveMeasures(combine.optimizations(list(opt.MeanES.ROI, opt.MeanES.RP)))
#extractWeights(combine.optimizations(list(opt.MeanES.ROI, opt.MeanES.RP)))

plot(opt.MeanES.RP, neighbors=25)

# Calculate the component contribution to risk
portContribES <- ES(R, p=0.92, portfolio_method="component", 
                    weights=extractWeights(opt.MeanES.RP))
portContribES$pct_contrib_MES

# Now suppose we want to place limits on percent component contribution to risk
port3 <- add.objective(port2, type="risk_budget", name="ES", 
                       arguments=list(p=0.92, clean="boudt"), max_prisk=0.35)

opt.MeanES.RB <- optimize.portfolio(R, port3, optimize_method="random", 
                                   trace=TRUE, rp=rp)
opt.MeanES.RB

chart.RiskBudget(opt.MeanES.RB, risk.type="percentage", neighbors=25)


port4 <- add.objective(port2, type="risk_budget", name="ES", 
                       arguments=list(p=0.92, clean="boudt"), 
                       min_concentration=TRUE)
opt.MeanES.EqRB <- optimize.portfolio(R, port4, optimize_method="random", 
                                   trace=TRUE, rp=rp)
opt.MeanES.EqRB
chart.RiskBudget(opt.MeanES.EqRB, risk.type="percentage", neighbors=25)

# plot
# - opt.meanES.ROI
# - opt.meanES.RP
# - opt.meanES.RB
# - opt.meanES.EqRB

xtract <- extractStats(opt.MeanES.RP)

# plot the feasible space
par(mar=c(6,4,4,1)+0.1)
plot(xtract[,"ES"], xtract[,"mean"], col="gray", 
     xlab="ES", ylab="Mean",
     xlim=c(0, max(xtract[,"ES"])))

# opt.MeanES.ROI
points(x=opt.MeanES.ROI$objective_measures$ES,
       y=opt.MeanES.ROI$objective_measures$mean,
       pch=15, col="blue")
text(x=opt.MeanES.ROI$objective_measures$ES,
       y=opt.MeanES.ROI$objective_measures$mean,
       labels="Mean ES ROI", pos=4, col="blue", cex=0.8)

# opt.MeanES.RP
points(x=opt.MeanES.RP$objective_measures$ES,
       y=opt.MeanES.RP$objective_measures$mean,
       pch=15, col="purple")
text(x=opt.MeanES.RP$objective_measures$ES,
     y=opt.MeanES.RP$objective_measures$mean,
     labels="Mean ES RP", pos=1, col="purple", cex=0.8)

# opt.MeanES.RB
points(x=opt.MeanES.RB$objective_measures$ES$MES,
       y=opt.MeanES.RB$objective_measures$mean,
       pch=15, col="black")
text(x=opt.MeanES.RB$objective_measures$ES$MES,
     y=opt.MeanES.RB$objective_measures$mean,
     labels="Mean ES RB", pos=4, col="black", cex=0.8)

# opt.MeanES.EqRB
points(x=opt.MeanES.EqRB$objective_measures$ES$MES,
       y=opt.MeanES.EqRB$objective_measures$mean,
       pch=15, col="darkgreen")
text(x=opt.MeanES.EqRB$objective_measures$ES$MES,
     y=opt.MeanES.EqRB$objective_measures$mean,
     labels="Mean ES EqRB", pos=4, col="darkgreen", cex=0.8)


# Backtest these three portfolios
# I'm going to add a risk budget object to port2 with multiplier=0 so that
# it is calculated, but does not affect the optimization
port2 <- add.objective(port2, name="ES", type="risk_budget", arguments=list(p=0.92), multiplier=0)


# Rebalancing parameters
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 120
# Trailing Period
trailing <- 72

bt.opt.MeanES <- optimize.portfolio.rebalancing(R, port2, rp=rp,
                                                optimize_method="random", 
                                                rebalance_on=rebal.freq, 
                                                training_period=training, 
                                                trailing_periods=trailing)
chart.RiskBudget(bt.opt.MeanES, main="Mean ES", risk.type="percentage")

bt.opt.MeanES.RB <- optimize.portfolio.rebalancing(R, port3, rp=rp,
                                                   optimize_method="random", 
                                                   rebalance_on=rebal.freq, 
                                                   training_period=training, 
                                                   trailing_periods=trailing)
chart.RiskBudget(bt.opt.MeanES.RB, main="Mean-ES 30% Limit", 
                 risk.type="percentage")


bt.opt.MeanES.EqRB <- optimize.portfolio.rebalancing(R, port4, rp=rp,
                                                     optimize_method="random", 
                                                     rebalance_on=rebal.freq, 
                                                     training_period=training, 
                                                     trailing_periods=trailing)
chart.RiskBudget(bt.opt.MeanES.EqRB, main="Mean-ES Equal Risk", 
                 risk.type="percentage")

# pass in a portfolio.list instead of typing this 3 times

# calculate the returns
ret.MeanES <- summary(bt.opt.MeanES)$portfolio_returns
ret.MeanES.RB <- summary(bt.opt.MeanES.RB)$portfolio_returns
ret.MeanES.EqRB <- summary(bt.opt.MeanES.EqRB)$portfolio_returns

# Combine the returns
ret <- cbind(ret.MeanES, ret.MeanES.RB, ret.MeanES.EqRB)
colnames(ret) <- c("Mean.ES", "MeanES.RB", "MeanES.EqRB")
charts.PerformanceSummary(ret)

# CRRA 4th order expansion expected utility
# look in PerformanceAnalytics
# M3.MM
# M4.MM
# StdDev.MM
# skewness.MM
# kurtosis.MM

