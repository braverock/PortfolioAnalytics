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

# Set the directory to save the optimization results
results.dir <- "optimization_results"

# mix of blue, green, and red hues
my_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")

# Load the packages
library(PortfolioAnalytics)
library(foreach)
library(ROI)
library(ROI.plugin.quadprog)

# for running via Rscript
library(methods)

# Source in the lwShrink function
source("R/lwShrink.R")

# Example 1 and Example 2 will use the crsp_weekly data
# Example 3 and Example 4 will use the edhec data
source("data_prep.R")


##### Example 1 #####
stocks <- colnames(equity.data)
# Specify an initial portfolio
portf.init <- portfolio.spec(stocks)
# Add constraints
# weights sum to 1
portf.minvar <- add.constraint(portf.init, type="full_investment")
# box constraints
portf.minvar <- add.constraint(portf.minvar, type="box", min=0.01, max=0.45)

# Add objective
# objective to minimize portfolio variance
portf.minvar <- add.objective(portf.minvar, type="risk", name="var")

# Backtesting parameters
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

cat("Example 1: running minimum variance with sample covariance matrix 
    estimate backtest\n")
if(file.exists(paste(results.dir, "opt.minVarSample.rda", sep="/"))){
  cat("file already exists\n")
} else {
  opt.minVarSample <- optimize.portfolio.rebalancing(equity.data, portf.minvar, 
                                                     optimize_method="ROI", 
                                                     rebalance_on=rebal.freq, 
                                                     training_period=training, 
                                                     trailing_periods=trailing)
  cat("opt.minVarSample complete. Saving results to ", results.dir, "\n") 
  save(opt.minVarSample, file=paste(results.dir, "opt.minVarSample.rda", sep="/"))
}

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

cat("Example 1: running minimum variance with Ledoit-Wolf shrinkage covariance 
    matrix estimate backtest\n")
if(file.exists(paste(results.dir, "opt.minVarLW.rda", sep="/"))){
  cat("file already exists\n")
} else{
  # Using Ledoit-Wolf Shrinkage Covariance Matrix Estimate
  opt.minVarLW <- optimize.portfolio.rebalancing(equity.data, portf.minvar, 
                                                 optimize_method="ROI", 
                                                 momentFUN=lw.sigma,
                                                 rebalance_on=rebal.freq, 
                                                 training_period=training, 
                                                 trailing_periods=trailing)
  cat("opt.minVarLW complete. Saving results to ", results.dir, "\n") 
  save(opt.minVarLW, file=paste(results.dir, "opt.minVarLW.rda", sep="/"))
}

##### Example 2 #####
portf.init <- portfolio.spec(stocks)

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
                           lower=-0.25, upper=0.25)
# portf.dn <- add.constraint(portf.dn, type="leverage_exposure", leverage=2)

# generate random portfolios
if(file.exists(paste(results.dir, "rp.rda", sep="/"))){
  cat("random portfolios already generated\n")
} else {
  cat("generating random portfolios\n")
  rp <- random_portfolios(portf.dn, 10000, eliminate=TRUE)
  cat("random portfolios generated. Saving rp to ", results.dir, "\n") 
  save(rp, file=paste(results.dir, "rp.rda", sep="/"))
}

# Add objective to maximize return
portf.dn.StdDev <- add.objective(portf.dn, type="return", name="mean",
                                 target=0.0015)
# Add objective to target a portfolio standard deviation of 2%
portf.dn.StdDev <- add.objective(portf.dn.StdDev, type="risk", name="StdDev",
                                 target=0.02)

cat("Example 2: running dollar neutral optimization\n")
if(file.exists(paste(results.dir, "opt.dn.rda", sep="/"))){
  cat("file already exists\n")
} else {
  # Run optimization
  opt.dn <- optimize.portfolio(equity.data, portf.dn.StdDev, 
                               optimize_method="random", rp=rp,
                               trace=TRUE)
  cat("opt.dn complete. Saving results to ", results.dir, "\n") 
  save(opt.dn, file=paste(results.dir, "opt.dn.rda", sep="/"))
}

##### Example 3 #####
# Example 3 will consider three portfolios
# - minES
# - minES with component contribution limit
# - minES with equal risk contribution

funds <- colnames(R)
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
                                name="ES", max_prisk=0.3)

# Relax the box constraint
portf.minES.RB$constraints[[2]]$max <- rep(1,ncol(R))
# print.default(portf.minES.RB$constraints[[2]])

# Add risk budget objective to minimize concentration of percentage component
# contribution to risk. Concentration is defined as the Herfindahl-Hirschman
# Index (HHI). $\sum_i x_i^2$
portf.minES.EqRB <- add.objective(portf.minES, type="risk_budget", 
                                  name="ES", min_concentration=TRUE)
# relax the box constraint
portf.minES.EqRB <- add.constraint(portf.minES.EqRB, type="box", 
                                   min=0.05, max=1, indexnum=2)
# portf.minES.RB$constraints[[2]]$max <- rep(1,ncol(R))
# print.default(portf.minES.EqRB$constraints[[2]])

# Add risk budget objective to minES portfolio with multiplier=0 so that it
# is calculated, but does not affect optimization
portf.minES <- add.objective(portf.minES, type="risk_budget", 
                             name="ES", multiplier=0)

# Combine the portfolios so we can make a single call to 
# optimize.portfolio
portf <- combine.portfolios(list(minES=portf.minES, 
                                 minES.RB=portf.minES.RB, 
                                 minES.EqRB=portf.minES.EqRB))

cat("Example 3: running minimum ES optimizations\n")
if(file.exists(paste(results.dir, "opt.minES.rda", sep="/"))){
  cat("file already exists\n")
} else {
  # Run the optimization
  opt.minES <- optimize.portfolio(R, portf, optimize_method="DEoptim", 
                                  search_size=5000, trace=TRUE, traceDE=0,
                                  message=TRUE)
  cat("opt.minES complete. Saving results to ", results.dir, "\n")
  save(opt.minES, file=paste(results.dir, "opt.minES.rda", sep="/"))
}

# Now we want to evaluate the optimization through time

# Rebalancing parameters
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 120
# Trailing Period
trailing <- 72

cat("Example 3: running minimum ES backtests\n")
if(file.exists(paste(results.dir, "bt.opt.minES.rda", sep="/"))){
  cat("file already exists\n")
} else {
  # Backtest
  bt.opt.minES <- optimize.portfolio.rebalancing(R, portf,
                                                 optimize_method="DEoptim", 
                                                 rebalance_on=rebal.freq, 
                                                 training_period=training, 
                                                 trailing_periods=trailing,
                                                 search_size=5000,
                                                 traceDE=0, message=TRUE)
  cat("bt.opt.minES complete. Saving results to ", results.dir, "\n")
  save(bt.opt.minES, file=paste(results.dir, "bt.opt.minES.rda", sep="/"))
}

##### Example 4 #####

# Simple function to compute the moments used in CRRA
crra.moments <- function(R, ...){
  out <- list()
  out$mu <- colMeans(R)
  out$sigma <- cov(R)
  out$m3 <- PerformanceAnalytics:::M3.MM(R)
  out$m4 <- PerformanceAnalytics:::M4.MM(R)
  out
}


# Fourth order expansion of CRRA expected utility
CRRA <- function(R, weights, lambda, sigma, m3, m4){
  weights <- matrix(weights, ncol=1)
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
portf.crra <- portfolio.spec(funds)
portf.crra <- add.constraint(portf.crra, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)

portf.crra <- add.constraint(portf.crra, type="box", 
                             min=0.05, max=0.4)

portf.crra <- add.objective(portf.crra, type="return", 
                            name="CRRA", arguments=list(lambda=10))

# I just want these for plotting
# Set multiplier=0 so that it is calculated, but does not affect the optimization
portf.crra <- add.objective(portf.crra, type="return", name="mean", multiplier=0)
portf.crra <- add.objective(portf.crra, type="risk", name="ES", multiplier=0)
portf.crra <- add.objective(portf.crra, type="risk", name="StdDev", multiplier=0)

cat("Example 4: running maximum CRRA optimization\n")
if(file.exists(paste(results.dir, "opt.crra.rda", sep="/"))){
  cat("file already exists\n")
} else {
  # Run the optimization
  opt.crra <- optimize.portfolio(R, portf.crra, optimize_method="DEoptim", 
                                 search_size=5000, trace=TRUE, traceDE=0,
                                 momentFUN="crra.moments")
  cat("opt.crra complete. Saving results to ", results.dir, "\n") 
  save(opt.crra, file=paste(results.dir, "opt.crra.rda", sep="/"))
}

cat("Example 4: running maximum CRRA backtest\n")
if(file.exists(paste(results.dir, "bt.opt.crra.rda", sep="/"))){
  cat("file already exists\n")
} else {
  # Run the optimization with rebalancing
  bt.opt.crra <- optimize.portfolio.rebalancing(R, portf.crra, 
                                                optimize_method="DEoptim",
                                                search_size=5000, trace=TRUE,
                                                traceDE=0,
                                                momentFUN="crra.moments",
                                                rebalance_on=rebal.freq, 
                                                training_period=training, 
                                                trailing_periods=trailing)
  cat("bt.opt.crra complete. Saving results to ", results.dir, "\n")
  save(bt.opt.crra, file=paste(results.dir, "bt.opt.crra.rda", sep="/"))
}

##### RP Demo #####
cat("Random portfolio method comparison\n")
if(file.exists("figures/rp_plot.png") & file.exists("figures/rp_viz.rda")){
  cat("file already exists\n")
} else {
  portf.lo <- portfolio.spec(colnames(R))
  portf.lo <- add.constraint(portf.lo, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
  
  portf.lo <- add.constraint(portf.lo, type="long_only")
  
  # Use the long only portfolio previously created
  # Generate random portfolios using the 3 methods
  rp1 <- random_portfolios(portf.lo, permutations=2000, 
                           rp_method='sample')
  rp2 <- random_portfolios(portf.lo, permutations=2000, 
                           rp_method='simplex') 
  rp3 <- random_portfolios(portf.lo, permutations=2000, 
                           rp_method='grid')
  
  # Calculate the portfolio mean return and standard deviation
  rp1_mean <- apply(rp1, 1, function(x) mean(R %*% x))
  rp1_StdDev <- apply(rp1, 1, function(x) StdDev(R, weights=x))
  rp2_mean <- apply(rp2, 1, function(x) mean(R %*% x))
  rp2_StdDev <- apply(rp2, 1, function(x) StdDev(R, weights=x))
  rp3_mean <- apply(rp3, 1, function(x) mean(R %*% x))
  rp3_StdDev <- apply(rp3, 1, function(x) StdDev(R, weights=x))
  
  x.assets <- StdDev(R)
  y.assets <- colMeans(R)
  ###
  require(rCharts)
  # create an interactive plot using rCharts and nvd3 scatterChart
  tmp1 <- data.frame(name="sample", mean=rp1_mean, sd=rp1_StdDev)
  tmp2 <- data.frame(name="simplex", mean=rp2_mean, sd=rp2_StdDev)
  tmp3 <- data.frame(name="grid", mean=rp3_mean, sd=rp3_StdDev)
  tmp <- rbind(tmp1, tmp2, tmp3)
  rp_viz <- nPlot(mean ~ sd, group="name", data=tmp, type="scatterChart")
  rp_viz$xAxis(
    axisLabel = 'Risk (std. dev.)'
    ,tickFormat = "#!d3.format('0.4f')!#"
  )
  rp_viz$yAxis(
    axisLabel = 'Return'
    ,tickFormat = "#!d3.format('0.4f')!#" 
  )
  rp_viz$chart(color = my_colors[c(2,4,6)])
  #set left margin so y axis label will show up
  rp_viz$chart( margin = list(left = 100) )
#   rp_viz$chart(
#     tooltipContent = "#!
#     function(a,b,c,d) {
#     //d has all the info  you need
#     return( '<h3>' + d.point.series + '</h3>Return: ' + d.point.y  +  '<br>Risk: ' + d.point.x)
#     }
#     !#")
  ####if you do not want fisheye/magnify
  ####let me know, and will show how to remove
  ####this will solve the tooltip problem
  save(rp_viz, file="figures/rp_viz.rda")
  ###
  x.lower <- min(x.assets) * 0.9
  x.upper <- max(x.assets) * 1.1
  y.lower <- min(y.assets) * 0.9
  y.upper <- max(y.assets) * 1.1
  
  png("figures/rp_plot.png", height = 500, width = 1000)
  # plot feasible portfolios 
  plot(x=rp1_StdDev, y=rp1_mean, col=my_colors[2], main="Random Portfolio Methods",
       ylab="mean", xlab="StdDev", xlim=c(x.lower, x.upper), 
       ylim=c(y.lower, y.upper))
  points(x=rp2_StdDev, y=rp2_mean, col=my_colors[4], pch=2)
  points(x=rp3_StdDev, y=rp3_mean, col=my_colors[6], pch=5)
  points(x=x.assets, y=y.assets)
  text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
  legend("bottomright", legend=c("sample", "simplex", "grid"), 
         col=my_colors[c(2,4,6)],
         pch=c(1, 2, 5), bty="n")
  dev.off()
}

cat("Random portfolio simplex method fev biasing\n")
if(file.exists("figures/fev_plot.png")){
  cat("file already exists\n")
} else {
  png("figures/fev_plot.png", height = 500, width = 1000)
  fev <- 0:5
  x.assets <- StdDev(R)
  y.assets <- colMeans(R)
  par(mfrow=c(2, 3))
  for(i in 1:length(fev)){
    rp <- rp_simplex(portfolio=portf.lo, permutations=2000, fev=fev[i])
    tmp.mean <- apply(rp, 1, function(x) mean(R %*% x))
    tmp.StdDev <- apply(rp, 1, function(x) StdDev(R=R, weights=x))
    x.lower <- min(c(tmp.StdDev, x.assets)) * 0.85
    x.upper <- max(c(tmp.StdDev, x.assets)) * 1.15
    y.lower <- min(c(tmp.mean, y.assets)) * 0.85
    y.upper <- max(c(tmp.mean, y.assets)) * 1.15
    plot(x=tmp.StdDev, y=tmp.mean, main=paste("FEV =", fev[i]),
         ylab="mean", xlab="StdDev", col=rgb(0, 0, 100, 50, maxColorValue=255),
         xlim=c(x.lower, x.upper), 
         ylim=c(y.lower, y.upper))
    points(x=x.assets, y=y.assets)
    text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
  }
  par(mfrow=c(1,1))
  dev.off()
}

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
