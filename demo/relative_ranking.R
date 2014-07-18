#' ---
#' title: "Regime Switching Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates expressing views on the relative ranking of 
#' expected returns based on two methods:
#' 1. R. Almgren and N. Chriss, "Portfolios from Sorts"
#' 2. A. Meucci, "Fully Flexible Views: Theory and Practice"

#' Load package and data.
library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:4]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box",
                             min=0.05, max=0.5)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

#' Here we express views on the relative rank of the asset returns.
#' E{ R[,2] < R[,3] < R[,1] < R[,4] }
asset.rank <- c(2, 3, 1, 4)

#' Use Meucci Fully Flexible Views framework to express views on the relative
#' order of asset returns.
#' Define prior probabilities.
p <- rep(1 / nrow(R), nrow(R))

#' Express view on the relative ordering of asset returns
m.moments <- meucci.ranking(R, p, asset.rank)

#' Express views using the method described in Almgren and Chriss, 
#' "Portfolios from Sorts".
ac.moments <- list()
ac.moments$mu <- ac.ranking(R, asset.rank)
# Sample estimate for second moment
ac.moments$sigma <- cov(R)

#' Generate random portfolios for use in the optimization.
rp <- random_portfolios(init.portf, 5000)

#' Run the optimization using first and second moments estimated from 
#' Meucci's Fully Flexible Views framework using the moments we calculated
#' from our view.
opt.meucci <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp, 
                                 trace=TRUE,
                                 momentargs=m.moments)

#' Run the optimization using first moment estimated based on Almgren and Chriss, 
#' "Portfolios from Sorts". The second moment uses the sample estimate.
opt.ac <- optimize.portfolio(R, 
                             init.portf, 
                             optimize_method="random", 
                             rp=rp, 
                             trace=TRUE,
                             momentargs=ac.moments)

#' For comparison, run the optimization using sample estimates for first and 
#' second moments.
opt.sample <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp,
                                 trace=TRUE)

#' Here we plot the optimal weights of each optimization.
chart.Weights(combine.optimizations(list(meucci=opt.meucci, 
                                         ac=opt.ac,
                                         sample=opt.sample)), 
              ylim=c(0,1), plot.type="barplot")

#' Here we define a custom moment function to estimate moments based on 
#' relative ranking views.
#' Asset are ranked according to a momentum or reversal view based on the 
#' previous n periods.
moment.ranking <- function(R, n=1, momentum=TRUE, method=c("meucci", "ac")){
  # Moment function to estimate moments based on relative ranking of 
  # expected returns.
  
  method <- match.arg(method)
  
  # Use the most recent n periods of returns
  tmpR <- apply(tail(R, n), 2, function(x) prod(1 + x) - 1)
  
  if(momentum){
    # Assume that the assets with the highest return will continue to outperform
    asset.rank <- order(tmpR)
  } else {
    # Assume that the assets with the highest return will reverse
    asset.rank <- rev(order(tmpR))
  }
  switch(method,
         meucci = {
           # Meucci Fully Flexible Views framework
           # Prior probabilities
           p <- rep(1 / nrow(R), nrow(R))
           
           # Relative ordering view
           moments <- meucci.ranking(R, p, asset.rank)
         },
         ac = {
           # Almgren and Chriss Portfolios from Sorts
           moments <- list()
           moments$mu <- ac.ranking(R, asset.rank)
           # Sample estimate for second moment
           moments$sigma <- cov(R)
         }
  )
  return(moments)
}

#' Here we run out of sample backtests to test the out of sample performance
#' using using the different frameworks to express our views on relative
#' asset return ranking.
opt.bt.meucci <- optimize.portfolio.rebalancing(R, init.portf, 
                                                optimize_method="random", 
                                                rebalance_on="quarters", 
                                                training_period=100,
                                                rp=rp,
                                                momentFUN="moment.ranking",
                                                n=2,
                                                momentum=TRUE,
                                                method="meucci")

opt.bt.ac <- optimize.portfolio.rebalancing(R, init.portf, 
                                            optimize_method="random", 
                                            rebalance_on="quarters", 
                                            training_period=100,
                                            rp=rp,
                                            momentFUN="moment.ranking",
                                            n=2,
                                            momentum=TRUE,
                                            method="ac")

opt.bt.sample <- optimize.portfolio.rebalancing(R, init.portf, 
                                                optimize_method="random", 
                                                rebalance_on="quarters", 
                                                training_period=100,
                                                rp=rp)

#' Compute returns and chart performance summary.
ret.meucci <- Return.portfolio(R, extractWeights(opt.bt.meucci))
ret.ac <- Return.portfolio(R, extractWeights(opt.bt.ac))
ret.sample <- Return.portfolio(R, extractWeights(opt.bt.sample))
ret <- cbind(ret.meucci, ret.ac, ret.sample)
colnames(ret) <- c("meucci.rank", "ac.rank", "sample")
charts.PerformanceSummary(ret, main="Ranking Views Performance")

