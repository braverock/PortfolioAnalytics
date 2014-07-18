#' ---
#' title: "Meucci FFV Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrate Meucci's Fully Flexible Views framework to estimate 
#' moments to use as inputs for a minimum variance optimization.

#' Load package and data
library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:5]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box",
                             min=0.05, max=0.5)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean", multiplier=0)

# prior probabilities
p <- rep(1 / nrow(R), nrow(R))

#' Here we express views
#' lambda is the ad-hoc multiplier
#' m_k = m(V_k) + lambda * sigma(V_k)
#' sigma(k) is a measure of volatility (i.e. standard deviation, interquartile range, etc.)
#' Meucci recommends -2 (very bearish), -1 (bearish), 1 (bullish), 2 (very bullish)

#' View 1: very bearish view on R[,1] - R[,2]
V1 <- coredata(R[,1] - R[,2])
b1 <- mean(V1) - 2 * sd(V1)

#' View 2: bearish view on R[,5] - R[,4]
V2 <- coredata(R[,5] - R[,4])
b2 <- mean(V2) - 1 * sd(V2)

#' Here we compute the posterior probabilities for each view.

#' Set up equality constraints to constrain the posterior probabilities to 
#' sum to 1.
Aeq <- matrix(1, ncol=nrow(R))
beq <- 1

#' Run the entropy optimization to compute the posterior for each view.
p1 <- EntropyProg(p, t(V1), b1, Aeq, beq)$p_
p2 <- EntropyProg(p, t(V2), b2, Aeq, beq)$p_

#' Assign confidence weights to the views and pool opinions.
#' 0.35 : confidence weight on reference model
#' 0.25 : confidence weight on view 1
#' 0.4  : confidence weight on view 2

#' Use entropy pooling to estimate the posterior.
p_ <- cbind(p, p1, p2) %*% c(0.35 , 0.25 , 0.4)

#' Generate random portfolios for use in the optimization.
rp <- random_portfolios(init.portf, 10000)

#' Run the optimization using first and second moments estimated from 
#' Meucci's Fully Flexible Views framework.
opt.meucci <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp, 
                                 trace=TRUE,
                                 method="meucci", 
                                 posterior_p=p_)


# Run the optimization using sample estimates for first and second moments.
opt.sample <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp,
                                 trace=TRUE)

#' Extract the stats for plotting.
stats.meucci <- extractStats(opt.meucci)
stats.sample <- extractStats(opt.sample)

#' Plot the optimal weights.
chart.Weights(combine.optimizations(list(meucci=opt.meucci, sample=opt.sample)))

#' Plot the risk-reward of each chart on the same scale.
xrange <- range(c(stats.meucci[,"StdDev"], stats.sample[,"StdDev"]))
yrange <- range(c(stats.meucci[,"mean"], stats.sample[,"mean"]))
layout(matrix(c(1,2)), widths=1, heights=1)
# c(bottom, left, top, right)
par(mar=c(0, 4, 4, 4) + 0.1)
plot(x=stats.meucci[,"StdDev"], stats.meucci[,"mean"], xlab="", ylab="mean", 
     xlim=xrange, ylim=yrange, xaxt="n", yaxt="n")
axis(2, pretty(yrange), cex.axis=0.8)
legend("topright", legend="Meucci", bty="n")
par(mar=c(5, 4, 0, 4) + 0.1)
plot(x=stats.sample[,"StdDev"], stats.sample[,"mean"], xlab="StdDev", ylab="", 
     xlim=xrange, ylim=yrange, yaxt="n", cex.axis=0.8)
axis(4, pretty(yrange), cex.axis=0.8)
legend("topright", legend="Sample", bty="n")
par(mar=c(5, 4, 4, 2) + 0.1)
layout(matrix(1), widths=1, heights=1)
