# Demonstrate Meucci's Fully Flexible Views framework to express views on 
# relative ranking and estimate moments used as inputs for mean-variance 
# optimization

library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:4]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box",
                             min=0.05, max=0.5)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

# Prior probabilities
p <- rep(1 / nrow(R), nrow(R))

# Relative ordering view
# E{ R[,2] < R[,3], < R[,1] < R[,4] }
moments <- meucci.ranking(R, p, c(2, 3, 1, 4))

# Generate random portfolios
rp <- random_portfolios(init.portf, 5000)

# Optimization using first and second moments estimated from Meucci's Fully 
# Flexible Views framework.
opt.meucci <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp, 
                                 trace=TRUE,
                                 method="meucci", 
                                 momentargs=moments)


# Optimization using sample estimates for first and second moments
opt.sample <- optimize.portfolio(R, 
                                 init.portf, 
                                 optimize_method="random", 
                                 rp=rp,
                                 trace=TRUE)

#Extract the stats for plotting
stats.meucci <- extractStats(opt.meucci)
stats.sample <- extractStats(opt.sample)


# Plots
# Plot the optimal weights
chart.Weights(combine.optimizations(list(meucci=opt.meucci, sample=opt.sample)), ylim=c(0,1))

# Plot the risk-reward of each chart on the same scale
xrange <- range(c(stats.meucci[,"StdDev"], stats.sample[,"StdDev"]))
yrange <- range(c(stats.meucci[,"mean"], stats.sample[,"mean"]))
layout(matrix(c(1,2)), widths=1, heights=1)
# c(bottom, left, top, right)
par(mar=c(0, 4, 4, 4) + 0.1)
plot(x=stats.meucci[,"StdDev"], stats.meucci[,"mean"], xlab="", ylab="mean", 
     xlim=xrange, ylim=yrange, xaxt="n", yaxt="n")
axis(2, pretty(yrange), cex.axis=0.8)
legend("topleft", legend="Meucci", bty="n")
par(mar=c(5, 4, 0, 4) + 0.1)
plot(x=stats.sample[,"StdDev"], stats.sample[,"mean"], xlab="StdDev", ylab="", 
     xlim=xrange, ylim=yrange, yaxt="n", cex.axis=0.8)
axis(4, pretty(yrange), cex.axis=0.8)
legend("topleft", legend="Sample", bty="n")
par(mar=c(5, 4, 4, 2) + 0.1)
layout(matrix(1), widths=1, heights=1)
