library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")

# Run optimization with DEoptim
minStdDev.DE <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="DEoptim", 
                                   search_size=2000, 
                                   traceDE=0,
                                   trace=TRUE)
xtract.DE <- extractStats(minStdDev.DE)

# Here we extract the objective value returned from the optimizer of each
# iteration. I'm not sure how useful this information is.
obj.DE <- xtract.DE[,"out"]
hist(obj.DE)
rug(obj.DE)
plot(density(obj.DE))
qqnorm(obj.DE)
boxplot(obj.DE)

# Run optimization with random portfolios
minStdDev.RP <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="random", 
                                   search_size=2000, 
                                   trace=TRUE)
xtract.RP <- extractStats(minStdDev.RP)
obj.RP <- xtract.RP[,"out"]
hist(obj.RP)
rug(obj.RP)
plot(density(obj.RP))
qqnorm(obj.RP)
boxplot(obj.RP)

# I think the best way is to do a sort of bootstrap by running several
# hundred or thousand (depending on your resources) optimizations and
# analyze the objective value from each optimal portfolio
opt <- optimize.portfolio.parallel(R=R, 
                                   nodes=50,
                                   portfolio=init.portf, 
                                   optimize_method="random", 
                                   search_size=2000, 
                                   trace=TRUE)
opt
xx <- summary(opt)
obj_val <- xx$obj_val

# estimate of the objective measures, objective value, and weights from the
# optimal portfolio of each optimization
apply(xx$stats, 2, mean)

# plot the objective values from each optimization
hist(obj_val)
rug(obj_val)
plot(density(obj_val))
qqnorm(obj_val)
qqline(obj_val)
boxplot(obj_val)

# These should match the print method
# estimated objective value
mean(obj_val)
# percentile confidence interval estimate
quantile(obj_val, probs = c(0.025, 0.975))

