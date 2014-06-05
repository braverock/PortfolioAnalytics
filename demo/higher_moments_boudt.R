library(PortfolioAnalytics)

# Examples of solving optimization problems using a statistical factor model
# to estimate the higher moments

data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum",
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES",
                            arguments=list(p=0.9, clean="boudt"))

# This is not necessary for the optimization, but demonstrates how the
# moments are estimated using portfolio.moments.boudt
cleanR <- Return.clean(R, "boudt")
fit <- statistical.factor.model(cleanR, 3)
sigma <- extractCovariance(fit)
m3 <- extractCoskewness(fit)
m4 <- extractCokurtosis(fit)

moments.boudt <- portfolio.moments.boudt(R, init.portf, k=3)
all.equal(moments.boudt$sigma, sigma)
all.equal(moments.boudt$m3, m3)
all.equal(moments.boudt$m4, m4)

# Generate set of random portfolios
rp <- random_portfolios(init.portf, 5000)

# Optimization with sample estimates
# The default for momentFUN is set.portfolio.moments which computes
# the sample estimates of the moments
minES.lo.sample <- optimize.portfolio(R=R, portfolio=init.portf, 
                                      rp=rp, optimize_method="random",
                                      trace=TRUE)

# Optimization with statistical factor model estimates of the moments
minES.lo.boudt <- optimize.portfolio(R=R, portfolio=init.portf, 
                                     momentFUN=portfolio.moments.boudt, 
                                     k=3, rp=rp,
                                     optimize_method="random",
                                     trace=TRUE)


