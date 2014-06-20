library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES",
                            arguments=list(p=0.9))

# uncleaned R
moments.sample <- set.portfolio.moments(R, init.portf)
all.equal(moments.sample$mu, matrix(colMeans(R), ncol=1))
all.equal(moments.sample$sigma, cov(R))
all.equal(moments.sample$m3, PerformanceAnalytics:::M3.MM(R))
all.equal(moments.sample$m4, PerformanceAnalytics:::M4.MM(R))

moments.boudt <- set.portfolio.moments(R, init.portf, method="boudt", k=3)
fit <- statistical.factor.model(R, 3)
all.equal(moments.boudt$mu, matrix(colMeans(R), ncol=1))
all.equal(moments.boudt$sigma, extractCovariance(fit))
all.equal(moments.boudt$m3, extractCoskewness(fit))
all.equal(moments.boudt$m4, extractCokurtosis(fit))

moments.bl <- set.portfolio.moments(R, init.portf, method="black_litterman")
BL <- black.litterman(R, matrix(rep(1, ncol(R)), 1))
all.equal(moments.bl$mu, BL$BLMu)
all.equal(moments.bl$sigma, BL$BLSigma)
all.equal(moments.bl$m3, PerformanceAnalytics:::M3.MM(R))
all.equal(moments.bl$m4, PerformanceAnalytics:::M4.MM(R))


# cleaned R
cleanR <- Return.clean(R, method="boudt")
init.portf$objectives[[1]]$arguments$clean <- "boudt"

moments.sample <- set.portfolio.moments(R, init.portf)
all.equal(moments.sample$mu, matrix(colMeans(cleanR), ncol=1))
all.equal(moments.sample$sigma, cov(cleanR))
all.equal(moments.sample$m3, PerformanceAnalytics:::M3.MM(cleanR))
all.equal(moments.sample$m4, PerformanceAnalytics:::M4.MM(cleanR))

moments.boudt <- set.portfolio.moments(R, init.portf, method="boudt", k=3)
fit <- statistical.factor.model(cleanR, 3)
all.equal(moments.boudt$mu, matrix(colMeans(cleanR), ncol=1))
all.equal(moments.boudt$sigma, extractCovariance(fit))
all.equal(moments.boudt$m3, extractCoskewness(fit))
all.equal(moments.boudt$m4, extractCokurtosis(fit))

moments.bl <- set.portfolio.moments(R, init.portf, method="black_litterman")
BL <- black.litterman(cleanR, matrix(rep(1, ncol(cleanR)), 1))
all.equal(moments.bl$mu, BL$BLMu)
all.equal(moments.bl$sigma, BL$BLSigma)
all.equal(moments.bl$m3, PerformanceAnalytics:::M3.MM(cleanR))
all.equal(moments.bl$m4, PerformanceAnalytics:::M4.MM(cleanR))

