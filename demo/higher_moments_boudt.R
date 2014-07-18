#' ---
#' title: "Higher Moment Estimates Demo"
#' author: Ross Bennett
#' date: "7/17/2014"
#' ---

#' This script demonstrates how to solve a constrained portfolio optimization 
#' problem using moments estimated via a factor model based on the work of
#' Kris Boudt.

#' Load package and data.
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

#' Construct initial portfolio with basic constraints.
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum",
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES",
                            arguments=list(p=0.9, clean="boudt"))

#' This is not necessary for the optimization, but demonstrates how the
#' moments are estimated using portfolio.moments.boudt.
cleanR <- Return.clean(R, "boudt")
fit <- statistical.factor.model(cleanR, 3)

#' Here we extract the moments.
sigma <- extractCovariance(fit)
m3 <- extractCoskewness(fit)
m4 <- extractCokurtosis(fit)

# Here we compute the moments with portfolio.moments.boudt and show that the
#' result is equivalent to fitting the factor model and extracting the moment
#' estimates.
moments.boudt <- portfolio.moments.boudt(R, init.portf, k=3)
all.equal(moments.boudt$sigma, sigma)
all.equal(moments.boudt$m3, m3)
all.equal(moments.boudt$m4, m4)

#' Generate set of random portfolios to use for the optimization.
rp <- random_portfolios(init.portf, 5000)

#' Here we run the optimization using sample estimates for the moments.
#' The default for momentFUN is set.portfolio.moments which computes
#' the sample estimates of the moments.
minES.lo.sample <- optimize.portfolio(R=R, portfolio=init.portf, 
                                      rp=rp, optimize_method="random",
                                      trace=TRUE)

#' Now we run the optimization with statistical factor model estimates of the 
#' moments.
minES.lo.boudt <- optimize.portfolio(R=R, portfolio=init.portf, 
                                     momentFUN=portfolio.moments.boudt, 
                                     k=3, rp=rp,
                                     optimize_method="random",
                                     trace=TRUE)


