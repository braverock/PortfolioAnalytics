# # # # # # # # # #
# Try DEoptim
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(DEoptim)

data(edhec)
R <- edhec
N <- ncol(edhec)
mu <- colMeans(R)
sigma <- cov(R)

obj <- function(w){
  if (sum(w)==0) {w <- w + 1e-2}
  w <- w/sum(w)
  CVaR <- ES(weights= w, 
             method="gaussian", 
             portfolio_method="component",
             mu=mu,
             sigma=sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
  out <- tmp1 + 1e3 * tmp2
}

set.seed(1234)
out <- DEoptim(fn=obj,
               lower = rep(0, N),
               upper = rep(1, N))

out$optim$bestval
wts.deoptim <- out$optim$bestmem / sum(out$optim$bestmem)

test <- psoptim(rep(NA,N), obj, lower=0, upper=5, control=list(abstol=1e-8))
test$value
wts.pso <- test$par/sum(test$par)
