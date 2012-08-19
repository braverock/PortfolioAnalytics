# # # # # # # # # #
# Try DEoptim
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(DEoptim)
library(pso)
library(GenSA)

data(edhec)
N <- 4
R <- edhec[, 1:N]
T <- nrow(edhec)
mu <- colMeans(R)
sigma <- cov(R)

obj <- function(w){
  if (sum(w)==0) {w <- w + 1e-2}
  w <- w/sum(w)
  CVaR <- ES(weights= w, 
             method="gaussian", max.
             portfolio_method="component",
             mu=mu,
             sigma=sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.225, 0)
  out <- tmp1 + 1e3 * tmp2
}

set.seed(1234)
deoptim.sol <- DEoptim(fn=obj, lower = rep(0, N), upper = rep(1, N))

deoptim.sol$optim$bestval
wts.deoptim <- deoptim.sol$optim$bestmem / sum(deoptim.sol$optim$bestmem)

test <- psoptim(rep(NA,N), obj, lower=0, upper=1, control=list(abstol=1e-8, trace=TRUE))
test$value
wts.pso <- test$par/sum(test$par)

gen.sol <- GenSA(rep(1/N, N), lower=rep(0,N), upper=rep(1,N), obj, control=list(verbose=TRUE))











