
# DEoptim with max positions constraint
# Uses fnMap to impose a cardinality constraint with DEoptim

library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)

data(edhec)
R <- edhec

# use example objective function from
# http://cran.r-project.org/web/packages/DEoptim/vignettes/DEoptimPortfolioOptimization.pdf
obj <- function(w) {
  if(sum(w) == 0){
    w <- w + 1e-2
  }
  w <- w / sum(w)
  CVaR <- ES(weights=w,
             method="gaussian",
             portfolio_method="component",
             mu=mu,
             sigma=sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.05, 0)
  out <- tmp1 + tmp2
  return(out)
}

mu <- colMeans(R)
sigma <- cov(R)

N <- ncol(R)
minw <- 0
maxw <- 1
lower <- rep(minw, N)
upper <- rep(maxw, N)

eps <- 0.025

weight_seq <- generatesequence(min=minw, max=maxw, by=0.001, rounding=3)

rpconstraint <- constraint(assets=N, min_sum=1-eps, max_sum=1+eps,
                           min=lower, max=upper, weight_seq=weight_seq)
set.seed(1234)
rp <- random_portfolios(rpconstraints=rpconstraint, permutations=N*10)
rp <- rp / rowSums(rp)

controlDE <- list(reltol=.000001,steptol=150, itermax = 5000,trace = 250,
                  NP=as.numeric(nrow(rp)),initialpop=rp)
set.seed(1234)
out1 <- DEoptim(fn = obj, lower=lower, upper=upper, control=controlDE)

weights1 <- out1$optim$bestmem
weights1 <- weights1 / sum(weights1)
sum(weights1)
out1$optim$bestval

# Implement a cardinality constraint for max positions with DEoptim
# http://grokbase.com/t/r/r-help/126fsz99gh/r-deoptim-example-illustrating-use-of-fnmap-parameter-for-enforcement-of-cardinality-constraints
mappingFun <- function(x, max.pos) {
  N <- length(x)
  num <- N - max.pos
  # Two smallest weights are given a value of 0
  x[order(x)][1:num] <- 0
  x / sum(x)
}

out2 <- DEoptim(fn = obj, lower=lower, upper=upper, control=controlDE, fnMap=function(x) mappingFun(x, max.pos=10))
weights2 <- out2$optim$bestmem
weights2 <- weights2 / sum(weights2)
out2$optim$bestval
sum(round(weights2, 4))

# mappingGroupFun <- function(x) {
#   i <- 1
#   while(sum(x[1:2]) > 0.4 & i <= 5) {
#     x[1:2] <- x[1:2] - 0.01
#     i <- 1 + 1
#   }
#   x / sum(x)
# }
# 
# out3 <- DEoptim(fn = obj, lower=lower, upper=upper, control=controlDE, fnMap=mappingGroupFun)
# weights3 <- out3$optim$bestmem
# sum(weights[1:2])
# out3$optim$bestval
# sum(round(weights3, 4))


