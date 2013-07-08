# R script to test the ideas in the Shaw 2011 paper

# Function based on the Shaw 2011 paper to generate sets of portfolio weights
# with FEV biasing
rp_shaw <- function(N, p, k, L){
  # N = number of assets
  # p = vector of values of p for level of FEV-biasing
  # k = number of portfolios for each value of p
  # L = lower bounds
  
  # generate uniform[0, 1] random numbers
  U <- runif(n=k*N, 0, 1)
  Umat <- matrix(data=U, nrow=k, ncol=N)
  
  # List to store the portfolios for each value of p
  out <- list()
  
  # Create k portfolios for each value of p
  # Total of k * length(p) portfolios
  for(i in 1:length(p)){
    q <- 2^p[i]
    tmp_Umat <- t(apply(Umat, 1, function(x) L + (1 - sum(L)) * log(x)^q / sum(log(x)^q)))
    out[[i]] <- tmp_Umat
  }
  return(out)
}

# Quick test of the rp_shaw function
# create 10 portfolios for 4 assets
tmp <- rp_shaw(N=6, p=0:5, k=10, L=rep(0, 6))
tmp
do.call("rbind", tmp)

##### Shaw 2011 Example #####

# Replicate exaple from Shaw 2011
# covariance matrix 4.19
Sigma <- rbind(c(0.0549686, 0.144599, -0.188442, 0.0846818, 0.21354, 0.0815392),
               c(0.144599, 1.00269, -0.837786, 0.188534, 0.23907, -0.376582),
               c(-0.188442, -0.837786, 1.65445, 0.404402, 0.34708, -0.350142),
               c(0.0846818, 0.188534, 0.404402, 0.709815, 1.13685, -0.177787),
               c(0.21354, 0.23907, 0.34708, 1.13685, 2.13408, 0.166434),
               c(0.0815392, -0.376582, -0.350142, -0.177787, 0.166434, 0.890896))
w_optimum <- c(0.883333, 0, 0.11667, 0, 0, 0)

# Create 3,600,000 portfolios
# Create 600,000 portfolios of 6 assets for each value of p
# This is slow... takes about 30 seconds
# Investigate possible solutions for parallel random number generation in R
system.time(
tmp_shaw <- rp_shaw(N=6, p=0:5, k=600000, L=rep(0, 6))
)
tmp <- do.call("rbind", tmp_shaw)

# Calculate the objective function on the tmp matrix
# get this working in parallel

# Define objective function
obj_fun <- function(x) t(x) %*% Sigma %*% x

# single-core version using apply
# takes about 70 seconds
system.time(
  obj1 <- apply(tmp, 1, obj_fun)
)

# single-core version using lapply
# faster than apply... takes about 38 seconds
system.time(
  obj2 <- unlist(lapply(1:nrow(tmp), function(x) obj_fun(tmp[x,])))
)
all.equal(obj1, obj2)

# multi-core version using mclapply
# faster than lapply version... takes about 24 seconds
library(multicore)
system.time(
  obj3 <- unlist(mclapply(1:nrow(tmp), function(x) obj_fun(tmp[x,])))
)
all.equal(obj1, obj3)

# library(foreach)
# system.time(
#   obj4 <- foreach(i=1:nrow(tmp)) %dopar% obj_fun(tmp[i,])
# )
# all.equal(obj1, obj4)

# Find the minimum of the objective measure
tmp_min <- min(obj1)

# Find the optimal weights that minimize the objective measure
w <- tmp[which.min(obj1),]

# view the weights
print(round(w,6))
print(w_optimum)

# solution is close
print(all.equal(round(w,6), w_optimum))

##### Lower Bounds #####
# Specify lower bounds
L <- c(0.1, 0.05, 0.05, 0.08)

U <- runif(4, 0, 1)
log(U) / sum(log(U))
# w_i = L_i + sum(L) * log(U_i)/sum(log(U)
w <- L + (1 - sum(L)) * log(U) / sum(log(U))
w
sum(w)
all(w >= L)

##### Lower Bounds with FEV Biasing #####

# N = number of assets
# p = vector of values of p for level of FEV-biasing
# k = number of portfolios for each value of p
# L = lower bounds
N <- 4
k <- 10
p <- 0:5
L <- c(0.1, 0.05, 0.05, 0.08)

U <- runif(n=k*N, 0, 1)
Umat <- matrix(data=U, nrow=k, ncol=N)

# List to store the portfolios for each value of p
out <- list()

# Create k portfolios for each value of p
# Total of k * length(p) portfolios
for(i in 1:length(p)){
  q <- 2^p[i]
  tmp_Umat <- t(apply(Umat, 1, function(x) L + (1 - sum(L)) * log(x)^q / sum(log(x)^q)))
  out[[i]] <- tmp_Umat
}
out

# rbind each matrix in the list together
tmp <- do.call("rbind", out)
tmp

# check that all the weights sum to 1
apply(tmp, 1, sum)

# check that all weights obey the lower bounds
apply(tmp, 1, function(x) all(x >= L))

