
library(PortfolioAnalytics)

# Use random_portfolios to generate weights that do not meet the full
# investment constraint where the sum of the weights range from 0.8 to 1.2

# Note: slow using random portfolios

sum_seq <- seq(from=0.8, to=1.5, by=0.1)

##### Random Portfolios: 50 assets 5,000 portfolios
nassets <- 50
npermutations <- 500
min <- rep(0, nassets)
# random_index <- sample(1:nassets, 5)
# min[random_index] <- 0.01
max <- rep(0.5, nassets)
rp <- list()
for(i in 1:10){
min_sum <- sample(sum_seq, 1)
max_sum <- min_sum + 0.01

cset <- constraint(assets=nassets, min=min, max=max, 
                   min_sum=min_sum, max_sum=max_sum,
                   weight_seq=generatesequence(min=0, max=0.5, by=0.005))

rp[[i]] <- random_portfolios(rpconstraints=cset, permutations=npermutations)
}

rp <- do.call(rbind, rp)

# transform the entire vector to meet leverage constraints
tmp_rp <- t(apply(rp, 1, txfrm_weight_sum_constraint, min_sum=0.99, max_sum=1.01))

# percentage of portfolios that satisfy box constraints after the simple transformation
sum(apply(tmp_rp, 1, function(x) all(x >= min & x <= max))) / (nrow(tmp_rp)) * 100

# only works if I relax min and 
new_rp <- t(apply(tmp_rp, 1, rp_transform, min=rep(-0.05, nassets), max=rep(0.5, nassets), 
                  groups=NULL, cLO=NULL, cUP=NULL,
                  max_permutations=500))

##### Random Portfolios: 250 assets 5,000 portfolios
nassets <- 250
npermutations <- 500
min <- rep(0, nassets)
random_index <- sample(1:nassets, 10)
min[random_index] <- 0.01
max <- rep(0.5, nassets)
rp <- list()
for(i in 1:10){
  min_sum <- sample(sum_seq, 1)
  max_sum <- min_sum + 0.01
  
  cset <- constraint(assets=nassets, min=min, max=max, 
                     min_sum=min_sum, max_sum=max_sum,
                     weight_seq=generatesequence(min=0, max=0.5, by=0.005))
  
  rp[[i]] <- random_portfolios(rpconstraints=cset, permutations=npermutations)
}

rp <- do.call(rbind, rp)

# transform the entire vector to meet leverage constraints
tmp_rp <- t(apply(rp, 1, txfrm_weight_sum_constraint, min_sum=0.99, max_sum=1.01))

# percentage of portfolios that satisfy box constraints after the simple transformation
sum(apply(tmp_rp, 1, function(x) all(x >= min & x <= max))) / (nrow(tmp_rp)) * 100

new_rp <- t(apply(tmp_rp, 1, rp_transform, min=min, max=max, groups=NULL, cLO=NULL, cUP=NULL))


# generate portfolios of uniform random numbers that satisfy box constraints,
# but will violate leverage constraints
N <- 500
k <- 10000
min <- -0.01
max <- 0.15

set.seed(123)
tmp <- runif(N*k, min, max)
tmp_mat <- matrix(tmp, nrow=k)

summary(rowSums(tmp_mat))

# transform the entire vector to meet leverage constraints
tmp_rp <- t(apply(tmp_mat, 1, txfrm_weight_sum_constraint, min_sum=0.99, max_sum=1.01))

min <- c(rep(-0.01, 200), 0.01, rep(-0.01, 299))
max <- rep(0.15, 500)
# percentage of portfolios that satisfy box constraints after the simple transformation
sum(apply(tmp_rp, 1, function(x) all(x >= min & x <= max))) / (nrow(tmp_rp)) * 100

# All portfolios seem to satisfy box constraints if min is a vector of all 0s or
# all elements are less than 0 and the sum of the weights is greater than 1

# If elements of the min vector are positive, then 0 portfolios satisfy constraints

# very sensitive to box constraint parameters and sum of the weights

