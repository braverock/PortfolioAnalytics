library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:5]

# compute the probabilities
probs <- rep(1 / nrow(R), nrow(R))
# probabilities should always sum to 1
sum(probs)

# suppose that I express a bearish view on R[,1] - R[,2]
# lambda is the ad-hoc multiplier
# Meucci recommends -2 (very bearish), -1 (bearish), 1 (bullish), 2 (very bullish)
lambda <- -2
V <- coredata(R[,1] - R[,2])
m <- mean(V)
s <- sd(V)
b <- matrix(m + lambda * s, 1)

# set up matrix for equality constraints
# constrain such that probabilities sum to 1
Aeq <- matrix(1, nrow=1, ncol=nrow(R))
beq <- matrix(1, 1, 1)

# set up matrix for inequality constraints
A <- t(V)

# Compute posterior probabilities
# The EntryProg optimization handles inequality constraints as A < b
# If I hav 
posterior_probs <- Meucci::EntropyProg(probs, A, b, Aeq, beq)$p_
dim(posterior_probs)
sum(posterior_probs)

# why do the probs equal the posterior probs?
all.equal(as.numeric(posterior_probs), probs)

# Now I have my posterior probabilities
# What is a general approach I can use for any arbitrary optimization
all.equal(as.numeric(t(coredata(R)) %*% posterior_probs),as.numeric(matrix(colMeans(R), ncol=1)))

# This is pretty close to sample cov
Exps <- t(R) %*% posterior_probs
Scnd_Mom = t(R) %*% (R * (posterior_probs %*% matrix( 1,1,ncol(R)) ) )
Scnd_Mom = ( Scnd_Mom + t(Scnd_Mom) ) / 2
Covs = Scnd_Mom - Exps %*% t(Exps)
Covs
cov(R)


all.equal(coredata(R[,1] - R[,2]), A, check.attributes=FALSE)

