library(PortfolioAnalytics)
data(edhec)
R <- edhec[,1:5]
funds <- colnames(R)

# Construct initial portfolio
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")

# prior probabilities
p <- rep(1 / nrow(R), nrow(R))

# Express views
# lambda is the ad-hoc multiplier
# Meucci recommends -2 (very bearish), -1 (bearish), 1 (bullish), 2 (very bullish)

# View 1: very bearish view on R[,1] - R[,2]
V1 <- coredata(R[,1] - R[,2])
b1 <- mean(V1) - 2 * sd(V1)

# View 2: bearish view on R[,5] - R[,4]
V2 <- coredata(R[,5] - R[,4])
b2 <- mean(V2) - 1 * sd(V2)

# Compute the posterior probabilities for each view
# Equality constraints to constrain the posterior probabilities to sum to 1
Aeq <- matrix(1, ncol=nrow(R))
beq <- 1
p1 <- EntropyProg(p, t(V1), b1, Aeq, beq)$p_
p2 <- EntropyProg(p, t(V2), b2, Aeq, beq)$p_

# Assign confidence weights to the views and pool opinions
# 0.35 : confidence weight on reference model
# 0.25 : confidence weight on view 1
# 0.4  : confidence weight on view 2

# Prior posterior of pooled opinions
p_ <- cbind(p, p1, p2) %*% c(0.35 , 0.25 , 0.4)

m1 <- meucci.moments(R, p_)
m2 <- set.portfolio.moments(R = R, portfolio=init.portf, method="meucci", posterior_p=p_)
all.equal(m1, m2)

