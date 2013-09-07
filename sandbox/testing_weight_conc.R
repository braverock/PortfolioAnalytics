library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# data(edhec)
# R <- edhec[, 1:4]
# colnames(R) <- c("CA", "CTAG", "DS", "EM")
# funds <- colnames(R)

load("~/Desktop/Testing/crsp.short.Rdata")
R <- cbind(microcap.ts[, 1:2],
           smallcap.ts[, 1:2],
           midcap.ts[, 1:2],
           largecap.ts[, 1:2])

funds <- colnames(R)

cap_labels <- c(rep("MICRO", 2), rep("SMALL", 2), 
                rep("MID", 2), rep("LARGE", 2))

# Create initial portfolio object with category_labels
init <- portfolio.spec(assets=funds, category_labels=cap_labels)
# Add some weight constraints
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="long_only")
# Add objective to minimize variance
init <- add.objective(portfolio=init, type="risk", name="var")

# Run the optimization with var as the only objective
opt1 <- optimize.portfolio(R=R, portfolio=init, optimize_method="ROI", trace=TRUE)
opt1

# Add the weight_concentration objective
# Set the conc_aversion values to 0 so that we should get the same value as min var
conc <- add.objective(portfolio=init, type="weight_concentration", name="HHI", 
                       conc_aversion=0, conc_groups=init$category_labels)

opt2 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt2
all.equal(opt1$weights, opt2$weights)

# From the chart we can see that the allocation to MGF is very high.
chart.Weights(opt2)

# MGF is part of the SMALL group
# Now change the conc_aversion values
conc$objectives[[2]]$conc_aversion <- c(0.1, 0.05, 0.1, 0)
opt3 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt3

chart.Weights(opt3)

# We do not have a group constraint, but we can plot the groups based on
# category labels in the portfolio object
chart.GroupWeights(opt3, grouping="category", plot.type="barplot", col=bluemono)

# If all the conc_aversion values are very high, this should result in an equal weight portfolio
conc$objectives[[2]]$conc_aversion <- rep(1e6, 4)
opt4 <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
opt4
chart.Weights(opt4)

#####
# Use solve.QP manually
library(quadprog)

# number of assets
N <- ncol(R)

# concentration aversion parameter
lambda_hhi <- 2

# Quadratic objective
Q <- 2*var(R) + lambda_hhi * diag(N)

# Constraints matrix and rhs for full investment and long only
Amat <- cbind(rep(1, N), diag(N), -diag(N))
rhs <- c(1, rep(0, N), rep(-1, N))

sol <- solve.QP(Dmat=Q, dvec=rep(0, N), Amat=Amat, bvec=rhs, meq=1)
sol$solution

conc <- add.objective(portfolio=init, type="weight_concentration", name="HHI", 
                      conc_aversion=2)

opt <- optimize.portfolio(R=R, portfolio=conc, optimize_method="ROI", trace=TRUE)
all.equal(opt$weights, sol$solution, check.attributes=F)

# concentration aversion parameter by group
lambda_hhi <- c(0.1, 0.05, 0.1, 0)

hhi1 <- diag(N)
hhi1[3:8,]  <- 0

hhi2 <- diag(N)
hhi2[c(1:2, 5:8),] <- 0

hhi3 <- diag(N)
hhi3[c(1:4, 7:8),] <- 0

hhi4 <- diag(N)
hhi4[1:6,]  <- 0

Q <- 2*var(R) + lambda_hhi[1]*hhi1 + lambda_hhi[2]*hhi2 + lambda_hhi[3]*hhi3 + lambda_hhi[4]*hhi4

sol <- solve.QP(Dmat=Q, dvec=rep(0, N), Amat=Amat, bvec=rhs, meq=1)
sol$solution
all.equal(opt3$weights, sol$solution, check.attributes=F)

