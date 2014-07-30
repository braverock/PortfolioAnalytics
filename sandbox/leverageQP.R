
# leverage constrained minimum variance QP

library(PortfolioAnalytics)
library(corpcor)
library(quadprog)

data(edhec)
R <- edhec[, 1:4]

N <- ncol(R)
leverage <- 1.6
min_sum <- 0.99
max_sum <- 1.01
min_box <- rep(-0.3, N)
max_box <- rep(1, N)
lambda <- 1

R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
returns <- cbind(R, R0, R0)
V <- corpcor::make.positive.definite(cov(returns))

# separate the weights into w, w+, and w-
# w - w+ + w- = 0
Amat <- cbind(diag(N), -diag(N), diag(N))
rhs <- rep(0, N)
dir <- rep("==", N)

# leverage constraint
# w+ + w- <= leverage
Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(-1, N)))
rhs <- c(rhs, -leverage)
dir <- c(dir, ">=")

# w+ >= 0
Amat <- rbind(Amat, cbind(diag(0, N), diag(N), diag(0, N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# w- >= 0
Amat <- rbind(Amat, cbind(diag(0, N), diag(0, N), diag(N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# w^T 1 >= min_sum
Amat <- rbind(Amat, c(rep(1, N), rep(0, N), rep(0, N)))
rhs <- c(rhs, min_sum)
dir <- c(dir, ">=")

# w^T 1 <= max_sum
Amat <- rbind(Amat, c(rep(-1, N), rep(0, N), rep(0, N)))
rhs <- c(rhs, -max_sum)
dir <- c(dir, ">=")

# lower box constraints
Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, min_box)
dir <- c(dir, rep(">=", N))

# upper box constraints
Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, -max_box)
dir <- c(dir, rep(">=", N))

sol <- solve.QP(Dmat=V, dvec=rep(0, 3*N), Amat=t(Amat), bvec=rhs, meq=N)
sol

weights <- sol$solution[1:N]
round(weights, 4)
sum(weights)
sum(abs(weights)) <= leverage

##### ROI #####
ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), 
                             L=rep(0, N*3))

opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))

roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
wts <- roi.result$solution[1:N]
round(wts, 4)
sum(wts)
sum(abs(wts)) <= leverage

# The quadprog and ROI solution should result in the same solution using the
# same Amat, dir, and rhs objects
all.equal(weights, wts)

# Load the package and data
# funds <- colnames(R)

# Construct initial portfolio with basic constraints.
# init.portf <- portfolio.spec(assets=funds)
# init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
# init.portf <- add.constraint(portfolio=init.portf, type="box", min=-0.3, max=1)
# init.portf <- add.constraint(portfolio=init.portf, type="leverage_exposure", leverage=1.6)
# init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
# 
# opt <- optimize.portfolio(R, init.portf, optimize_method="ROI")
# round(opt$weights, 4)
