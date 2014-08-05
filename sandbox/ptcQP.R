
# proportional transaction costs minimum variance QP

library(PortfolioAnalytics)
library(corpcor)
library(quadprog)
library(ROI)
library(ROI.plugin.quadprog)

data(edhec)
R <- edhec[, 1:4]

N <- ncol(R)
mu <- colMeans(R)
mu_target <- median(mu)
w_init <- rep(1 / N, N)
tcb <- tcs <- rep(0.01, N)
min_sum <- 0.99
max_sum <- 1.01
min_box <- rep(0, N)
max_box <- rep(1, N)
lambda <- 1

R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
returns <- cbind(R, R0, R0)
V <- corpcor::make.positive.definite(cov(returns))

Amat <- matrix(c(1 + mu, rep(0, 2 * N)), nrow=1)
rhs <- 1 + mu_target
dir <- "=="

# separate the weights into w, w+, and w-
# w - w+ + w- = 0
Amat <- rbind(Amat, cbind(diag(N), -diag(N), diag(N)))
rhs <- c(rhs, w_init)
dir <- c(dir, rep("==", N))
meq <- N + 1

# w+ >= 0
Amat <- rbind(Amat, cbind(diag(0, N), diag(N), diag(0, N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# w- >= 0
Amat <- rbind(Amat, cbind(diag(0, N), diag(0, N), diag(N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# 1^T w + tcb^T w^+ + tcs^T w^- >= min_sum
Amat <- rbind(Amat, c(rep(1, N), tcb, tcs))
rhs <- c(rhs, min_sum)
dir <- c(dir, ">=")

# 1^T w + tcb^T w^+ + tcs^T w^- >= min_sum
Amat <- rbind(Amat, c(rep(-1, N), -tcb, -tcs))
rhs <- c(rhs, -max_sum)
dir <- c(dir, ">=")

# -(1 + tcb)^T w^+ + (1 - tcs)^T w^- >= 0
Amat <- rbind(Amat, c(rep(0, N), -(1 + tcb), (1 - tcs)))
rhs <- c(rhs, 0)
dir <- c(dir, ">=")

# lower box constraints
Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, min_box)
dir <- c(dir, rep(">=", N))

# upper box constraints
Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
rhs <- c(rhs, -max_box)
dir <- c(dir, rep(">=", N))

sol <- solve.QP(Dmat=V, dvec=rep(0, 3*N), Amat=t(Amat), bvec=rhs, meq=meq)
sol

weights <- sol$solution[1:N]
round(weights, 4)
sum(weights * mu)

##### ROI #####
ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), 
                             L=rep(0, N*3))

opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))

roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
wts <- roi.result$solution[1:N]
round(wts, 4)
sum(wts)

# The quadprog and ROI solution should result in the same solution using the
# same Amat, dir, and rhs objects
all.equal(weights, wts)

