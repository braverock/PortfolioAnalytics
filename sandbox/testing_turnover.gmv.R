
# script to solve the gmv optimization problem with turnover constraints using quadprog or ROI

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(quadprog)
library(corpcor)

data(edhec)
R <- edhec[, 1:4]
init.weights <- rep(1/4, 4)
min <- rep(0.1, 4)
max <- rep(0.6, 4)
toc <- 0.8
lambda <- 0.25
mu <- colMeans(R)
target <- 0.0071

group_mat <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
grp_min <- c(0.05, 0.05)
grp_max <- c(0.85, 0.85)

# Computes optimal weights for global minimum variance portfolio with
# constraints including turnover constraint

# number of assets in R
p <- ncol(R)

# Modify the returns matrix. This is done because there are 3 sets of
# variables w.initial, w.buy, and w.sell
returns <- cbind(R, R, R)

V <- cov(returns)
V <- make.positive.definite(V)

# A matrix for full investment, mean, and initial weights
A2 <- cbind(rbind(diag(p), matrix(0, nrow=2*p, ncol=p)),
            rep(mu, 3),
            rep(1, p*3), rep(-1, p*3))

# A matrix for lower box constraints
Alo <- rbind(diag(p), diag(p), diag(p))

# A matrix for upper box constraints
Aup <- rbind(-diag(p), -diag(p), -diag(p))

# vector to appply turnover constraint
A3 <- c(rep(0, p), rep(-1, p), rep(1, p))

# matrix for positive weight
A4 <- rbind(matrix(0, nrow=p, ncol=p), diag(p), matrix(0, nrow=p, ncol=p))

# matrix for negative weight
A5 <- rbind(matrix(0, nrow=p*2, ncol=p), -diag(p))

# Combine the temporary A matrices
A.c <- cbind(A2, Alo, Aup, A3, A4, A5)

# Constraints matrix for group_mat
A.c <- cbind(A.c, rbind(group_mat, group_mat, group_mat))
A.c <- cbind(A.c, rbind(-group_mat, -group_mat, -group_mat))

# b vector holding the values of the constraints
b <- c(init.weights, target, 0.99, -1.01, min, -max, -toc, rep(0, 2*p))
b <- c(b, grp_min, -grp_max)

# no linear term so set this equal to 0s
d <- rep(0, p*3)
# d <- rep(-mu, 3)

sol <- try(solve.QP(Dmat=make.positive.definite(2*lambda*V), dvec=d, 
                Amat=A.c, bvec=b, meq=4), silent=TRUE)
if(inherits(sol, "try-error")) message("No solution found")
wts <- sol$solution
wts.final <- wts[(1:p)] + wts[(1+p):(2*p)] + wts[(2*p+1):(3*p)]
wts.final
sum(wts.final)
wts.final %*% mu

# calculate turnover
to <- sum(abs(diff(rbind(init.weights, wts.final))))
to

##### ROI Turnover constraints using ROI solver #####

N <- ncol(R)

tmpR <- cbind(R, R, R)
V <- var(tmpR)
V <- corpcor:::make.positive.definite(V)
# lambda <- 0.25
# mu <- colMeans(R)

# Amat for full investment constraint
Amat <- rbind(rep(1, N*3), rep(1, N*3))
rhs <- c(1, 1)
dir <- c("==", "==")
# dir <- c(">=", "<=")

# Amat for initial weights
Amat <- rbind(Amat, cbind(diag(N), matrix(0, nrow=N, ncol=N*2)))
rhs <- c(rhs, init.weights)
dir <- c(dir, rep("==", N))

# Amat for lower box constraints
Amat <- rbind(Amat, cbind(diag(N), diag(N), diag(N)))
rhs <- c(rhs, min)
dir <- c(dir, rep(">=", N))

# Amat for upper box constraints
Amat <- rbind(Amat, cbind(-diag(N), -diag(N), -diag(N)))
rhs <- c(rhs, -max)
dir <- c(dir, rep(">=", N))

# Amat for turnover constraints
Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(1, N)))
rhs <- c(rhs, -toc)
dir <- c(dir, ">=")

# Amat for positive weights
Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# Amat for negative weights
Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), -diag(N)))
rhs <- c(rhs, rep(0, N))
dir <- c(dir, rep(">=", N))

# set up the quadratic objective
ROI_objective <- Q_objective(Q=make.positive.definite(2 * lambda * V), L=rep(0, N*3))

opt.prob <- OP(objective=ROI_objective, 
               constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))
roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
# not sure why no solution with ROI
print.default(roi.result)

# check that the same constraints matrix and rhs vectors are used
all.equal(t(Amat), A.c, check.attributes=FALSE)
all.equal(rhs, b)

# run solve.QP using Amat and rhs from ROI problem
qp.result <- solve.QP(Dmat=make.positive.definite(2*lambda*V), 
                      dvec=rep(0, N*3), Amat=t(Amat), bvec=rhs, meq=6)

# results with solve.QP are working, but not with ROI
all.equal(qp.result$solution, sol$solution)
all.equal(roi.result$solution, sol$solution)

roi.result$solution
qp.result$solution
sol$solution



