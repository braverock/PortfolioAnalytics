
##### GMV and QU QP Function #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param lambda_hhi concentration aversion parameter
#' @param conc_groups list of vectors specifying the groups of the assets. 
#' @author Ross Bennett
gmv_opt <- function(R, constraints, moments, lambda, target, lambda_hhi, conc_groups){

  N <- ncol(R)
  # Applying box constraints
  bnds <- list(lower=list(ind=seq.int(1L, N), val=as.numeric(constraints$min)),
               upper=list(ind=seq.int(1L, N), val=as.numeric(constraints$max)))
  
  # set up initial A matrix for leverage constraints
  Amat <- rbind(rep(1, N), rep(1, N))
  dir.vec <- c(">=","<=")
  rhs.vec <- c(constraints$min_sum, constraints$max_sum)
  
  # check for a target return
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
    Amat <- rbind(Amat, tmp_means)
    dir.vec <- c(dir.vec, "==")
    rhs.vec <- c(rhs.vec, target)
  }
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir.vec, and rhs.vec
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, t.B, -t.B)
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }
  
  # set up the quadratic objective
  if(!is.null(lambda_hhi)){
    if(length(lambda_hhi) == 1 & is.null(conc_groups)){
      ROI_objective <- Q_objective(Q=2*lambda*(moments$var + lambda_hhi * diag(N)), L=-moments$mean)
    } else if(!is.null(conc_groups)){
      # construct the matrix with concentration aversion values by group
      hhi_mat <- matrix(0, nrow=N, ncol=N)
      vec <- 1:N
      for(i in 1:length(conc_groups)){
        tmpI <- diag(N)
        tmpvec <- conc_groups[[i]]
        zerovec <- setdiff(vec, tmpvec)
        for(j in 1:length(zerovec)){
          tmpI[zerovec[j], ] <- rep(0, N)
        }
        hhi_mat <- hhi_mat + lambda_hhi[i] * tmpI
      }
      ROI_objective <- Q_objective(Q=2*lambda*(moments$var + hhi_mat), L=-moments$mean)
    }
  } else {
    ROI_objective <- Q_objective(Q=2*lambda*moments$var, L=-moments$mean)
  }
  # set up the optimization problem and solve
  opt.prob <- OP(objective=ROI_objective, 
                       constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                       bounds=bnds)
  roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
  
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return LP Function #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @author Ross Bennett
maxret_opt <- function(R, moments, constraints, target){
  
  N <- ncol(R)
  # Applying box constraints
  bnds <- list(lower=list(ind=seq.int(1L, N), val=as.numeric(constraints$min)),
               upper=list(ind=seq.int(1L, N), val=as.numeric(constraints$max)))
  
  # set up initial A matrix for leverage constraints
  Amat <- rbind(rep(1, N), rep(1, N))
  dir.vec <- c(">=","<=")
  rhs.vec <- c(constraints$min_sum, constraints$max_sum)
  
  # check for a target return
  if(!is.na(target)) {
    Amat <- rbind(Amat, moments$mean)
    dir.vec <- c(dir.vec, "==")
    rhs.vec <- c(rhs.vec, target)
  }
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir.vec, and rhs.vec
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, t.B, -t.B)
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }
  
  # set up the linear objective
  ROI_objective <- L_objective(L=-moments$mean)
  # objL <- -moments$mean
  
  # set up the optimization problem and solve
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                 bounds=bnds)
  roi.result <- ROI_solve(x=opt.prob, solver="glpk")
  # roi.result <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=rhs.vec, bounds=bnds)
  
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(roi.result$status$code != 0) {
    message(roi.result$status$msg$message)
    stop("No solution")
    return(NULL)
  }
  
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return MILP Function #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @author Ross Bennett
maxret_milp_opt <- function(R, constraints, moments, target){
  
  N <- ncol(R)
  
  max_pos <- constraints$max_pos
  
  LB <- as.numeric(constraints$min)
  UB <- as.numeric(constraints$max)
  
  # Check for target return
  if(!is.na(target)){
    # We have a target
    targetcon <- rbind(c(moments$mean, rep(0, N)),
                       c(-moments$mean, rep(0, N)))
    targetdir <- c("<=", "==")
    targetrhs <- c(Inf, -target)
  } else {
    # No target specified, just maximize
    targetcon <- NULL
    targetdir <- NULL
    targetrhs <- NULL
  }
  
  Amat <- rbind(c(rep(1, N), rep(0, N)),
                c(rep(1, N), rep(0, N)))
  Amat <- rbind(Amat, targetcon)
  Amat <- rbind(Amat, cbind(-diag(N), diag(LB)))
  Amat <- rbind(Amat, cbind(diag(N), -diag(UB)))
  Amat <- rbind(Amat, c(rep(0, N), rep(1, N)))
  
  dir <- c("<=", ">=", targetdir, rep("<=", 2*N), "==")
  
  rhs <- c(1, 1, targetrhs, rep(0, 2*N), max_pos)
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(data=0, nrow=nrow(Amat.group), ncol=ncol(Amat.group))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  objL <- c(-moments$mean, rep(0, N))
  
  # Only seems to work if I do not specify bounds
  # bounds = list( lower=list( ind=1L:(2*N), val=c(LB, rep(0, N)) ),
  #                upper=list( ind=1L:(2*N), val=c(UB, rep(1, N)) ) )
  bnds <- NULL
  
  # Set up the types vector with continuous and binary variables
  types <- c(rep("C", N), rep("B", N))
  
  # Solve directly with Rglpk... getting weird errors with ROI
  result <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir, rhs=rhs, types=types, bounds=bnds, max=FALSE)
  
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(result$status != 0) {
    message("Undefined Solution")
    return(NULL)
  }
  
  weights <- result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- result$optimum
  #out$call <- call # add this outside of here, this function doesn't have the call
  return(out)
}

##### Minimize ETL LP Function #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @author Ross Bennett
etl_opt <- function(R, constraints, moments, target, alpha){
  
  N <- ncol(R)
  T <- nrow(R)
  # Applying box constraints
  bnds <- list(lower=list(ind=seq.int(1L, N), val=as.numeric(constraints$min)),
               upper=list(ind=seq.int(1L, N), val=as.numeric(constraints$max)))
  
  Rmin <- ifelse(is.na(target), 0, target)
  
  Amat <- cbind(rbind(1, 1, moments$mean, coredata(R)), rbind(0, 0, 0, cbind(diag(T), 1))) 
  dir.vec <- c(">=","<=",">=",rep(">=",T))
  rhs.vec <- c(constraints$min_sum, constraints$max_sum, Rmin ,rep(0, T))
  
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(0, nrow=n.groups, ncol=(T+1))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=(T+1))
    Amat <- rbind(Amat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }
  
  ROI_objective <- L_objective(c(rep(0,N), rep(1/(alpha*T),T), 1))
  opt.prob <- OP(objective=ROI_objective, 
                       constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                       bounds=bnds)
  roi.result <- ROI_solve(x=opt.prob, solver="glpk")
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  #out$call <- call # add this outside of here, this function doesn't have the call
  return(out)
}

##### Minimize ETL MILP Function #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @author Ross Bennett
etl_milp_opt <- function(R, constraints, moments, target, alpha){
  
  # Number of rows
  n <- nrow(R)
  
  # Number of columns
  m <- ncol(R)
  
  max_sum <- constraints$max_sum
  min_sum <- constraints$min_sum
  LB <- constraints$min
  UB <- constraints$max
  max_pos <- constraints$max_pos
  moments_mean <- as.numeric(moments$mean)
  
  # A benchmark can be specified in the parma package. 
  # Leave this in and set to 0 for now
  benchmark <- 0
  
  # Check for target return
  if(!is.na(target)){
    # We have a target
    targetcon <- c(moments_mean, rep(0, n+2))
    targetdir <- "=="
    targetrhs <- target
  } else {
    # No target specified, just maximize
    targetcon <- NULL
    targetdir <- NULL
    targetrhs <- NULL
  }
  
  # Set up initial A matrix
  tmpAmat <- cbind(-coredata(R),
                   matrix(-1, nrow=n, ncol=1), 
                   -diag(n),
                   matrix(benchmark, nrow=n, ncol=1))
  
  # Add leverage constraints to matrix
  tmpAmat <- rbind(tmpAmat, rbind(c(rep(1, m), rep(0, n+2)),
                                  c(rep(1, m), rep(0, n+2))))
  
  # Add target return to matrix
  tmpAmat <- rbind(tmpAmat, as.numeric(targetcon))
  
  # This step just adds m rows to the matrix to accept box constraints in the next step
  tmpAmat <- cbind(tmpAmat, matrix(0, ncol=m, nrow=dim(tmpAmat)[1]))
  
  # Add lower bound box constraints
  tmpAmat <- rbind(tmpAmat, cbind(-diag(m), matrix(0, ncol=n+2, nrow=m), diag(LB)))
  
  # Add upper bound box constraints
  tmpAmat <- rbind(tmpAmat, cbind(diag(m), matrix(0, ncol=n+2, nrow=m), diag(-UB)))
  
  # Add row for max_pos cardinality constraints
  tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(1, ncol=m, nrow=1))) 

  # Set up the rhs vector
  rhs <- c( rep(0, n), min_sum, max_sum, targetrhs, rep(0, 2*m), max_pos)
  
  # Set up the dir vector
  dir <- c( rep("<=", n), ">=", "<=", targetdir, rep("<=", 2*m), "==")
  
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=m)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(0, nrow=n.groups, ncol=(m + n + 2))
    tmpAmat <- rbind(tmpAmat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=(m + n + 2))
    tmpAmat <- rbind(tmpAmat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Linear objective vector
  objL <- c( rep(0, m), 1, rep(1/n, n) / alpha, 0, rep(0, m))
  
  # Set up the types vector with continuous and binary variables
  types <- c( rep("C", m), "C", rep("C", n), "C", rep("B", m))
  
  bounds <- list( lower = list( ind = 1L:(m + n + 2 + m), val = c(LB,  -1, rep(0, n), 1, rep(0, m)) ),
                  upper = list( ind = 1L:(m + n + 2 + m), val = c( UB, 1, rep(Inf, n), 1 , rep(1, m)) ) )
  
  result <- Rglpk_solve_LP(obj=objL, mat=tmpAmat, dir=dir, rhs=rhs, types=types, bounds=bounds)
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(result$status != 0) {
    message("Undefined Solution")
    return(NULL)
  }
  
  weights <- result$solution[1:m]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- result$optimum
  #out$call <- call # add this outside of here, this function doesn't have the call
  return(out)
}

##### minimize variance or maximize quadratic utility with turnover constraints #####
#' Optimization function to solve minimum variance or maximum quadratic utility problems
#' 
#' This function is called by optimize.portfolio to solve minimum variance or maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param init_weights initial weights to compute turnover
#' @author Ross Bennett
gmv_opt_toc <- function(R, constraints, moments, lambda, target, init_weights){
  # function for minimum variance or max quadratic utility problems
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  returns <- cbind(R, R, R)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # Amat for initial weights
  Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
  rhs <- init_weights
  dir <- rep("==", N)
  meq <- 4
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
    Amat <- rbind(Amat, rep(tmp_means, 3))
    dir <- c(dir, "==")
    rhs <- c(rhs, target)
    meq <- 5
  }
  
  # Amat for full investment constraint
  Amat <- rbind(Amat, rbind(rep(1, N*3), rep(-1, N*3)))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(N), diag(N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), -diag(N), -diag(N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # Amat for turnover constraints
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(1, N)))
  rhs <- c(rhs, -constraints$toc)
  dir <- c(dir, ">=")
  
  # Amat for positive weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for negative weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), -diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, Amat.group, Amat.group))
    Amat <- rbind(Amat, cbind(-Amat.group, -Amat.group, -Amat.group))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, cbind(t.B, t.B, t.B))
    Amat <- rbind(Amat, cbind(-t.B, -t.B, -t.B))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  d <- rep(-moments$mean, 3)
  
  qp.result <- try(solve.QP(Dmat=make.positive.definite(2*lambda*V), 
                            dvec=d, Amat=t(Amat), bvec=rhs, meq=meq), silent=TRUE)
  if(inherits(qp.result, "try-error")) stop("No solution found, consider adjusting constraints.")
  
  wts <- qp.result$solution
  wts.final <- wts[(1:N)] + wts[(1+N):(2*N)] + wts[(2*N+1):(3*N)]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- qp.result$val
  return(out)
  
  # TODO
  # Get this working with ROI
  
  # Not getting solution using ROI
  # set up the quadratic objective
  # ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), L=rep(-moments$mean, 3))
  
  # opt.prob <- OP(objective=ROI_objective, 
  #                constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))
  # roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
}

# proportional transaction cost constraint
gmv_opt_ptc <- function(R, constraints, moments, lambda, target, init_weights){
  # function for minimum variance or max quadratic utility problems
  # modifying ProportionalCostOpt function from MPO package
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  returns <- cbind(R, R, R)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # Amat for initial weights
  Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
  rhs <- init_weights
  dir <- rep("==", N)
  meq <- 4
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
    Amat <- rbind(Amat, rep((1+tmp_means), 3))
    dir <- c(dir, "==")
    rhs <- c(rhs, (1+target))
    meq <- 5
  }
  
  # Amat for positive weights for w.buy and w.sell
  weights.positive <- rbind(matrix(0,ncol=2*N,nrow=N),diag(2*N))
  temp.index <- (N*3-N+1):(N*3)
  weights.positive[temp.index,] <- -1*weights.positive[temp.index,]
  Amat <- rbind(Amat, t(weights.positive))
  rhs <- c(rhs, rep(0, 2*N))
  
  # Amat for full investment constraint
  ptc <- constraints$ptc
  Amat <- rbind(Amat, rbind(c(rep(1, N), (1+ptc), (1-ptc)), -c(rep(1, N), (1+ptc), (1-ptc))))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(N), diag(N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), -diag(N), -diag(N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, Amat.group, Amat.group))
    Amat <- rbind(Amat, cbind(-Amat.group, -Amat.group, -Amat.group))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, cbind(t.B, t.B, t.B))
    Amat <- rbind(Amat, cbind(-t.B, -t.B, -t.B))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  d <- rep(-moments$mean, 3)
  
  qp.result <- try(solve.QP(Dmat=corpcor:::make.positive.definite(2*lambda*V), 
                            dvec=d, Amat=t(Amat), bvec=rhs, meq=meq), silent=TRUE)
  if(inherits(qp.result, "try-error")) stop("No solution found, consider adjusting constraints.")
  
  wts <- qp.result$solution
  w.buy <- qp.result$solution[(N+1):(2*N)]
  w.sell <- qp.result$solution[(2*N+1):(3*N)]
  w.total <- init_weights + w.buy + w.sell
  wts.final <- wts[(1:N)] + wts[(1+N):(2*N)] + wts[(2*N+1):(3*N)]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- qp.result$val
  return(out)
  
  # TODO
  # Get this working with ROI
  
  # Not getting solution using ROI
  # set up the quadratic objective
  # ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), L=rep(-moments$mean, 3))
  
  # opt.prob <- OP(objective=ROI_objective, 
  #                constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))
  # roi.result <- ROI_solve(x=opt.prob, solver="quadprog")
}
