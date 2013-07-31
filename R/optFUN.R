
##### GMV and QU QP Function #####
gmv_opt <- function(R, constraints, moments, lambda, target){
  
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
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- constraints$groups[i] 
      Amat.group[i, k:(l+j)] <- 1
      k <- l + j + 1
      l <- k - 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  
  # set up the quadratic objective
  ROI_objective <- Q_objective(Q=2*lambda*moments$var, L=-moments$mean)
  
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
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- constraints$groups[i] 
      Amat.group[i, k:(l+j)] <- 1
      k <- l + j + 1
      l <- k - 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
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
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- constraints$groups[i] 
      Amat.group[i, k:(l+j)] <- 1
      k <- l + j + 1
      l <- k - 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(data=0, nrow=nrow(Amat.group), ncol=ncol(Amat.group))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
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
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- constraints$groups[i] 
      Amat.group[i, k:(l+j)] <- 1
      k <- l + j + 1
      l <- k - 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(0, nrow=n.groups, ncol=(T+1))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
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
