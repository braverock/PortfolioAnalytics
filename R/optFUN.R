
##### GMV and QU QP Function #####
#' GMV/QU QP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param lambda_hhi concentration aversion parameter
#' @param conc_groups list of vectors specifying the groups of the assets.
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
gmv_opt <- function(R, constraints, moments, lambda, target, lambda_hhi, conc_groups, solver="quadprog", control=NULL){
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI", quietly=TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of assets
  N <- ncol(R)
  
  # Check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- tmp_means
  dir.vec <- "=="
  rhs.vec <- target
  meq <- 1
  
  # Set up initial A matrix for leverage constraints
  Amat <- rbind(Amat, rep(1, N), rep(-1, N))
  dir.vec <- c(dir.vec, ">=",">=")
  rhs.vec <- c(rhs.vec, constraints$min_sum, -constraints$max_sum)
  
  # Add min box constraints
  Amat <- rbind(Amat, diag(N))
  dir.vec <- c(dir.vec, rep(">=", N))
  rhs.vec <- c(rhs.vec, constraints$min)
  
  # Add max box constraints
  Amat <- rbind(Amat, -1*diag(N))
  dir.vec <- c(dir.vec, rep(">=", N))
  rhs.vec <- c(rhs.vec, -constraints$max)
  
  # Applying box constraints
  lb <- constraints$min
  ub <- constraints$max
  
  bnds <- ROI::V_bound(li=seq.int(1L, N), lb=as.numeric(lb),
                  ui=seq.int(1L, N), ub=as.numeric(ub))
  
  # Include group constraints
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

  # quadprog cannot handle infinite values so replace Inf with .Machine$double.xmax
  # This is the strategy used in ROI
  # Amat[ is.infinite(Amat) & (Amat <= 0) ] <- -.Machine$double.xmax
  # Amat[ is.infinite(Amat) & (Amat >= 0) ] <-  .Machine$double.xmax
  # rhs.vec[is.infinite(rhs.vec) & (rhs.vec <= 0)] <- -.Machine$double.xmax
  # rhs.vec[is.infinite(rhs.vec) & (rhs.vec >= 0)] <- .Machine$double.xmax
  
  # Remove the rows of Amat and elements of dir.vec and rhs.vec where rhs.vec is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs.vec), ]
  dir.vec <- dir.vec[!is.infinite(rhs.vec)]
  rhs.vec <- rhs.vec[!is.infinite(rhs.vec)]
  
  # Set up the quadratic objective
  if(!is.null(lambda_hhi)){
    if(length(lambda_hhi) == 1 & is.null(conc_groups)){
      ROI_objective <- ROI::Q_objective(Q=2*lambda*(moments$var + lambda_hhi * diag(N)), L=-moments$mean) # ROI
      #Dmat <- 2*lambda*(moments$var + lambda_hhi * diag(N)) # solve.QP
      #dvec <- moments$mean # solve.QP
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
      ROI_objective <- ROI::Q_objective(Q=2*lambda*(moments$var + hhi_mat), L=-moments$mean) # ROI
      #Dmat <- 2 * lambda * (moments$var + hhi_mat) # solve.QP
      #dvec <- moments$mean # solve.QP
    }
  } else {
    ROI_objective <- ROI::Q_objective(Q=2*lambda*moments$var, L=-moments$mean) # ROI
    #Dmat <- 2 * lambda * moments$var # solve.QP
    #dvec <- moments$mean # solve.QP
  }
  # set up the optimization problem and solve
  opt.prob <- ROI::OP(objective=ROI_objective, 
                       constraints=ROI::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                 bounds=bnds)
  result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  
  # result <- try(solve.QP(Dmat=Dmat, dvec=dvec, Amat=t(Amat), bvec=rhs.vec, meq=meq), silent=TRUE)
  if(inherits(x=result, "try-error")) stop(paste("No solution found:", result))
  
  weights <- result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  # out$out <- result$objval # ROI
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return LP Function #####
#' Maximum Return LP Optimization
#' 
#' This function is called by optimize.portfolio to solve maximum return
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
maxret_opt <- function(R, moments, constraints, target, solver="glpk", control=NULL){
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI",quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  N <- ncol(R)
  # Applying box constraints
  # maxret_opt needs non infinite values for upper and lower bounds
  lb <- constraints$min
  ub <- constraints$max
  if(any(is.infinite(lb)) | any(is.infinite(ub))){
    warning("Inf or -Inf values detected in box constraints, maximum return 
            objectives must have finite box constraint values.")
    ub[is.infinite(ub)] <- max(abs(c(constraints$min_sum, constraints$max_sum)))
    lb[is.infinite(lb)] <- 0
  }
  bnds <- ROI::V_bound(li=seq.int(1L, N), lb=as.numeric(lb),
                  ui=seq.int(1L, N), ub=as.numeric(ub))
  
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
  ROI_objective <- ROI::L_objective(L=-moments$mean)
  # objL <- -moments$mean
  
  # set up the optimization problem and solve
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                 bounds=bnds)
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
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
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  port.mean <- -roi.result$objval
  names(port.mean) <- "mean"
  obj_vals[["mean"]] <- port.mean
  out$obj_vals <- obj_vals
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return MILP Function #####
#' Maximum Return MILP Optimization
#' 
#' This function is called by optimize.portfolio to solve maximum return 
#' problems via mixed integer linear programming.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
maxret_milp_opt <- function(R, constraints, moments, target, solver="glpk", control=NULL){
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI",quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of assets
  N <- ncol(R)
  
  # Maximum number of positions (non-zero weights)
  max_pos <- constraints$max_pos
  min_pos <- 1
  
  # Upper and lower bounds on weights
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
  
  # weight_sum constraint
  Amat <- rbind(c(rep(1, N), rep(0, N)),
                c(rep(1, N), rep(0, N)))
  
  # Target return constraint
  Amat <- rbind(Amat, targetcon)
  
  # Bounds and position limit constraints
  Amat <- rbind(Amat, cbind(-diag(N), diag(LB)))
  Amat <- rbind(Amat, cbind(diag(N), -diag(UB)))
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N)))
  Amat <- rbind(Amat, c(rep(0, N), rep(1, N)))
  
  dir <- c("<=", ">=", targetdir, rep("<=", 2*N), "<=", "<=")
  rhs <- c(1, 1, targetrhs, rep(0, 2*N), -min_pos, max_pos)
  
  # Include group constraints
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
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Only seems to work if I do not specify bounds
  # bnds <- ROI::V_Bound(li=seq.int(1L, 2*m), lb=c(as.numeric(constraints$min), rep(0, m)),
  #                 ui=seq.int(1L, 2*m), ub=c(as.numeric(constraints$max), rep(1, m)))
  bnds <- NULL
  
  # Set up the types vector with continuous and binary variables
  types <- c(rep("C", N), rep("B", N))
  
  # Set up the linear objective to maximize mean return
  ROI_objective <- ROI::L_objective(L=c(-moments$mean, rep(0, N)))
  
  # Set up the optimization problem and solve
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=Amat, dir=dir, rhs=rhs),
                 bounds=bnds, types=types)
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  # Weights
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  
  # The out object is returned
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  
  obj_vals <- list()
  port.mean <- -roi.result$objval
  names(port.mean) <- "mean"
  obj_vals[["mean"]] <- port.mean
  out$obj_vals <- obj_vals
  return(out)
}

##### Minimize ETL LP Function #####
#' Minimum ETL LP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum ETL problems.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
etl_opt <- function(R, constraints, moments, target, alpha, solver="glpk", control=NULL){
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI",quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  N <- ncol(R)
  T <- nrow(R)
  # Applying box constraints
  LB <- c(as.numeric(constraints$min), rep(0, T), -1)
  UB <- c(as.numeric(constraints$max), rep(Inf, T), 1)
  bnds <- ROI::V_bound(li=seq.int(1L, N+T+1), lb=LB,
                  ui=seq.int(1L, N+T+1), ub=UB)
  
  # Add this check if mean is not an objective and return is a constraints
  if(!is.na(target)){
    if(all(moments$mean == 0)){
      moments$mean <- colMeans(R)
    }
  } else {
    moments$mean <- rep(0, N)
    target <- 0
  }
  
  Amat <- cbind(rbind(1, 1, moments$mean, coredata(R)), rbind(0, 0, 0, cbind(diag(T), 1))) 
  dir.vec <- c(">=","<=",">=",rep(">=",T))
  rhs.vec <- c(constraints$min_sum, constraints$max_sum, target ,rep(0, T))
  
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
  
  ROI_objective <- ROI::L_objective(c(rep(0,N), rep(1/(alpha*T),T), 1))
  opt.prob <- ROI::OP(objective=ROI_objective, 
                       constraints=ROI::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                       bounds=bnds)
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(x=roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  es_names <- c("ES", "ETL", "CVaR")
  es_idx <- which(es_names %in% names(moments))
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  } else {
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### Minimize ETL MILP Function #####
#' Minimum ETL MILP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum ETL problems 
#' via mixed integer linear programming.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
etl_milp_opt <- function(R, constraints, moments, target, alpha, solver="glpk", control=NULL){
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI",quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of rows
  n <- nrow(R)
  
  # Number of columns
  m <- ncol(R)
  
  max_sum <- constraints$max_sum
  min_sum <- constraints$min_sum
  LB <- constraints$min
  UB <- constraints$max
  max_pos <- constraints$max_pos
  min_pos <- 1
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
  
  # Add rows cardinality constraints
  tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(-1, ncol=m, nrow=1))) 
  tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(1, ncol=m, nrow=1)))
  
  # Set up the rhs vector
  rhs <- c( rep(0, n), min_sum, max_sum, targetrhs, rep(0, 2*m), -min_pos, max_pos)
  
  # Set up the dir vector
  dir <- c( rep("<=", n), ">=", "<=", targetdir, rep("<=", 2*m), "<=", "<=")
  
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
  ROI_objective <- ROI::L_objective(c( rep(0, m), 1, rep(1/n, n) / alpha, 0, rep(0, m)))
  
  # Set up the types vector with continuous and binary variables
  types <- c( rep("C", m), "C", rep("C", n), "C", rep("B", m))
  
  bnds <- ROI::V_bound( li = 1L:(m + n + 2 + m), lb = c(LB, -1, rep(0, n), 1, rep(0, m)),
                   ui = 1L:(m + n + 2 + m), ub = c(UB, 1, rep(Inf, n), 1, rep(1, m)))
  
  # Set up the optimization problem and solve  
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=tmpAmat, dir=dir, rhs=rhs),
                 bounds=bnds, types=types)
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(roi.result$status$code != 0) {
    message("Undefined Solution")
    return(NULL)
  }
  
  weights <- roi.result$solution[1:m]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  es_names <- c("ES", "ETL", "CVaR")
  es_idx <- which(es_names %in% names(moments))
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  } else {
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### minimize variance or maximize quadratic utility with turnover constraints #####
#' GMV/QU QP Optimization with Turnover Constraint
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems with turnover constraint
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param init_weights initial weights to compute turnover
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
gmv_opt_toc <- function(R, constraints, moments, lambda, target, init_weights, solver="quadprog", control=NULL){
  # function for minimum variance or max quadratic utility problems
  stopifnot("package:corpcor" %in% search() || requireNamespace("corpcor",quietly = TRUE))
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI", quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
  returns <- cbind(R, R0, R0)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- c(tmp_means, rep(0, 2*N))
  dir <- "=="
  rhs <- target
  meq <- N + 1
  
  # Amat for initial weights
  # Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
  Amat <- rbind(Amat, cbind(diag(N), -1*diag(N), diag(N)))
  rhs <- c(rhs, init_weights)
  dir <- c(dir, rep("==", N))
  
  # Amat for turnover constraints
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(-1, N)))
  rhs <- c(rhs, -constraints$turnover_target)
  dir <- c(dir, ">=")
  
  # Amat for positive weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for negative weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for full investment constraint
  Amat <- rbind(Amat, rbind(c(rep(1, N), rep(0,2*N)), c(rep(-1, N), rep(0,2*N))))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    zeros <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, zeros, zeros))
    Amat <- rbind(Amat, cbind(-Amat.group, zeros, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros, zeros))
    Amat <- rbind(Amat, cbind(-t.B, zeros, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Remove the rows of Amat and elements of rhs.vec where rhs is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs), ]
  rhs <- rhs[!is.infinite(rhs)]
  dir <- dir[!is.infinite(rhs)]
  
  ROI_objective <- ROI::Q_objective(Q=corpcor::make.positive.definite(2*lambda*V), 
                               L=rep(-tmp_means, 3))
  
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=Amat, dir=dir, rhs=rhs))
  
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  wts <- roi.result$solution
  wts.final <- wts[1:N]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### minimize variance or maximize quadratic utility with proportional transactioncosts constraints #####
#' GMV/QU QP Optimization with Proportional Transaction Cost Constraint
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems with proportional transaction cost constraint
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param init_weights initial weights to compute turnover
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
gmv_opt_ptc <- function(R, constraints, moments, lambda, target, init_weights, solver="quadprog", control=NULL){
  # function for minimum variance or max quadratic utility problems
  # modifying ProportionalCostOpt function from MPO package
  stopifnot("package:corpcor" %in% search() || requireNamespace("corpcor", quietly = TRUE))
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI", quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # proportional transaction costs
  ptc <- constraints$ptc
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
  returns <- cbind(R, R0, R0)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # Check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- c(tmp_means, rep(0, 2 * N))
  dir <- "=="
  rhs <- 1 + target
  meq <- 1
  
  # separate the weights into w, w^+, and w^-
  # w - w^+ + w^- = 0
  Amat <- rbind(Amat, cbind(diag(N), -diag(N), diag(N)))
  rhs <- c(rhs, init_weights)
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
  Amat <- rbind(Amat, c(rep(1, N), ptc, ptc))
  rhs <- c(rhs, constraints$min_sum)
  dir <- c(dir, ">=")
  
  # 1^T w + tcb^T w^+ + tcs^T w^- <= max_sum
  Amat <- rbind(Amat, c(rep(-1, N), -ptc, -ptc))
  rhs <- c(rhs, -constraints$max_sum)
  dir <- c(dir, ">=")
  
  # -(1 + tcb)^T w^+ + (1 - tcs)^T w^- >= 0
  Amat <- rbind(Amat, c(rep(0, N), -(1 + ptc), (1 - ptc)))
  rhs <- c(rhs, 0)
  dir <- c(dir, ">=")
  
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
  d <- c(-tmp_means, rep(0, 2 * N))
  
  # Remove the rows of Amat and elements of rhs where rhs is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs), ]
  rhs <- rhs[!is.infinite(rhs)]
  dir <- dir[!is.infinite(rhs)]
  
  ROI_objective <- ROI::Q_objective(Q=corpcor::make.positive.definite(2*lambda*V), L=d)
  
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=Amat, dir=dir, rhs=rhs))
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  wts <- roi.result$solution
  weights <- wts[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### minimize variance or maximize quadratic utility with leverage constraints #####
#' GMV/QU QP Optimization with Turnover Constraint
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems with a leverage constraint
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param solver solver to use
#' @param control list of solver control parameters
#' @author Ross Bennett
gmv_opt_leverage <- function(R, constraints, moments, lambda, target, solver="quadprog", control=NULL){
  # function for minimum variance or max quadratic utility problems
  stopifnot("package:corpcor" %in% search() || requireNamespace("corpcor",quietly = TRUE))
  stopifnot("package:ROI" %in% search() || requireNamespace("ROI", quietly = TRUE))
  plugin <- paste0("ROI.plugin.", solver)
  stopifnot(paste0("package:", plugin) %in% search() || requireNamespace(plugin, quietly=TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
  returns <- cbind(R, R0, R0)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- c(tmp_means, rep(0, 2*N))
  dir <- "=="
  rhs <- target
  # meq <- N + 1
  
  # separate the weights into w, w+, and w-
  # w - w+ + w- = 0
  Amat <- rbind(Amat, cbind(diag(N), -1*diag(N), diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep("==", N))
  
  # Amat for leverage constraints
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(-1, N)))
  rhs <- c(rhs, -constraints$leverage)
  dir <- c(dir, ">=")
  
  # Amat for positive weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for negative weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for full investment constraint
  Amat <- rbind(Amat, rbind(c(rep(1, N), rep(0,2*N)), 
                            c(rep(-1, N), rep(0,2*N))))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    zeros <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, zeros, zeros))
    Amat <- rbind(Amat, cbind(-Amat.group, zeros, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros, zeros))
    Amat <- rbind(Amat, cbind(-t.B, zeros, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Remove the rows of Amat and elements of rhs.vec where rhs is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs), ]
  rhs <- rhs[!is.infinite(rhs)]
  dir <- dir[!is.infinite(rhs)]
  
  ROI_objective <- ROI::Q_objective(Q=corpcor::make.positive.definite(2*lambda*V), 
                               L=rep(-tmp_means, 3))
  
  opt.prob <- ROI::OP(objective=ROI_objective, 
                 constraints=ROI::L_constraint(L=Amat, dir=dir, rhs=rhs))
  
  roi.result <- try(ROI::ROI_solve(x=opt.prob, solver=solver, control=control), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  wts <- roi.result$solution
  wts.final <- wts[1:N]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    # faster and more efficient way to compute t(w) %*% Sigma %*% w
    port.sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  return(out)
}

# This function uses optimize() to find the target return value that 
# results in the maximum starr ratio (mean / ES).
# returns the target return value
mean_etl_opt <- function(R, constraints, moments, alpha, solver, control){
  # create a copy of the moments that can be modified
  tmp_moments <- moments
  
  # Find the maximum return
  if(!is.null(constraints$max_pos)){
    max_ret <- maxret_milp_opt(R=R, constraints=constraints, moments=moments, target=NA, solver=solver, control=control)
  } else {
    max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, target=NA, solver=solver, control=control)
  }
  max_mean <- as.numeric(-max_ret$out)
  
  # Find the minimum return
  tmp_moments$mean <- -1 * moments$mean
  if(!is.null(constraints$max_pos)){
    min_ret <- maxret_milp_opt(R=R, constraints=constraints, moments=tmp_moments, target=NA, solver=solver, control=control)
  } else {
    min_ret <- maxret_opt(R=R, constraints=constraints, moments=tmp_moments, target=NA, solver=solver, control=control)
  }
  min_mean <- as.numeric(min_ret$out)
  
  # use optimize() to find the target return value that maximizes sharpe ratio
  opt <- try(optimize(f=starr_obj_fun, R=R, constraints=constraints, 
                      solver=solver, moments=moments, alpha=alpha,
                      lower=min_mean, upper=max_mean, control=control,
                      maximum=TRUE, tol=.Machine$double.eps), 
             silent=TRUE)
  if(inherits(opt, "try-error")){
    stop(paste("Objective function failed with message\n", opt))
    return(NULL)
  }
  return(opt$maximum)
}

# Function to calculate the starr ratio.
# Used as the objective function for optimize()
starr_obj_fun <- function(target_return, R, constraints, moments, alpha, solver, control){
  if(!is.null(constraints$max_pos)){
    opt <- etl_milp_opt(R=R, constraints=constraints, moments=moments, 
                        target=target_return, alpha=alpha, solver=solver, 
                        control=control)
  } else {
    opt <- etl_opt(R=R, constraints=constraints, moments=moments, 
                   target=target_return, alpha=alpha, solver=solver,
                   control=control)
  }
  weights <- matrix(opt$weights, ncol=1)
  opt_mean <- sum(weights * moments$mean)
  opt_etl <- as.numeric(opt$out)
  starr <- opt_mean / opt_etl
  return(starr)
}


# This was my implementation of a binary search for the maximum starr ratio 
# target return. Better to use optimize() in R rather than my method. -Ross Bennett
# mean_etl_opt <- function(R, constraints, moments, target, alpha, solver="glpk", tol=.Machine$double.eps^0.5, maxit=50){
#   # This function returns the target mean return that maximizes mean / etl (i.e. starr)
#   
#   # if all(moments$mean == 0) then the user did not specify mean as an objective,
#   # and we just want to return the target mean return value
#   if(all(moments$mean == 0)) return(target)
#   
#   fmean <- matrix(moments$mean, ncol=1)
#   
#   # can't use optimize.portfolio here, this function is called inside 
#   # optimize.portfolio and will throw an error message about nesting too deeply
#   
#   # Find the maximum return
#   if(!is.null(constraints$max_pos)){
#     max_ret <- maxret_milp_opt(R=R, constraints=constraints, moments=moments, target=NA, solver=solver)
#   } else {
#     max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, target=NA, solver=solver)
#   }
#   max_mean <- as.numeric(-max_ret$out)
#   
#   # Find the starr at the maximum etl portfolio
#   if(!is.null(constraints$max_pos)){
#     ub_etl <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=max_mean, alpha=alpha, solver=solver)
#   } else {
#     ub_etl <- etl_opt(R=R, constraints=constraints, moments=moments, target=max_mean, alpha=alpha, solver=solver)
#   }
#   ub_weights <- matrix(ub_etl$weights, ncol=1)
#   ub_mean <- as.numeric(t(ub_weights) %*% fmean)
#   ub_etl <- as.numeric(ub_etl$out)
#   # starr at the upper bound
#   ub_starr <- ub_mean / ub_etl
#   if(is.infinite(ub_starr)) stop("Inf value for STARR, objective value is 0")
#   
#   # cat("ub_mean", ub_mean, "\n")
#   # cat("ub_etl", ub_etl, "\n")
#   # cat("ub_starr", ub_starr, "\n")
#   
#   # Find the starr at the minimum etl portfolio
#   if(!is.null(constraints$max_pos)){
#     lb_etl <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=NA, alpha=alpha, solver=solver)
#   } else {
#     lb_etl <- etl_opt(R=R, constraints=constraints, moments=moments, target=NA, alpha=alpha, solver=solver)
#   }
#   lb_weights <- matrix(lb_etl$weights)  
#   lb_mean <- as.numeric(t(lb_weights) %*% fmean)  
#   lb_etl <- as.numeric(lb_etl$out)
#   
#   # starr at the lower bound
#   lb_starr <- lb_mean / lb_etl
#   # if(is.infinite(lb_starr)) stop("Inf value for STARR, objective value is 0")
#   
#   # set lb_starr equal to 0, should this be a negative number like -1e6?
#   # the lb_* values will be 0 for a dollar-neutral strategy so we need to reset the values
#   if(is.na(lb_starr) | is.infinite(lb_starr)) lb_starr <- 0
#   
#   # cat("lb_mean", lb_mean, "\n")
#   # cat("lb_etl", lb_etl, "\n")
#   # cat("lb_starr", lb_starr, "\n")
#   
#   # want to find the return that maximizes mean / etl
#   i <- 1
#   while((abs(ub_starr - lb_starr) > tol) & (i < maxit)){
#     # bisection method to find the maximum mean / etl
#     
#     # print(i)
#     # cat("ub_starr", ub_starr, "\n")
#     # cat("lb_starr", lb_starr, "\n")
#     # print("**********")
#     # Find the starr at the mean return midpoint
#     new_ret <- (lb_mean + ub_mean) / 2
#     if(!is.null(constraints$max_pos)){
#       mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#     } else {
#       mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#     }
#     # print(mid)
#     mid_weights <- matrix(mid$weights, ncol=1)
#     mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#     mid_etl <- as.numeric(mid$out)
#     mid_starr <- mid_mean / mid_etl
#     # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
#     # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
#     # tmp_starr <- mid_starr
#     
#     # cat("mid_mean", mid_mean, "\n")
#     # cat("mid_etl", mid_etl, "\n")
#     # cat("mid_starr", mid_starr, "\n")
#     
#     if(mid_starr > ub_starr){
#       # if mid_starr > ub_starr then mid_starr becomes the new upper bound
#       ub_mean <- mid_mean
#       ub_starr <- mid_starr
#       new_ret <- (lb_mean + ub_mean) / 2
#       if(!is.null(constraints$max_pos)){
#         mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#       } else {
#         mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#       }
#       mid_weights <- matrix(mid$weights, ncol=1)
#       mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#       mid_etl <- as.numeric(mid$out)
#       mid_starr <- mid_mean / mid_etl
#       # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
#       # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
#     } else if(mid_starr > lb_starr){
#       # if mid_starr > lb_starr then mid_starr becomes the new lower bound
#       lb_mean <- mid_mean
#       lb_starr <- mid_starr
#       new_ret <- (lb_mean + ub_mean) / 2
#       if(!is.null(constraints$max_pos)){
#         mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#       } else {
#         mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
#       }
#       mid_weights <- matrix(mid$weights, ncol=1)
#       mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#       mid_etl <- as.numeric(mid$out)
#       mid_starr <- mid_mean / mid_etl
#       # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
#       # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
#     }
#     i <- i + 1
#   }
#   return(new_ret)
# }



# Function to calculate the sharpe ratio.
# Used as the objective function for optimize()
sharpe_obj_fun <- function(target_return, R, constraints, moments, lambda_hhi=NULL, conc_groups=NULL, solver="quadprog", control=control){
  opt <- gmv_opt(R=R, constraints=constraints, moments=moments, lambda=1, 
                 target=target_return, lambda_hhi=lambda_hhi,
                 conc_groups=conc_groups, solver=solver, control=control)
  weights <- opt$weights
  # opt_mean <- as.numeric(t(weights) %*% matrix(moments$mean, ncol=1))
  opt_mean <- sum(weights * moments$mean)
  # opt_sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
  # faster and more efficient way to compute t(w) %*% Sigma %*% w
  opt_sd <- sqrt(sum(crossprod(weights, moments$var) * weights))
  opt_sr <- opt_mean / opt_sd
  return(opt_sr)
}

# This function uses optimize() to find the target return value that 
# results in the maximum sharpe ratio (mean / sd).
# returns the target return value
max_sr_opt <- function(R, constraints, moments, lambda_hhi, conc_groups, solver, control){
  # create a copy of the moments that can be modified
  tmp_moments <- moments
  
  # Find the maximum return
  max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, 
                        target=NA, solver="glpk", control=control)
  max_mean <- as.numeric(-max_ret$out)
  
  # Find the minimum return
  tmp_moments$mean <- -1 * moments$mean
  min_ret <- maxret_opt(R=R, moments=tmp_moments, constraints=constraints, 
                        target=NA, solver="glpk", control=control)
  min_mean <- as.numeric(min_ret$out)
  
  # use optimize() to find the target return value that maximizes sharpe ratio
  opt <- try(optimize(f=sharpe_obj_fun, R=R, constraints=constraints, 
                      solver=solver, lambda_hhi=lambda_hhi, 
                      conc_groups=conc_groups, moments=moments, control=control,
                      lower=min_mean, upper=max_mean, 
                      maximum=TRUE, tol=.Machine$double.eps), 
             silent=TRUE)
  if(inherits(opt, "try-error")){
    stop(paste("Objective function failed with message\n", opt))
    return(NULL)
  }
  return(opt$maximum)
}


# This was my implementation of a binary search for the maximum sharpe ratio 
# target return. Better to use optimize() in R rather than my method. -Ross Bennett
# max_sr_opt <- function(R, constraints, moments, lambda, target, lambda_hhi, conc_groups, solver="quadprog", tol=.Machine$double.eps^0.5, maxit=50){
#   # This function returns the target mean return that maximizes mean / sd (i.e. sharpe ratio)
#   
#   # get the forecast mean from moments
#   fmean <- matrix(moments$mean, ncol=1)
#   
#   # Find the maximum return
#   max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, target=NA)
#   max_mean <- as.numeric(-max_ret$out)
#   
#   # Calculate the sr at the maximum mean return portfolio
#   ub_weights <- matrix(max_ret$weights, ncol=1)
#   ub_mean <- max_mean
#   ub_sd <- as.numeric(sqrt(t(ub_weights) %*% moments$var %*% ub_weights))
#   # sr at the upper bound
#   ub_sr <- ub_mean / ub_sd
#   
#   # Calculate the sr at the miminum var portfolio
#   tmpmoments <- moments
#   tmpmoments$mean <- rep(0, length(moments$mean))
#   lb_sr <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=NA, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
#   lb_weights <- matrix(lb_sr$weights)
#   lb_mean <- as.numeric(t(lb_weights) %*% fmean)
#   lb_sd <- as.numeric(sqrt(t(lb_weights) %*% moments$var %*% lb_weights))
#   # sr at the lower bound
#   lb_sr <- lb_mean / lb_sd
#   
#   # cat("lb_mean:", lb_mean, "\n")
#   # cat("ub_mean:", ub_mean, "\n")
#   # print("**********")
#   
#   # want to find the return that maximizes mean / sd
#   i <- 1
#   while((abs(ub_sr - lb_sr) > tol) & (i < maxit)){
#     # bisection method to find the maximum mean / sd
#     
#     # Find the starr at the mean return midpoint
#     new_ret <- (lb_mean + ub_mean) / 2
#     mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
#     mid_weights <- matrix(mid$weights, ncol=1)
#     mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#     mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
#     mid_sr <- mid_mean / mid_sd
#     # tmp_sr <- mid_sr
#     
#     # print(i)
#     # cat("new_ret:", new_ret, "\n")
#     # cat("mid_sr:", mid_sr, "\n")
#     # print("**********")
#     
#     if(mid_sr > ub_sr){
#       # if mid_sr > ub_sr then mid_sr becomes the new upper bound
#       ub_mean <- mid_mean
#       ub_sr <- mid_sr
#       new_ret <- (lb_mean + ub_mean) / 2
#       mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
#       mid_weights <- matrix(mid$weights, ncol=1)
#       mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#       mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
#       mid_sr <- mid_mean / mid_sd
#     } else if(mid_sr > lb_sr){
#       # if mid_sr > lb_sr then mid_sr becomes the new lower bound
#       lb_mean <- mid_mean
#       lb_sr <- mid_sr
#       new_ret <- (lb_mean + ub_mean) / 2
#       mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
#       mid_weights <- matrix(mid$weights, ncol=1)
#       mid_mean <- as.numeric(t(mid_weights) %*% fmean)
#       mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
#       mid_sr <- mid_mean / mid_sd
#     }
#     i <- i + 1
#   }
#   return(new_ret)
# }


###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2018 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
