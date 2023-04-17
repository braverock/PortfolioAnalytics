
#' @title Compute moments using covRobMM methods
#'
#' @description 
#' This is a function that makes use of covRobMM from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRobMM
#'
#' @return a list contains mean and covariance matrix of the data matrix
#' @export
#'
custom.covRobMM <- function(R, ...){
  out <- list()
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=1e-4
  if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=50
  
  robustCov <- RobStatTM::covRob(X=R, tolpar=tol, maxit=maxit)
  
  out$sigma <- robustCov$cov
  out$mu <- robustCov$center
  return(out)
}


#' @title Compute moments using covRobRocke methods
#'
#' @description 
#' This is a function that makes use of covRobRocke from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRobRocke
#'
#' @return a list contains mean and covariance matrix of the data matrix
#' @export
#'
custom.covRobRocke <- function(R, ...){
  out <- list()
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=1e-4
  if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=50
  if(hasArg(initial)) initial=match.call(expand.dots=TRUE)$initial else initial='K'
  if(hasArg(maxsteps)) maxsteps=match.call(expand.dots=TRUE)$maxsteps else maxsteps=5
  if(hasArg(propmin)) propmin=match.call(expand.dots=TRUE)$propmin else propmin=2
  if(hasArg(qs)) qs=match.call(expand.dots=TRUE)$qs else qs=50
  
  robustCov <- RobStatTM::covRobRocke(X=R, initial=initial, maxsteps=maxsteps, propmin=propmin, 
                                      qs=qs, tol=tol, maxit=maxit)
  
  out$sigma <- robustCov$cov
  out$mu <- robustCov$center
  return(out)
}