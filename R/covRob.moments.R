
#' @title Compute moments using covRob methods
#'
#' @description 
#' This is a function that makes use of covRob from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRob
#'
#' @return a list contains mean and covariance matrix of the data matrix
#' @export
#'
#' @examples
custom.covRob <- function(R, ...){
  out <- list()
  if(hasArg(type)) type=match.call(expand.dots=TRUE)$type else type="auto"
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=1e-4
  if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=50
  
  robustCov <- RobStatTM::covRob(X=R, type=type, tol=tol, maxit=maxit)
  
  out$sigma <- robustCov$cov
  out$mu <- robustCov$center
  return(out)
}