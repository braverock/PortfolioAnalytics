#'
#'
#'
#'
#' This is a function that makes use of covRob from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param type a string indicating which estimator to compute.
#'             Valid options are "Rocke" for Rocke’s S-estimator, 
#'             "MM" for an MM-estimator with a SHR rho function, 
#'             or "auto" (default) which selects "Rocke" if the 
#'             number of variables is greater than or equal to 10, 
#'             and "MM" otherwise.
#' @param maxit maximum number of iterations
#' @param tol tolerance for convergence
#'
#' @return estimators of moments
#' @export
robust.moments <- function(R, type="auto", maxit=50, tol=1e-4){
  
  robustCov <- RobStatTM::covRob(X=R, type=type, maxit=maxit, tol=tol)

  return(list(rbMu = robustCov$center, rbSigma = robustCov$cov))
}
