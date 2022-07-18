
#' Title
#'
#' @param R 
#' @param type 
#' @param maxit 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
robust.moments <- function(R, type="type", maxit=100, tol=1e-4){
  
  robustCov <- RobStatTM::covRob(X=R, type=type, maxit=maxit, tol=tol)

  return(list(rbMu = robustCov$center, rbSigma = robustCov$cov))
}
