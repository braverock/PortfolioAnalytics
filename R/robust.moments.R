
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
robust.moments <- function(R, type=, maxit, tol){
  
  robustCov <- RobStatTM::covRob(R, type=type, maxit=maxit, tol=tol)

  return(list(rbMu = robustCov$center, rbSigma = robustCov$cov))
}
