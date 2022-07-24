#' Title
#'
#' @param x 
#' @param filter 
#' @param partial.impute 
#' @param tol 
#' @param maxiter 
#' @param method 
#' @param init 
#' @param mu0 
#' @param S0 
#'
#' @return
#' @export
#'
#' @examples
tsgs.moments <- function(R, filter="UBF-DDC", 
                         partial.impute=FALSE, tol=1e-4, maxiter=150, 
                         method="bisquare",
                         init="emve_c", mu0=NULL, S0=NULL){
  tsgsRob <- GSE::TSGS(x=R, filter=filter,
                       partial.impute=partial.impute, tol=tol, maxiter=maxiter, method=method,
                       init=init, mu0=NULL, S0=NULL)
  return(list(mu=tsgsRob@mu, sig=tsgsRob@S))
  
}