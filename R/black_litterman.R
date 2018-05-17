
#' @title Computes the Black-Litterman formula for the moments of the posterior normal.
#'
#' @description This function computes the Black-Litterman formula for the moments of the posterior normal, as described in  
#' A. Meucci, "Risk and Asset Allocation", Springer, 2005.
#' 
#' @param		Mu       [vector] (N x 1) prior expected values.
#' @param		Sigma    [matrix] (N x N) prior covariance matrix.
#' @param		P        [matrix] (K x N) pick matrix.
#' @param		v        [vector] (K x 1) vector of views.
#' @param		Omega    [matrix] (K x K) matrix of confidence.
#'
#' @return	BLMu     [vector] (N x 1) posterior expected values.
#' @return	BLSigma  [matrix] (N x N) posterior covariance matrix.
#'
#' @references
#' A. Meucci - "Exercises in Advanced Risk and Portfolio Management" \url{http://symmys.com/node/170}.
#'
#' See Meucci's script for "BlackLittermanFormula.m"
#'
#' @author Xavier Valls \email{flamejat@@gmail.com}
BlackLittermanFormula = function( Mu, Sigma, P, v, Omega)
{
	BLMu    = Mu + Sigma %*% t( P ) %*% ( solve( P %*% Sigma %*% t( P ) + Omega ) %*% ( v - P %*% Mu ) );
	BLSigma =  Sigma -  Sigma %*% t( P ) %*% ( solve( P %*% Sigma %*% t( P ) + Omega ) %*% ( P %*% Sigma ) );

	return( list( BLMu = BLMu , BLSigma = BLSigma ) );

}

#' Black Litterman Estimates
#' 
#' Compute the Black Litterman estimate of moments for the posterior normal.
#' 
#' @note This function is largely based on the work of Xavier Valls to port
#' the matlab code of Attilio Meucci to \R as documented in the Meucci package.
#' 
#' @param R returns
#' @param P a K x N pick matrix
#' @param Mu vector of length N of the prior expected values. The sample mean
#' is used if \code{Mu=NULL}.
#' @param Sigma an N x N matrix of the prior covariance matrix. The sample 
#' covariance is used if \code{Sigma=NULL}.
#' @param Views a vector of length K of the views
#' @return \itemize{
#'   \item{BLMu:}{ posterior expected values}
#'   \item{BLSigma:}{ posterior covariance matrix}
#' }
#' @author Ross Bennett, Xavier Valls
#' @references
#' A. Meucci - "Exercises in Advanced Risk and Portfolio Management" \url{http://symmys.com/node/170}.
#' @seealso \code{\link{BlackLittermanFormula}}
#' @export
black.litterman <- function(R, P, Mu=NULL, Sigma=NULL, Views=NULL){
  
  # Compute the sample estimate if mu is null
  if(is.null(Mu)){
    Mu <- colMeans(R) 
  }
  if(length(Mu) != NCOL(R)) stop("length of Mu must equal number of columns of R")
  
  # Compute the sample estimate if sigma is null
  if(is.null(Sigma)){
    Sigma <- cov(R)
  }
  if(!all(dim(Sigma) == NCOL(R))) stop("dimensions of Sigma must equal number of columns of R")
  
  # Compute the Omega matrix and views value
  Omega = tcrossprod(P %*% Sigma, P)
  if(is.null(Views)) Views = as.numeric(sqrt( diag( Omega ) ))
  B = BlackLittermanFormula( Mu, Sigma, P, Views, Omega )
  return(B)
}

