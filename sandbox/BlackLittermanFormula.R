#' @title Computes the Black-Litterman formula for the moments of the posterior normal.
#'
#' @description This function computes the Black-Litterman formula for the moments of the posterior normal, as described in  
#' A. Meucci, "Risk and Asset Allocation", Springer, 2005.
#' 
#'   @param		Mu       [vector] (N x 1) prior expected values.
#'   @param		Sigma    [matrix] (N x N) prior covariance matrix.
#'   @param		P        [matrix] (K x N) pick matrix.
#'   @param		v        [vector] (K x 1) vector of views.
#'   @param		Omega    [matrix] (K x K) matrix of confidence.
#'
#'   @return	BLMu     [vector] (N x 1) posterior expected values.
#'   @return	BLSigma  [matrix] (N x N) posterior covariance matrix.
#'
#' @references
#' A. Meucci - "Exercises in Advanced Risk and Portfolio Management" \url{http://symmys.com/node/170}.
#'
#' See Meucci's script for "BlackLittermanFormula.m"
#'
#' @author Xavier Valls \email{flamejat@@gmail.com}
#' @export

BlackLittermanFormula = function( Mu, Sigma, P, v, Omega)
{
	BLMu    = Mu + Sigma %*% t( P ) %*% ( solve( P %*% Sigma %*% t( P ) + Omega ) %*% ( v - P %*% Mu ) );
	BLSigma =  Sigma -  Sigma %*% t( P ) %*% ( solve( P %*% Sigma %*% t( P ) + Omega ) %*% ( P %*% Sigma ) );

	return( list( BLMu = BLMu , BLSigma = BLSigma ) );

}