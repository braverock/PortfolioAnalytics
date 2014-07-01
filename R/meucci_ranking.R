
#' Asset Ranking
#' 
#' Express views on the relative expected asset returns as in A. Meucci, 
#' "Fully Flexible Views: Theory and Practice" and compute the first 
#' and second moments.
#' 
#' @note This function is based on the \code{ViewRanking} function written by
#' Ram Ahluwalia in the Meucci package.
#' 
#' @param R xts object of asset returns
#' @param p a vector of the prior probability values
#' @param order a vector of indexes of the relative ranking of expected asset 
#' returns in ascending order. For example, \code{order = c(2, 3, 1, 4)} means 
#' that the expected returns of \code{R[,2] < R[,3], < R[,1] < R[,4]}.
#' 
#' @return The estimated moments based on ranking views
#' 
#' @seealso \code{\link{meucci.moments}}
#' 
#' @references 
#' A. Meucci, "Fully Flexible Views: Theory and Practice" \url{http://www.symmys.com/node/158}
#' See Meucci script for "RankingInformation/ViewRanking.m"
#' @examples
#' data(edhec)
#' R <- edhec[,1:4]
#' p <- rep(1 / nrow(R), nrow(R))
#' meucci.ranking(R, p, c(2, 3, 1, 4))
#' @export
meucci.ranking <- function(R, p, order){
  R = coredata(R)
  
  J = nrow( R )
  N = ncol( R )
  
  k = length( order )
  
  # Equality constraints
  # constrain probabilities to sum to one across all scenarios...
  # Aeq = ones( 1 , J )
  Aeq = matrix(rep(1, J), ncol=J)
  beq = matrix(1, 1)
  
  # Inequality constraints
  # ...constrain the expectations... A*x <= 0
  # Expectation is assigned to each scenario
  V = R[ , order[1:(k-1)] ] - R[ , order[2:k] ]
  A = t( V )
  b = matrix(rep(0, nrow(A)), ncol=1)
  
  # ...compute posterior probabilities
  p_ = EntropyProg( p , A , b , Aeq , beq )$p_
  
  # compute the moments
  out <- meucci.moments(R, p_)
  
  return( out )
}
