

#' Compute moments 
#' 
#' Compute the first and second moments using the Fully Flexible Views 
#' framework as described in A. Meucci - "Fully Flexible Views: Theory and Practice".
#' 
#' @param R xts object of asset returns
#' @param posterior_p vector of posterior probabilities
#' @return a list with the first and second moments
#' \itemize{
#'   \item{\code{mu}: }{vector of expected returns}
#'   \item{\code{sigma}: }{covariance matrix}
#' }
#' @references 
#' A. Meucci - "Fully Flexible Views: Theory and Practice".
#' @author Ross Bennett
#' @export
meucci.moments <- function(R, posterior_p){
  R = coredata(R)
  # expected return vector
  mu = t(R) %*% posterior_p
  
  # covariance matrix
  Scnd_Mom = t(R) %*% (R * (posterior_p %*% matrix( 1, 1, ncol(R))))
  Scnd_Mom = ( Scnd_Mom + t(Scnd_Mom) ) / 2
  sigma = Scnd_Mom - mu %*% t(mu)
  list(mu=mu, sigma=sigma)
}
