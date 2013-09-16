
#' Function to compute diversification as a constraint
#' 
#' Diversification is defined as 1 minus the sum of the squared weights
#' \deqn{diversification = 1 - sum(w^2)}
#' 
#' @param weights vector of asset weights
#' @author Ross Bennett
#' @export
diversification <- function(weights){
   div <- 1 - sum(weights^2)
   return(div)
}
