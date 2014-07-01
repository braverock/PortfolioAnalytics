
#' Asset Ranking
#' 
#' Compute the first moment from a single complete sort
#' 
#' This function computes the estimated centroid vector from a single complete
#' sort using the analytical approximation as described in R. Almgren and 
#' N. Chriss, "Portfolios from Sorts". The centroid is estimated and then 
#' scaled such that it is on a scale similar to the asset returns. By default,
#' the centroid vector is scaled according to the median of the asset mean 
#' returns.
#' 
#' @param R xts object of asset returns
#' @param order a vector of indexes of the relative ranking of expected asset 
#' returns in ascending order. For example, \code{order = c(2, 3, 1, 4)} means 
#' that the expected returns of \code{R[,2] < R[,3], < R[,1] < R[,4]}.
#' @param \dots any other passthrough parameters
#' 
#' @return The estimated first moments based on ranking views
#'  
#' @references 
#' R. Almgren and N. Chriss, "Portfolios from Sorts" 
#' \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=720041}
#' 
#' @examples
#' data(edhec)
#' R <- edhec[,1:4]
#' ac.ranking(R, c(2, 3, 1, 4))
#' @export
ac.ranking  <- function(R, order, ...){
  if(length(order) != ncol(R)) stop("The length of the order vector must equal the number of assets")
  nassets <- ncol(R)
  if(hasArg(max.value)) {
    max.value <- match.call(expand.dots=TRUE)$max.value
  } else {
    max.value <- median(colMeans(R))
  }
  # Compute the scaled centroid
  c_hat <- scale.range(centroid(nassets), max.value)
  
  # Here we reorder the vector such that the highest centroid value is assigned
  # to the asset index with the highest expected return and so on and so forth
  # until the smallest centroid value is assigned to the asset index with the 
  # lowest expected return. The asset index with the lowest expected return
  # is order[1]
  out <- vector("numeric", nassets)
  out[rev(order)] <- c_hat
  return(out)
}

# compute the centroid for a single complete sort
centroid <- function(n){
  # Analytical solution to the centroid for single complete sort
  # http://papers.ssrn.com/sol3/papers.cfm?abstract_id=720041
  A <- 0.4424
  B <- 0.1185
  beta <- 0.21
  alpha <- A - B * n^(-beta)
  j <- seq(from=1, to=n, by=1)
  c_hat <- qnorm((n + 1 - j - alpha) / (n - 2 * alpha + 1))
  c_hat
}

# If we used the unscaled centroid vector in the optimization, the optimal
# portfolio would be correct but anything that uses moments$mu will not make
# sense

# What is a valid value for max.value?
# - by default we use the median of the asset mean returns
scale.range <- function(x, max.value){
  new.max <- 0.05
  new.min <- -new.max
  old.range <- max(x) - min(x)
  new.range <- new.max - new.min
  ((x - min(x)) * new.range) / old.range + new.min
}
