
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
#' @seealso \code{\link{centroid.complete.mc}} \code{\link{centroid.sectors}}
#' \code{\link{centroid.sign}} \code{\link{centroid.buckets}}
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

# Numerically compute the centroid for different cases as described in
# the Almgren and Chriss paper.

#' Complete Cases Centroid
#' 
#' Numerical method to estimate complete cases centroid
#' @param order a vector of indexes of the relative ranking of expected asset 
#' returns in ascending order. For example, \code{order = c(2, 3, 1, 4)} 
#' expresses a view on the expected returns such that 
#' R_2 < R_3 < R_1 < R_4
#' @param simulations number of simulations
#' @return the centroid vector
#' @examples
#' # Express a view on the assets such that
#' # R_2 < R_1 < R_3 < R_4
#' centroid.complete.mc(c(2, 1, 3, 4))
#' @author Ross Bennett
#' @export
centroid.complete.mc <- function(order, simulations=1000){
  n <- length(order)
  c_hat <- matrix(0, simulations, n)
  for(i in 1:simulations){
    c_hat[i,] <- sort(rnorm(n), decreasing=TRUE)
  }
  out <- vector("numeric", n)
  out[rev(order)] <- colMeans(c_hat)
  return(out)
}

#' Multiple Sectors Centroid
#' 
#' Compute the centroid for expressing views on the relative ranking of assets
#' within sectors.
#' 
#' @param sectors a list where each list element contains the order of each 
#' asset in the given sector
#' @param simulations number of simulations
#' @return the centroid vector
#' @examples
#' # Express a view on the assets in two sectors
#' # Sector 1 View: R_2 < R_1 < R_3
#' # Sector 2 View: R_5 < R_4
#' x <- list()
#' x[[1]] <- c(2, 1, 3)
#' x[[2]] <- c(5, 4)
#' centroid.sectors(x)
#' @author Ross Bennett
#' @export
centroid.sectors <- function(sectors, simulations=1000){
  if(!is.list(sectors)) stop("sectors must be a list")
  
  # Get the number of assets and number of sectors
  nassets <- length(unlist(sectors))
  nsectors <- length(sectors)
  
  # Compute the centroid for each sector and combine at the end
  sim.list <- vector("list", nsectors)
  for(j in 1:nsectors){
    # number of assets in sector j
    n <- length(sectors[[j]])
    out <- matrix(0, simulations, n)
    for(i in 1:simulations){
      out[i,] <- sort(rnorm(n), decreasing=TRUE)
    }
    sim.list[[j]] <- out
  }
  c_hat <- lapply(sim.list, colMeans)
  out <- vector("numeric", nassets)
  for(i in 1:length(c_hat)){
    out[rev(sectors[[i]])] <- c_hat[[i]]
  }
  return(out)
}

#' Positive and Negative View Centroid
#' 
#' Compute the centroid for expressing a view on assets with positive or 
#' negative expected returns
#' 
#' @param positive a vector of the index of assets with positive expected 
#' return in ascending order
#' @param negative a vector of the index of assets with negative expected 
#' return in ascending order.
#' @param simulations number of simulations
#' @return the centroid vector
#' @examples
#' # Express a view that 
#' # R_1 < R_2 < 0 < R_3 < R_4
#' centroid.sign(c(1, 2), c(4, 3))
#' @author Ross Bennett
#' @export
centroid.sign <- function(positive, negative, simulations=1000){
  
  # Number of positive and negative assets
  pos <- length(positive)
  neg <- length(negative)
  nassets <- pos + neg
  
  c_hat <- matrix(0, simulations, nassets)
  for(i in 1:simulations){
    tmp <- rnorm(nassets)
    # subset the positive and negative assets
    tmp.pos <- tmp[1:pos]
    tmp.neg <- tmp[(pos+1):(pos+neg)]
    
    # Sign correct the positive assets
    idx <- which(tmp.pos < 0)
    if(length(idx) != 0){
      tmp.pos[idx] <- -1 * tmp.pos[idx]
    }
    
    # Sign correct the negative assets
    idx <- which(tmp.neg > 0)
    if(length(idx) != 0){
      tmp.neg[idx] <- -1 * tmp.neg[idx]
    }
    c_hat[i,] <- sort(c(tmp.pos, tmp.neg), decreasing=TRUE)
  }
  xx <- colMeans(c_hat)
  out <- vector("numeric", nassets)
  out[rev(positive)] <- xx[1:pos]
  out[rev(negative)] <- xx[(1+pos):(pos+neg)]
  return(out)
}

#' Buckets Centroid
#' 
#' Compute the centroid for buckets of assets
#' 
#' A common use of buckets is to divide the assets into quartiles or deciles,
#' but is generalized here for an arbitrary number of buckets and arbitrary
#' number of assets in each bucket.
#' 
#' @param buckets a list where each element contains the index of the assets in 
#' the respective bucket. The assets within each bucket have no order. 
#' The bucket elements are in ascending order such that 
#' R_bucket_1 < ... < R_bucket_n
#' @param simulations number of simulations
#' @return the centroid vector
#' @author Ross Bennett
#' @export
centroid.buckets <- function(buckets, simulations=1000){
  if(!is.list(buckets)) stop("buckets must be a list")
  
  # number of assets and buckets
  nassets <- length(unlist(buckets))
  nbuckets <- length(buckets)
  
  # Run simulations so we simulate n values for n buckets and then replicate
  # that value for the number of assets in the given bucket
  c_hat <- matrix(0, simulations, nbuckets)
  for(i in 1:simulations){
    c_hat[i,] <- sort(rnorm(nbuckets), decreasing=TRUE)
  }
  xx <- colMeans(c_hat)
  out <- vector("numeric", nassets)
  for(j in 1:nbuckets){
    out[buckets[[j]]] <- xx[j]
  }
  return(out)
}
