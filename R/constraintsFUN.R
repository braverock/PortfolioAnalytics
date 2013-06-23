#' Generic function to impose group constraints on a vector of weights
#' 
#' This function gets group subsets of the weights vector and checks if the sum
#' of the weights in that group violates the minimum or maximum value. If the 
#' sum of weights in a given group violates its maximum or minimum value, the 
#' group of weights is normalized to be equal to the minimum or maximum value.
#' This group normalization causes the sum of weights to change. The weights
#' vector is then normalized so that the min_sum and max_sum constraints are 
#' satisfied. This "re-normalization" of the weights vector may then cause the
#' group constraints to not be satisfied.
#' 
#' Group constraints are implemented in ROI solvers, but this function could
#' be used in constrained_objective for random portfolios, DEoptim, pso, or 
#' gensa solvers.
#' 
#' @param groups vector to group assets
#' @param cLO vector of group weight minimums
#' @param cUP vector of group weight maximums
#' @param weights vector of weights
#' @param min_sum minimum sum of weights
#' @param max_sum maximum sum of weights
#' @param normalize TRUE/FALSE to normalize the weights vector to satisfy the min_sum and max_sum constraints
#' 
#' @author Ross Bennett
#' @export
constrained_group_tmp <- function(groups, cLO, cUP, weights, min_sum, max_sum, normalize=TRUE){
  # Modify the args later to accept a portfolio or constraint object
  n.groups <- length(groups)
  
  k <- 1
  l <- 0
  for(i in 1:n.groups){
    j <- groups[i]
    tmp.w <- weights[k:(l+j)]
    # normalize weights for a given group that sum to less than specified group min
    grp.min <- cLO[i]
    if(sum(tmp.w) < grp.min) {
      weights[k:(l+j)] <- (grp.min / sum(tmp.w)) * tmp.w
    }
    # normalize weights for a given group that sum to greater than specified group max
    grp.max <- cUP[i]
    if(sum(tmp.w) > grp.max) {
      weights[k:(l+j)] <- (grp.max / sum(tmp.w)) * tmp.w
    }
    # cat(sum(tmp.w), "\t", cLO[i], "\n")
    # cat(k, " ", l+j, "\n")
    k <- k + j
    l <- k - 1
  }
  # Normalizing the weights inside the groups changes the sum of the weights.
  # Should normalizing the sum of weights take place here or somewhere else?
  
  if(normalize){
    # max_sum and min_sum normalization borrowed from constrained_objective
    # Normalize to max_sum
    if(sum(weights) > max_sum) { weights <- (max_sum / sum(weights)) * weights }
    # Normalize to min_sum
    if(sum(weights) < min_sum) { weights <- (min_sum / sum(weights)) * weights }
  }
  # "Re-normalizing" the weights causes some of the group constraints to
  # be violated. Can this be addressed later with a penalty term for violating
  # the group constraints? Or another way?
  return(weights)
}
