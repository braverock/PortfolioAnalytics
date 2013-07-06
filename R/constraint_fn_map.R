
#' mapping function to transform or penalize weights that violate constraints
#' 
#' The purpose of the mapping function is to transform a weights vector
#' that does not meet all the constraints into a weights vector that
#' does meet the constraints, if one exists, hopefully with a minimum
#' of transformation.
#' 
#' I think our first step should be to test each constraint type, in
#' some sort of hierarchy, starting with box constraints (almost all
#' solvers support box constraints, of course), since some of the other
#' transformations will violate the box constraints, and we'll need to
#' transform back again.
#' 
#' This function will replace constraint_fn_map
#' 
#' leverage, box, group, and position limit constraints are transformed
#' diversification and turnover constraints are penalized
#' 
#' @param weights vector of weights
#' @param portfolio object of class portfolio
#' @return 
#' \itemize{
#' \item{weights: }{vector of transformed weights meeting constraints}
#' \item{out: }{penalty term}
#' }
#' @author Ross Bennett
#' @export
fn_map <- function(weights, portfolio, ...){
  
  if(!is.portfolio(portfolio)) stop("portfolio passed in is not of class 'portfolio'")
  
  nassets <- length(portfolio$assets)
  
  # step 1: Get the constraints out of the portfolio object
  constraints <- get_constraints(portfolio)
  min_sum <- constraints$min_sum
  max_sum <- constraints$max_sum
  # rp_transform will rarely find a feasible portfolio if there is not some 
  # 'wiggle room' between min_sum and max_sum
  if((max_sum - min_sum) < 0.02){
    min_sum <- min_sum - 0.01
    max_sum <- max_sum + 0.01
  }
  min <- constraints$min
  max <- constraints$max
  groups <- constraints$groups
  cLO <- constraints$cLO
  cUP <- constraints$cUP
  div_target <- constraints$div_target
  turnover_target <- constraints$turnover_target
  max_pos <- constraints$max_pos
  tolerance <- .Machine$double.eps^0.5
  if(!hasArg(penalty)) penalty <- 1e4
  if(!hasArg(multiplier)) multiplier <- 1
  
  out <- 0
  
  tmp_weights <- weights
  
  # step 2: check that the vector of weights satisfies the constraints, 
  # transform weights if constraint is violated
  # TRUE if the weights vector is in compliance with the constraints
  # FALSE if the weights vector violates the constraint
  
  # check leverage constraints
  if(!is.null(min_sum) & !is.null(max_sum)){
    if(!(sum(tmp_weights) >= min_sum & sum(tmp_weights) <= max_sum)){
      tmp_weights <- try(rp_transform(tmp_weights, min_sum, max_sum, min, max, groups, cLO, cUP, max_pos, 500))
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Other actions to consider
        # relax constraints (rp_transform checks all constraints together so we may not know which constraint is too restrictive)
        # different normalization method
        # return initial weights and penalize?
      }
      # print("leverage constraint violated, transforming weights.")
      # print(tmp_weights)
      # tmp_weights <- txfrm_weight_sum_constraint(tmp_weights, min_sum, max_sum)
    }
  }
  
  # check box constraints
  if(!is.null(min) & !is.null(max)){
    if(!(all(tmp_weights >= min) & all(tmp_weights <= max))){
      tmp_weights <- try(rp_transform(tmp_weights, min_sum, max_sum, min, max, groups, cLO, cUP, max_pos, 500))
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Other actions to consider
        # relax constraints (rp_transform checks all constraints together so we may not know which constraint is too restrictive)
        # different normalization method
        # return initial weights and penalize?
      }
      # print("box constraints violated, transforming weights.")
      # print(tmp_weights)
      # tmp_weights <- txfrm_box_constraint(tmp_weights, min, max)
    }
  }
  
  # check group constraints
  if(!is.null(groups) & !is.null(cLO) & !is.null(cUP)){
    if(any(group_fail(tmp_weights, groups, cLO, cUP))){
      tmp_weights <- try(rp_transform(tmp_weights, min_sum, max_sum, min, max, groups, cLO, cUP, max_pos, 500))
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Other actions to consider
        # relax constraints (rp_transform checks all constraints together so we may not know which constraint is too restrictive)
        # different normalization method
        # return initial weights and penalize?
      }
      # print("group constraints violated, transforming weights.")
      # print(tmp_weights)
      # tmp_weights <- txfrm_group_constraint(tmp_weights, groups, cLO, cUP)
    }
  }
  
  # check position_limit constraints
  if(!is.null(max_pos)){
    if(!(sum(abs(tmp_weights) > tolerance) <= max_pos)){
      tmp_weights <- try(rp_transform(tmp_weights, min_sum, max_sum, min, max, groups, cLO, cUP, max_pos, 500))
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Other actions to consider
        # relax constraints (rp_transform checks all constraints together so we may not know which constraint is too restrictive)
        # different normalization method
        # return initial weights and penalize?
      }
      # print("position_limit constraint violated, transforming weights.")
      # print(tmp_weights)
      # tmp_weights <- txfrm_position_limit_constraint(tmp_weights, max_pos, nassets)
    }
  }
  
  # check diversification constraint
  if(!is.null(div_target)){
    # penalize instead of transform?
    div <- diversification(tmp_weights)
    # only penalize if not within +/- 5% of target
    if((div < div_target * .95) | (div > div_target * 1.05)){
      # print("transform or penalize to meet diversification target")
      out = out + penalty * abs(multiplier) * abs(div - div_target)
    }
  }
  
  # check turnover constraint
  if(!is.null(turnover_target)){
    # penalize instead of transform
    to <- turnover(tmp_weights)
    # only penalize if not within +/- 5% of target
    if((to < turnover_target * 0.95) | (to > turnover_target * 1.05)){
      # print("transform or penalize to meet turnover target")
      out = out + penalty * abs(multiplier) * abs(to - turnover_target)
    }
  }
  names(tmp_weights) <- names(weights)
  return(list(weights=tmp_weights, out=out))
}

#' Transform weights that violate min or max box constraints
#' 
#' This is a helper function called inside constraint_fnMap to transform the weights vector to satisfy box constraints.
#' 
#' @param weights vector of weights
#' @param min vector of minimum asset weights from box constraints
#' @param max vector of maximum asset weights from box constraints
#' @author Ross Bennett
#' @export
txfrm_box_constraint <- function(weights, min, max) {
  # 1. Check if any elements of the weights vector violate min or max
  # 2. If min or max is violated, then set those weights equal to their respective min or max values
  # The length of the weights vector must be equal to the length of min and max vectors so that an element-by-element comparison is done
  if(any(weights < min) | any(weights > max)){
    # get the index of elements in the weights vector that violate min
    idx.min <- which(weights < min)
    # set those elements in the weights vector equal to their respective min
    weights[idx.min] = min[idx.min]
    # print(weights)
    # get the index of elements in the weights vector that violate max
    idx.max <- which(weights > max)
    # set those elements in the weights vector equal to their respective max
    weights[idx.max] = max[idx.max]
    # print(weights)
    # The transformation will likely change the sum of weights and violate min_sum or max_sum
    # Should we normalize here by transforming the entire weights vector?
    # Normalizing by transforming the entire weights may violate min and max, but will get us *close*
    # if(sum(weights) < min_sum) weights <- min_sum / sum(weights) * weights
    # if(sum(weights) > max_sum) weights <- max_sum / sum(weights) * weights
  }
  return(weights)
}

#' Transform weights that violate group constraints
#' 
#' This is a helper function called inside constraint_fnMap to transform the weights vector to satisfy group constraints.
#' 
#' @param weights vector of weights
#' @param groups vector of groups
#' @param cLO vector of minimum group weights from group constraints
#' @param cUP vector of maximum group weights from group constraints
#' @author Ross Bennett
#' @export
txfrm_group_constraint <- function(weights, groups, cLO, cUP){
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
    k <- k + j
    l <- k - 1
  }
  # Normalizing the weights inside the groups changes the sum of the weights.
  # Should normalizing the sum of weights take place here or somewhere else?
  # Re-normalizing the weights will get us *close* to satisfying the group constraints.
  # Maybe then add a penalty in constrained objective for violation of group constraints?
  return(weights)
}

#' Transform weights that violate weight_sum constraints
#' 
#' This is a helper function called inside constraint_fnMap to transform the weights vector to satisfy weight_sum constraints.
#' 
#' @param weights vector of weights
#' @param min_sum minimum sum of asset weights
#' @param max_sum maximum sum of asset weights
#' @author Ross Bennett
#' @export
txfrm_weight_sum_constraint <- function(weights, min_sum, max_sum){
  # normalize to max_sum
  if(sum(weights) > max_sum) { weights <- (max_sum / sum(weights)) * weights }
  # normalize to min_sum
  if(sum(weights) < min_sum) { weights <- (min_sum / sum(weights)) * weights }
  return(weights)
}

#' Transform weights for position_limit constraints
#' 
#' This is a helper function called inside constraint_fnMap to transform the weights vector to satisfy position_limit constraints.
#' This function sets the minimum nassets-max_pos assets equal to 0 such that the max_pos number of assets will have non-zero weights.
#' 
#' @param weights vector of weights
#' @param max_pos maximum position of assets with non_zero weights
#' @param nassets number of assets
#' @author Ross Bennett
#' @export
txfrm_position_limit_constraint <- function(weights, max_pos, nassets, tolerance=.Machine$double.eps^0.5){
  # account for weights that are very small (less than .Machine$double.eps^0.5) and are basically zero
  # check if max_pos is violated
  if(sum(abs(weights) > tolerance) > max_pos){
    # set the minimum nassets-max_pos weights equal to 0
    weights[head(order(weights), nassets - max_pos)] <- 0
  }
  return(weights)
}

#' Transform a weights vector to satisfy leverage, box, group, and position_limit constraints using logic from \code{randomize_portfolio}
#' 
#' This function uses a block of code from \code{\link{randomize_portfolio}} 
#' to transform the weight vector if either the weight_sum (leverage) 
#' constraints, box constraints, group constraints, or position_limit constraints are violated.
#' The resulting weights vector might be quite different from the original weights vector.
#' 
#' @param w weights vector to be transformed
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying maximum weight box constraints
#' @param groups vector specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @param max_pos maximum assets with non-zero weights
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @return named weighting vector
#' @author Peter Carl, Brian G. Peterson, Ross Bennett (based on an idea by Pat Burns)
#' @export
rp_transform <- function(w, min_sum=0.99, max_sum=1.01, min, max, groups, cLO, cUP, max_pos=NULL, max_permutations=200){
  # Uses logic from randomize_portfolio to "normalize" a weights vector to 
  # satisfy min_sum and max_sum while accounting for box and group constraints
  # Modified from randomize_portfolio to trigger the while loops if any weights 
  # violate min or max box constraints. A weights vector would not be transformed
  # in randomize_portfolio if min_sum and max_sum were satisfied, but the
  # min/max constraints were violated.
  
  tolerance=.Machine$double.eps^0.5
  if(is.null(max_pos)) max_pos <- length(w)
  
  # Create a temporary min vector that will be modified, because a feasible
  # portfolio is rarely created if all(min > 0). This is due to the while
  # loop that checks any(tmp_w < min).
  tmp_min <- min
  
  # If weight_i = 0 and min_i > 0, then this will violate box constraints
  # even though weight_i = 0 to satisfy position_limit constraints. Modify
  # the tmp_min vector and set tmp_min_i equal to zero where weights_i = 0.
  # If w is less than or equal to tolerance then it is essentially 0
  if(any(abs(w) <= tolerance)){
    if(any(tmp_min[which(abs(w) <= tolerance)] > 0)){
      tmp_min[which(abs(w) <= tolerance)] <- -tolerance
    }
  }
  
  # return w if all constraints are satisfied
  if((sum(w) >= min_sum & sum(w) <= max_sum) & 
       (all(w >= tmp_min) & all(w <= max)) & 
       (all(!group_fail(weights, groups, cLO, cUP))) &
       (sum(abs(w) > tolerance) <= max_pos)){
    return(w)
  }
  
  # generate a sequence of weights based on min/max box constraints
  weight_seq <- generatesequence(min=min(min), max=max(max), by=0.005)
  # make sure there is a 0 in weight_seq
  if(!is.null(max_pos) & !is.element(0, weight_seq)) weight_seq <- c(0, weight_seq)
  
  # start the permutations counter
  permutations <- 1
  
  # create a temporary weights vector that will be modified in the while loops
  tmp_w <- w
  
  # while portfolio is outside min_sum/max_sum or tmp_min/max or group or postion_limit constraints and we have not reached max_permutations
  while ((sum(tmp_w) <= min_sum | sum(tmp_w) >= max_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP)) | (sum(abs(tmp_w) > tolerance) > max_pos)) & permutations <= max_permutations) {
    permutations = permutations + 1
    # check our box constraints on total portfolio weight
    # reduce(increase) total portfolio size till you get a match
    # 1> check to see which bound you've failed on, probably set this as a pair of while loops
    # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
    # 3> check and repeat
    
    # reset tmp_w and tmp_min to their original values
    tmp_w <- w
    tmp_min <- min
    
    random_index <- sample(1:length(tmp_w), max_pos)
    
    # Get the index values that are not in random_index and set them equal to 0
    full_index <- 1:length(tmp_w)
    not_index <- setdiff(full_index, random_index)
    tmp_w[not_index] <- 0
    
    # set some tmp_min values equal to zero so the while loops do not see a
    # violation of any(tmp_w < tmp_min)
    tmp_min[not_index] <- 0
    
    i = 1
    # while sum of weights is less than min_sum or tmp_min/max box or group constraint is violated
    while ((sum(tmp_w) <= min_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP))) & i <= length(tmp_w)) {
      # randomly permute and increase a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      if (length(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]) > 1) {
        # randomly sample an element from weight_seq that is greater than cur_val and less than max
        tmp_w[cur_index] <- sample(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])], 1)
        # print(paste("new val:",tmp_w[cur_index]))
      } else {
        if (length(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]) == 1) {
          tmp_w[cur_index] <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
        }
      }
      i=i+1 # increment our counter
    } # end increase loop
    # need to reset i here otherwise the decreasing loop will be ignored
    # group_fail does not test for direction of violation, just that group constraints were violated
    i = 1 
    # while sum of weights is greater than max_sum or tmp_min/max box or group constraint is violated
    while ((sum(tmp_w) >= max_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP))) & i <= length(tmp_w)) {
      # randomly permute and decrease a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      if (length(weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])] ) > 1) {
        # randomly sample an element from weight_seq that is less than cur_val and greater than tmp_min
        tmp_w[cur_index] <- sample(weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])] , 1)
      } else {
        if (length(weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])] ) == 1) {
          tmp_w[cur_index] <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])]
        }
      }
      i=i+1 # increment our counter
    } # end decrease loop
  } # end final walk towards the edges
  
  portfolio <- tmp_w
  
  colnames(portfolio)<-colnames(w)
  
  # checks for infeasible portfolio
  # Stop execution and return an error if an infeasible portfolio is created
  # This will be useful in fn_map so that we can catch the error and take
  # action (try again with more permutations, relax constraints, different
  # method to normalize, etc.)
  if (sum(portfolio)<=min_sum | sum(portfolio)>=max_sum){
    portfolio <- w
    stop("Infeasible portfolio created, perhaps increase max_permutations and/or adjust your parameters.")
  }
#   if(isTRUE(all.equal(w,portfolio))) {
#     if (sum(w)>=min_sum & sum(w)<=max_sum) {
#       warning("Unable to generate a feasible portfolio different from w, perhaps adjust your parameters.")
#       return(w)
#     } else {
#       warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
#       return(NULL)
#     }
#   }
  return(portfolio)
}

#' Test if group constraints have been violated
#' 
#' The function loops through each group and tests if cLO or cUP have been violated
#' for the given group. This is a helper function for \code{\link{rp_transform}}.
#' 
#' @param weights weights vector to test
#' @param groups vector specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @return logical vector: TRUE if group constraints are violated for a given group
#' @author Ross Bennett
#' @export
group_fail <- function(weights, groups, cLO, cUP){
  # return FALSE if groups, cLO, or cUP is NULL
  if(is.null(groups) | is.null(cLO) | is.null(cUP)) return(FALSE)
  
  n.groups <- length(groups)
  group_fail <- vector(mode="logical", length=n.groups)
  k <- 1
  l <- 0
  for(i in 1:n.groups){
    j <- groups[i]
    tmp.w <- weights[k:(l+j)]
    grp.min <- cLO[i]
    grp.max <- cUP[i]
    # return TRUE if grp.min or grp.max is violated
    group_fail[i] <- ( sum(tmp.w) < grp.min | sum(tmp.w) > grp.max )
    k <- k + j
    l <- k - 1
  }
  # returns logical vector of groups. TRUE if either cLO or cUP is violated
  return(group_fail)
}

# test
# w <- c(0.1, 0.25, 0.3, 0.15, 0.05, 0.15)
# min <- rep(0.1, length(w))
# max <- rep(0.45, length(w))
# w1 <- rp_normalize(w=w, min_sum=0.99, max_sum=1.01, min=min, max=max)
# w1
# sum(w1)
# any(w1 < min)
# any(w1 > max)

# library(PortfolioAnalytics)
# data(edhec)
# ret <- edhec[, 1:4]
# funds <- colnames(ret)
# 
# pspec <- portfolio.spec(assets=funds)
# pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01, enabled=TRUE)
# pspec <- add.constraint(portfolio=pspec, type="box", enabled=TRUE)
# pspec <- add.constraint(portfolio=pspec, type="group", groups=c(2,2), group_min=c(0.1, 0.2), group_max=c(0.3, 0.8), enabled=TRUE)
# 
# weights <- c(0.15, 0.2, 0.15, 0.5)
# sum(weights)
# 
# (w <- constraint_fn_map(weights, pspec))
# sum(w)
