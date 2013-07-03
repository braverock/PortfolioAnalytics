#' Constraint mapping function
#' 
#' The purpose of the mapping function is to transform a weights vector
#' that does not meet all the constraints into a weights vector that
#' does meet the constraints, if one exists, hopefully with a minimum
#' of transformation.

#' I think our first step should be to test each constraint type, in
#' some sort of hierarchy, starting with box constraints (almost all
#' solvers support box constraints, of course), since some of the other
#' transformations will violate the box constraints, and we'll need to
#' transform back again.
#' 
#' @param weights vector of weights
#' @param portfolio object of class portfolio
#' @author Ross Bennett
#' @export
constraint_fn_map <- function(weights, portfolio) {
  
  if (!is.portfolio(portfolio)) {
    stop("Portfolio passed in is not of class portfolio")
  }
  
  # number of assets
  nassets <- length(portfolio$assets)
  
  # This is in a loop so the order of transformation depends on how the constraints are added by the user.
  # Maybe take this out of a loop because the order of transformation is important
  for(constraint in portfolio$constraints) {
    # Check for enabled constraints
    if(constraint$enabled){
      
      ## box constraint
      if(inherits(constraint, "box_constraint")){
        min <- constraint$min
        max <- constraint$max
        
        w <- txfrm_box_constraint(weights=weights, min=min, max=max)
        
        # The transformation will likely change the sum of weights and violate min_sum or max_sum
        # Should we normalize here by transforming the entire weights vector?
        # Normalizing by transforming the entire weights may violate min and max, but will get us *close*
      } # end box_constraint transformation
      
      ## weight_sum constraint
      if(inherits(constraint, "weight_sum_constraint")){
        min_sum <- constraint$min_sum
        max_sum <- constraint$max_sum
        # print(min_sum)
        # print(max_sum)
        
        w <- txfrm_weight_sum_constraint(weights=weights, min_sum=min_sum, max_sum=max_sum)
        
      } # end weight_sum constraint transformation
      
      ## group constraint
      if(inherits(constraint, "group_constraint")){
        groups <- constraint$groups
        cLO <- constraint$cLO
        cUP <- constraint$cUP
        # print(groups)
        # print(cLO)
        # print(cUP)
        
        w <- txfrm_group_constraint(weights=weights, groups=groups, cLO=cLO, cUP=cUP)
        
        # Normalizing the weights inside the groups changes the sum of the weights.
        # Should normalizing the sum of weights take place here or somewhere else?
        # Re-normalizing the weights will get us *close* to satisfying the group constraints.
        # Maybe then add a penalty in constrained objective for violation of group constraints?
      } # end group_constraint transformation
      
      # Turnover constraints
      # TODO
      
      # Diversification constraints
      # TODO
      
      ## position_limit constraint
      if(inherits(constraint, "group_constraint")){
        max_pos <- constraint$max_pos
        
        w <- txfrm_position_limit_constraint(weights=weights, max_pos=max_pos, nassets=nassets)
        
      } # end position_limit_constraint transformation
      
    }
  }
  return(w)
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

#' Transform a weights vector to min_sum/max_sum leverage and min/max box constraints using logic from randomize_portfolio
#' 
#' This function uses a block of code from \link{\code{randomize_portfolio}} 
#' to transform the weight vector if either the weight_sum (leverage) 
#' constraints or box constraints are violated.
#' The resulting weights vector might be quite different from the original weights vector.
#' 
#' @param w weights vector to be transformed
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying maximum weight box constraints
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @return named weighting vector
#' @author Peter Carl, Brian G. Peterson, Ross Bennett (based on an idea by Pat Burns)
#' @export
rp_transform <- function(w, min_sum=0.99, max_sum=1.01, min, max, max_permutations=200){
  # Uses logic from randomize_portfolio to "normalize" a weights vector to 
  # satisfy min_sum and max_sum while account for min and max box constraints
  # Modified from randomize_portfolio to trigger the while loops if any weights 
  # violate min or max box constraints. A weights vector would not be transformed
  # in randomize_portfolio if min_sum and max_sum were satisfied, but the
  # min/max constraints were violated.
  
  # generate a sequence of weights based on min/max box constraints
  weight_seq <- generatesequence(min=min(min), max=max(max), by=0.005)
  
  # start the permutations counter
  permutations <- 1
  
  # create a temporary weights vector that will be modified in the while loops
  tmp_w <- w
  
  # while portfolio is outside min_sum/max_sum or min/max and we have not reached max_permutations
  while ((sum(tmp_w) <= min_sum | sum(tmp_w) >= max_sum | any(tmp_w < min) | any(tmp_w > max)) & permutations <= max_permutations) {
    permutations = permutations + 1
    # check our box constraints on total portfolio weight
    # reduce(increase) total portfolio size till you get a match
    # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
    # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
    # 3> check and repeat
    
    random_index <- sample(1:length(tmp_w), length(tmp_w))
    i = 1
    # while sum of weights is less than min_sum or min/max box constraint is violated
    while ((sum(tmp_w) <= min_sum | any(tmp_w < min) | any(tmp_w > max)) & i <= length(tmp_w)) {
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
    # while sum of weights is greater than max_sum or min/max box constraint is violated
    while ((sum(tmp_w) >= max_sum | any(tmp_w < min) | any(tmp_w > max)) & i <= length(tmp_w)) {
      # randomly permute and decrease a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      if (length(weight_seq <= cur_val & weight_seq >= min[cur_index] ) > 1) {
        # randomly sample an element from weight_seq that is less than cur_val and greater than min
        tmp_w[cur_index] <- sample(weight_seq[which(weight_seq <= cur_val & weight_seq >= min[cur_index] )], 1)
      } else {
        if (length(weight_seq <= cur_val & weight_seq >= min[cur_index] ) == 1) {
          tmp_w[cur_index] <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index])]
        }
      }
      i=i+1 # increment our counter
    } # end decrease loop
  } # end final walk towards the edges
  
  portfolio <- tmp_w
  
  colnames(portfolio)<-colnames(w)
  
  # checks for infeasible portfolio
  if (sum(portfolio)<=min_sum | sum(portfolio)>=max_sum){
    portfolio <- w
    warning("Infeasible portfolio created, defaulting to w, perhaps increase max_permutations.")
  }
  if(isTRUE(all.equal(w,portfolio))) {
    if (sum(w)>=min_sum & sum(w)<=max_sum) {
      warning("Unable to generate a feasible portfolio different from w, perhaps adjust your parameters.")
      return(w)
    } else {
      warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
      return(NULL)
    }
  }
  return(portfolio)
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
