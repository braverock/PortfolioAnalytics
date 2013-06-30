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
