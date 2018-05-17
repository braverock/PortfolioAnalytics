
#' mapping function to transform or penalize weights that violate constraints
#' 
#' The purpose of the mapping function is to transform a weights vector
#' that does not meet all the constraints into a weights vector that
#' does meet the constraints, if one exists, hopefully with a minimum
#' of transformation.
#' 
#' The first step is to test for violation of the constraint. If the constraint
#' is violated, we will apply a transformation such that the weights vector
#' satisfies the constraints. The following constraint types are tested in
#' the mapping function: leverage, box, group, and position limit. The 
#' transformation logic is based on code from the random portfolio sample method.
#' 
#' If relax=TRUE, we will attempt to relax the constraints if a feasible 
#' portfolio could not be formed with an initial call to \code{rp_transform}. 
#' We will attempt to relax the constraints up to 5 times. If we do not have a 
#' feasible portfolio after attempting to relax the constraints, then we will 
#' default to returning the weights vector that violates the constraints.
#' 
#' @param weights vector of weights
#' @param portfolio object of class \code{portfolio}
#' @param relax TRUE/FALSE, default FALSE. Enable constraints to be relaxed.
#' @param verbose print error messages for debuggin purposes
#' @param \dots any other passthru parameters
#' @return 
#' \itemize{
#' \item{weights: }{vector of transformed weights meeting constraints.}
#' \item{min: }{vector of min box constraints that may have been modified if relax=TRUE.}
#' \item{max: }{vector of max box constraints that may have been modified if relax=TRUE.}
#' \item{cLO: }{vector of lower bound group constraints that may have been modified if relax=TRUE.}
#' \item{cUP: }{vector of upper bound group constraints that may have been modified if relax=TRUE.}
#' }
#' @author Ross Bennett
#' @export
fn_map <- function(weights, portfolio, relax=FALSE, verbose=FALSE, ...){
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
  
  weight_seq <- portfolio$weight_seq
  if(is.null(weight_seq)){
    weight_seq <- generatesequence(min=min(constraints$min), max=max(constraints$max), by=0.002)
  }
  weight_seq <- as.vector(weight_seq)
  
  min <- constraints$min
  max <- constraints$max
  groups <- constraints$groups
  cLO <- constraints$cLO
  cUP <- constraints$cUP
  group_pos <- constraints$group_pos
  div_target <- constraints$div_target
  turnover_target <- constraints$turnover_target
  max_pos <- constraints$max_pos
  max_pos_long <- constraints$max_pos_long
  max_pos_short <- constraints$max_pos_short
  leverage <- constraints$leverage
  tolerance <- .Machine$double.eps^0.5
  
  # We will modify the weights vector so create a temporary copy
  # modified for transformation or to relax constraints
  tmp_weights <- weights
  tmp_min <- min
  tmp_max <- max
  tmp_cLO <- cLO
  tmp_cUP <- cUP
  tmp_max_pos <- max_pos
  tmp_max_pos_long <- max_pos_long
  tmp_max_pos_short <- max_pos_short
  tmp_leverage <- leverage
  
  # Do we need to step through each constraint type sequentially or can we just
  # call rp_transform once now that it has been modified to handle constraint
  # types seperately?
  
  # step 2: check that the vector of weights satisfies the constraints, 
  # transform weights if constraint is violated
  # TRUE if the weights vector is in compliance with the constraints
  # FALSE if the weights vector violates the constraint
  
  # check leverage constraints
  if(!is.null(min_sum) & !is.null(max_sum)){
    if(!(sum(tmp_weights) >= min_sum & sum(tmp_weights) <= max_sum)){
      # Try to transform only considering leverage and box constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, 
                                      max_sum=max_sum, 
                                      min_box=tmp_min, 
                                      max_box=tmp_max, 
                                      groups=NULL, 
                                      cLO=NULL, 
                                      cUP=NULL, 
                                      max_pos=NULL, 
                                      group_pos=NULL, 
                                      max_pos_long=NULL, 
                                      max_pos_short=NULL, 
                                      leverage=tmp_leverage, 
                                      weight_seq=weight_seq,
                                      max_permutations=500), 
                         silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
      } # end try-error recovery
    } # end check for leverage constraint violation
  } # end check for NULL arguments
  
  # check box constraints
  if(!is.null(tmp_min) & !is.null(tmp_max)){
    if(!(all(tmp_weights >= tmp_min) & all(tmp_weights <= tmp_max))){
      # Try to transform only considering leverage and box constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, 
                                      max_sum=max_sum, 
                                      min_box=tmp_min, 
                                      max_box=tmp_max, 
                                      groups=NULL, 
                                      cLO=NULL, 
                                      cUP=NULL, 
                                      max_pos=NULL, 
                                      group_pos=NULL, 
                                      max_pos_long=NULL, 
                                      max_pos_short=NULL, 
                                      leverage=tmp_leverage, 
                                      weight_seq=weight_seq,
                                      max_permutations=500), 
                         silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        if(verbose) message(tmp_weights)
        # Default to initial weights
        tmp_weights <- weights
        # Try to relax constraints if relax=TRUE
        if(relax){
          i <- 1
          # loop while constraints are violated and relax constraints
          # try to relax constraints up to 5 times
          while((sum(tmp_weights) < min_sum | sum(tmp_weights) > max_sum | any(tmp_weights < tmp_min) | any(tmp_weights > tmp_max)) & i <= 5){
            # check if min is violated
            if(any(tmp_weights < tmp_min)){
              # Find which elements of min are violated and decrease by a random amount
              tmp_min[which(tmp_weights < tmp_min)] <- tmp_min[which(tmp_weights < tmp_min)] - runif(1, 0.01, 0.05)
            }
            # check if max is violated
            if(any(tmp_weights > tmp_max)){
              # Find which elements of min are violated and increase by a random amount
              tmp_max[which(tmp_weights < tmp_max)] <- tmp_max[which(tmp_weights < tmp_max)] + runif(1, 0.01, 0.05)
            }
            
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, 
                                            max_sum=max_sum, 
                                            min_box=tmp_min, 
                                            max_box=tmp_max, 
                                            groups=NULL, 
                                            cLO=NULL, 
                                            cUP=NULL, 
                                            max_pos=NULL, 
                                            group_pos=NULL, 
                                            max_pos_long=NULL, 
                                            max_pos_short=NULL, 
                                            leverage=tmp_leverage, 
                                            weight_seq=weight_seq,
                                            max_permutations=500), 
                               silent=TRUE) # FALSE for testing
            # Default to original weights if this fails again
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
          # We have a feasible portfolio in terms of min_sum and max_sum, 
          # but were unable to produce a portfolio that satisfies box constraints
          if(isTRUE(all.equal(tmp_weights, weights))){
            # reset min and max to their original values and penalize later
            tmp_min <- min
            tmp_max <- max
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for box constraint violation
  } # end check for NULL arguments
  
  # check group constraints
  if(!is.null(groups) & !is.null(tmp_cLO) & !is.null(tmp_cUP)){
    if(any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))){
      # Try to transform only considering leverage, box, and group constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, 
                                      max_sum=max_sum, 
                                      min_box=tmp_min, 
                                      max_box=tmp_max, 
                                      groups=groups, 
                                      cLO=tmp_cLO, 
                                      cUP=tmp_cUP, 
                                      max_pos=NULL, 
                                      group_pos=group_pos, 
                                      max_pos_long=NULL, 
                                      max_pos_short=NULL, 
                                      leverage=tmp_leverage, 
                                      weight_seq=weight_seq,
                                      max_permutations=500), 
                         silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        if(verbose) message(tmp_weights)
        # Default to initial weights
        tmp_weights <- weights
        # Try to relax constraints if relax=TRUE
        if(relax){
          i <- 1
          # loop while constraints are violated and relax constraints
          # Try to relax constraints up to 5 times
          while(((sum(tmp_weights) < min_sum | sum(tmp_weights) > max_sum) | (any(tmp_weights < tmp_min) | any(tmp_weights > tmp_max)) | any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))) & i <= 5){
            if(any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))){
              # I know which group failed, but not if it was cUP or cLO that was violated
              # Maybe I can modify group_fail to report back what was violated and only relax cLO or cUP, not both
              # This relaxes both cLO and cUP
              tmp_cLO[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] <- tmp_cLO[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] - runif(1, 0.01, 0.05)
              tmp_cUP[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] <- tmp_cUP[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] + runif(1, 0.01, 0.05)
            }
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, 
                                            max_sum=max_sum, 
                                            min_box=tmp_min, 
                                            max_box=tmp_max, 
                                            groups=groups,
                                            cLO=tmp_cLO, 
                                            cUP=tmp_cUP, 
                                            max_pos=NULL, 
                                            group_pos=group_pos, 
                                            max_pos_long=NULL, 
                                            max_pos_short=NULL, 
                                            leverage=tmp_leverage, 
                                            weight_seq=weight_seq,
                                            max_permutations=500), 
                               silent=TRUE) # FALSE for testing
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
          # We have a feasible portfolio in terms of min_sum and max_sum, 
          # but were unable to produce a portfolio that satisfies group constraints
          if(isTRUE(all.equal(tmp_weights, weights))){
            # reset min and max to their original values and penalize later
            tmp_cLO <- cLO
            tmp_cUP <- cUP
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for group constraint violation
  } # end check for NULL arguments
  
  # check position_limit constraints
  if(!is.null(tmp_max_pos) | !is.null(tmp_max_pos_long) | !is.null(tmp_max_pos_short)){
    if(pos_limit_fail(tmp_weights, tmp_max_pos, tmp_max_pos_long, tmp_max_pos_short)){
      # Try to transform only considering leverage, box, group, and position_limit constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, 
                                      max_sum=max_sum, 
                                      min_box=tmp_min, 
                                      max_box=tmp_max, 
                                      groups=groups, 
                                      cLO=tmp_cLO, 
                                      cUP=tmp_cUP, 
                                      max_pos=tmp_max_pos, 
                                      group_pos=group_pos, 
                                      max_pos_long=tmp_max_pos_long,
                                      max_pos_short=tmp_max_pos_short, 
                                      leverage=tmp_leverage, 
                                      weight_seq=weight_seq,
                                      max_permutations=500), 
                         silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        if(verbose) message(tmp_weights)
        # Default to initial weights
        tmp_weights <- weights
        if(relax){
          i <- 1
          while(pos_limit_fail(tmp_weights, tmp_max_pos, tmp_max_pos_long, tmp_max_pos_short) & (i <= 5)){
            # increment tmp_max_pos by 1
            if(!is.null(tmp_max_pos)) tmp_max_pos <- min(nassets, tmp_max_pos + 1)
            if(!is.null(tmp_max_pos_long)) tmp_max_pos_long <- min(nassets, tmp_max_pos_long + 1)
            if(!is.null(tmp_max_pos_short)) tmp_max_pos_short <- min(nassets, tmp_max_pos_short + 1)
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, 
                                            max_sum=max_sum, 
                                            min_box=tmp_min, 
                                            max_box=tmp_max, 
                                            groups=groups, 
                                            cLO=tmp_cLO, 
                                            cUP=tmp_cUP, 
                                            max_pos=tmp_max_pos, 
                                            group_pos=group_pos, 
                                            max_pos_long=tmp_max_pos_long, 
                                            max_pos_short=tmp_max_pos_short, 
                                            leverage=tmp_leverage, 
                                            weight_seq=weight_seq,
                                            max_permutations=500), 
                               silent=TRUE) # FALSE for testing
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for position limit constraint violation
  } # end check for NULL arguments
  
  # check leverage constraints
  if(!is.null(tmp_leverage)){
    if(sum(abs(tmp_weights)) > tmp_leverage){
      # Try to transform only considering weight_sum, box, group, position_limit, and leverage exposure constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, 
                                      max_sum=max_sum, 
                                      min_box=tmp_min, 
                                      max_box=tmp_max, 
                                      groups=groups, 
                                      cLO=tmp_cLO, 
                                      cUP=tmp_cUP, 
                                      max_pos=tmp_max_pos, 
                                      group_pos=group_pos, 
                                      max_pos_long=tmp_max_pos_long, 
                                      max_pos_short=tmp_max_pos_short, 
                                      leverage=tmp_leverage, 
                                      weight_seq=weight_seq,
                                      max_permutations=500), 
                         silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        if(verbose) message(tmp_weights)
        # Default to initial weights
        tmp_weights <- weights
        if(relax){
          i <- 1
          while(sum(abs(tmp_weights)) > tmp_leverage & (i <= 5)){
            # increment tmp_leverage by 1%
            tmp_leverage <- tmp_leverage * 1.01
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, 
                                            max_sum=max_sum, 
                                            min_box=tmp_min, 
                                            max_box=tmp_max, 
                                            groups=groups, 
                                            cLO=tmp_cLO, 
                                            cUP=tmp_cUP, 
                                            max_pos=tmp_max_pos, 
                                            group_pos=group_pos, 
                                            max_pos_long=tmp_max_pos_long, 
                                            max_pos_short=tmp_max_pos_short, 
                                            leverage=tmp_leverage, 
                                            weight_seq=weight_seq,
                                            max_permutations=500), 
                               silent=TRUE) # FALSE for testing
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for leverage exposure violation
  } # end check for NULL arguments

  names(tmp_weights) <- names(weights)
  return(list(weights=tmp_weights, 
              min=tmp_min, 
              max=tmp_max, 
              cLO=tmp_cLO, 
              cUP=tmp_cUP, 
              max_pos=tmp_max_pos,
              max_pos_long=tmp_max_pos_long,
              max_pos_short=tmp_max_pos_short,
              leverage=tmp_leverage))
}



#' Transform a weights vector to satisfy constraints
#' 
#' This function uses a block of code from \code{\link{randomize_portfolio}} 
#' to transform the weight vector if either the weight_sum (leverage) 
#' constraints, box constraints, group constraints, position_limit constraints,
#' or leverage exposure constraints are violated. The logic from 
#' \code{randomize_portfolio} is heavily utilized here with extensions to
#' handle more complex constraints.
#' The resulting weights vector might be quite different from the original weights vector.
#' 
#' @param w weights vector to be transformed
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param min_box numeric or named vector specifying minimum weight box constraints
#' @param max_box numeric or named vector specifying maximum weight box constraints
#' @param groups vector specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @param max_pos maximum assets with non-zero weights
#' @param group_pos vector specifying maximum number assets with non-zero weights per group
#' @param max_pos_long maximum number of assets with long (i.e. buy) positions
#' @param max_pos_short maximum number of assets with short (i.e. sell) positions
#' @param leverage maximum leverage exposure where leverage is defined as \code{sum(abs(weights))}
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @param weight_seq vector of seed sequence of weights
#' @return named weighting vector
#' @author Peter Carl, Brian G. Peterson, Ross Bennett (based on an idea by Pat Burns)
#' @export
rp_transform <- function(w, 
                         min_sum, 
                         max_sum, 
                         min_box, 
                         max_box, 
                         groups=NULL, 
                         cLO=NULL, 
                         cUP=NULL, 
                         max_pos=NULL, 
                         group_pos=NULL, 
                         max_pos_long=NULL, 
                         max_pos_short=NULL, 
                         leverage=NULL, 
                         weight_seq=NULL,
                         max_permutations=200){
  tmp_w <- w
  
  # Set some reasonable default values
  # Maybe I should leave these as NULL values and incorporate that into the
  # checks
  #if(is.null(min_sum)) min_sum <- 0.99
  #if(is.null(max_sum)) max_sum <- 1.01
  #if(is.null(min_box)) min_box <- rep(-Inf, length(tmp_w))
  #if(is.null(max_box)) max_box <- rep(Inf, length(tmp_w))
  if(is.null(max_pos)) max_pos <- length(tmp_w)
  #if(is.null(max_poslong)) max_pos_long <- length(tmp_w)
  #if(is.null(max_pos_short)) max_pos_short <- length(tmp_w)
  #if(is.null(leverage)) leverage <- Inf
  
  # Generate a weight sequence, we should check for portfolio$weight_seq
  if(is.null(weight_seq))
    weight_seq <- generatesequence(min=min(min_box), max=max(max_box), by=0.002)
  
  # make sure there is a 0 in weight_seq if we have a position limit constraint
  if((!is.null(max_pos) | !is.null(group_pos) | !is.null(max_pos_long) | !is.null(max_pos_short)) & !is.element(0, weight_seq)) weight_seq <- c(0, weight_seq)
  
  # Tolerance for "non-zero" definition for position limit constraints
  tolerance <- .Machine$double.eps^0.5
  
  # initialize the outer while loop
  permutations <- 1
  
  # while we have not reached max_permutations and the following constraints 
  # are violated:
  # - min_sum
  # - max_sum
  # - leverage
  # - max_pos, max_pos_long, max_pos_short
  # - group
  
  # Do we want to check all constraints in here?
  # Box constraints should be satisfied by construction so we should not need 
  # to check those here
  while ((    min_sum_fail(tmp_w, min_sum) | 
              max_sum_fail(tmp_w, max_sum) | 
              leverage_fail(tmp_w, leverage) | 
              pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short) | 
              any(group_fail(tmp_w, groups, cLO, cUP)) ) & 
           (permutations < max_permutations)) {
    
    # cat("permutation #:", permutations, "\n")
    permutations <- permutations+1
    
    # Reset tmp_w to original weights vector
    # I'm not sure we want to do this here because it puts us back to where we
    # started, but it seems to help with the position limit constraint
    # tmp_w <- weights
    
    # Reset the random index based on the maximum position constraint
    # This basically allows us to generate a portfolio of max_pos assets 
    # with the given constraints and then add assets with zero weight
    random_index <- sample(1:length(tmp_w), max_pos)
    
    # Get the index values that are not in random_index and set them equal to 0
    full_index <- 1:length(tmp_w)
    not_index <- setdiff(full_index, random_index)
    tmp_w[not_index] <- 0
    
    # min_sum violation
    if(min_sum_fail(tmp_w, min_sum)){
      tmp_w <- rp_increase(weights=tmp_w, 
                           min_sum=min_sum, 
                           max_box=max_box, 
                           weight_seq=weight_seq)
    }
    
    # max_sum violation
    if(max_sum_fail(tmp_w, max_sum)){
      tmp_w <- rp_decrease(weights=tmp_w, 
                           max_sum=max_sum, 
                           min_box=min_box, 
                           weight_seq=weight_seq)
    }
    
    # leverage violation
    if(leverage_fail(tmp_w, leverage)){
      tmp_w <- rp_decrease_leverage(weights=tmp_w, 
                                    max_box=max_box, 
                                    min_box=min_box, 
                                    leverage=leverage, 
                                    weight_seq=weight_seq)
    }
    
    # position limit violation
    if(pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short)){
      tmp_w <- rp_position_limit(weights=tmp_w, 
                                 min_box=min_box, 
                                 max_box=max_box, 
                                 max_pos=max_pos, 
                                 max_pos_long=max_pos_long, 
                                 max_pos_short=max_pos_short, 
                                 weight_seq=weight_seq)
    }
    
    # group violation
    if(any(group_fail(tmp_w, groups, cLO, cUP, group_pos))){
      n_groups <- length(groups)
      for(j in 1:n_groups){
        # index of the weights vector belonging to the jth group
        j_idx <- groups[[j]]
        # weights of the jth group
        tmp_group_w <- tmp_w[j_idx]
        
        # May be easier to just make a recursive call and treat each group
        # as a portfolio of weight vectors
        tmp_w[j_idx] <- rp_transform(w=tmp_group_w, 
                                     min_sum=cLO[j], 
                                     max_sum=cUP[j], 
                                     min_box=min_box[j_idx], 
                                     max_box=max_box[j_idx],
                                     group_pos=group_pos[j])
        
        # treat this as if min_sum were violated
        #         if(sum(tmp_group_w) < cLO[j]){
        #           tmp_w[j_idx] <- rp_increase(weights=tmp_group_w, 
        #                                       min_sum=cLO[j], 
        #                                       max_box=max_box[j_idx], 
        #                                       weight_seq=weight_seq)
        #         }
        
        # treat this as if max_sum were violated
        #         if(sum(tmp_group_w) > cUP[j]){
        #           tmp_w[j_idx] <-  rp_decrease(weights=tmp_group_w, 
        #                                        max_sum=cUP[j], 
        #                                        min_box=min_box[j_idx], 
        #                                        weight_seq=weight_seq)
        #         }
      }
    } # end group violation loop
  } # end final walk towards the edges
  portfolio <- tmp_w
  
  colnames(portfolio) <- colnames(w)
  
  # checks for infeasible portfolio
  # Stop execution and return an error if an infeasible portfolio is created
  # This will be useful in fn_map so that we can catch the error and take
  # action (try again with more permutations, relax constraints, different
  # method to normalize, etc.)
  if (sum(portfolio) < min_sum | sum(portfolio) > max_sum){
    portfolio <- w
    stop("Infeasible portfolio created, perhaps increase max_permutations and/or adjust your parameters.")
  }
  return(portfolio)
}

# rp_transform <- function(w, 
#                          min_sum=0.99, 
#                          max_sum=1.01, 
#                          min, 
#                          max, 
#                          groups, 
#                          cLO, 
#                          cUP, 
#                          max_pos=NULL, 
#                          group_pos=NULL, 
#                          max_pos_long=NULL, 
#                          max_pos_short=NULL, 
#                          leverage=NULL, 
#                          max_permutations=200){
#   # Uses logic from randomize_portfolio to "normalize" a weights vector to 
#   # satisfy min_sum and max_sum while accounting for box and group constraints
#   # Modified from randomize_portfolio to trigger the while loops if any weights 
#   # violate min or max box constraints. A weights vector would not be transformed
#   # in randomize_portfolio if min_sum and max_sum were satisfied, but the
#   # min/max constraints were violated.
#   
#   # Set the tolerance to determine non-zero weights
#   tolerance=.Machine$double.eps^0.5
#   
#   # Set value for max_pos if it is not specified
#   if(is.null(max_pos)) max_pos <- length(w)
#   
#   # Set value for leverage if it is not specified
#   if(is.null(leverage)) leverage <- Inf
#   
#   # Determine maximum number of non-zero weights
#   if(!is.null(group_pos)) {
#     max_group_pos <- sum(group_pos)
#   } else {
#     max_group_pos <- length(w)
#   }
#   
#   # Set maximum number of assets based on max_pos and group_pos
#   max_assets <- min(max_pos, max_group_pos)
#   
#   # Create a temporary min vector that will be modified, because a feasible
#   # portfolio is rarely created if all(min > 0). This is due to the while
#   # loop that checks any(tmp_w < min).
#   tmp_min <- min
#   
#   # If weight_i = 0 and min_i > 0, then this will violate box constraints
#   # even though weight_i = 0 to satisfy position_limit constraints. Modify
#   # the tmp_min vector and set tmp_min_i equal to zero where weights_i = 0.
#   # If w is less than or equal to tolerance then it is essentially 0
#   if(any(abs(w) <= tolerance)){
#     if(any(tmp_min[which(abs(w) <= tolerance)] > 0)){
#       tmp_min[which(abs(w) <= tolerance)] <- -tolerance
#     }
#   }
#   
#   # return w if all constraints are satisfied
#   if((sum(w) >= min_sum & sum(w) <= max_sum) & 
#        (all(w >= tmp_min) & all(w <= max)) & 
#        (all(!group_fail(w, groups, cLO, cUP, group_pos))) &
#        !pos_limit_fail(w, max_pos, max_pos_long, max_pos_short) &
#        (sum(abs(w)) <= leverage)){
#     return(w)
#   }
#   
#   # generate a sequence of weights based on min/max box constraints
#   weight_seq <- generatesequence(min=min(min), max=max(max), by=0.002)
#   # make sure there is a 0 in weight_seq
#   if((!is.null(max_pos) | !is.null(group_pos) | !is.null(max_pos_long) | !is.null(max_pos_short)) & !is.element(0, weight_seq)) weight_seq <- c(0, weight_seq)
#   
#   # start the permutations counter
#   permutations <- 1
#   
#   # create a temporary weights vector that will be modified in the while loops
#   tmp_w <- w
#   
#   # while any constraint is violated and we have not reached max_permutations
#   while ((sum(tmp_w) < min_sum | 
#           sum(tmp_w) > max_sum | 
#           any(tmp_w < tmp_min) | 
#           any(tmp_w > max) | 
#           any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | 
#           pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short) |
#           sum(abs(w)) > leverage) & 
#           permutations <= max_permutations) {
#     permutations = permutations + 1
#     # check our box constraints on total portfolio weight
#     # reduce(increase) total portfolio size till you get a match
#     # 1> check to see which bound you've failed on, probably set this as a pair of while loops
#     # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
#     # 3> check and repeat
#     
#     # reset tmp_w and tmp_min to their original values
#     tmp_w <- w
#     tmp_min <- min
#     
#     random_index <- sample(1:length(tmp_w), max_assets)
#     
#     # Get the index values that are not in random_index and set them equal to 0
#     full_index <- 1:length(tmp_w)
#     not_index <- setdiff(full_index, random_index)
#     tmp_w[not_index] <- 0
#     
#     # set some tmp_min values equal to zero so the while loops do not see a
#     # violation of any(tmp_w < tmp_min). This tends to force weights to 0 and
#     # works well for long only, but we may want to allow negative weights.
#     # tmp_min[not_index] <- 0
#     # Only set values of tmp_min that are greater than 0 to 0
#     tmp_min[not_index[which(tmp_min[not_index] > 0)]] <- 0
#     
#     # Transform weights to satisfy max_pos_long and max_pos_short before being
#     # passed into the main loops
#     # Both max_pos_long and max_pos_short should be specified
#     if(!is.null(max_pos_long)){
#       pos_idx <- which(tmp_w > 0)
#       neg_idx <- which(tmp_w < 0)
#       
#       # Check if number of positive weights exceeds max_pos_long
#       if(length(pos_idx) > max_pos_long){
#         # Randomly sample positive weights that cause violation of max_pos_long
#         # and replace with randomly sampled negative weights from weight_seq
#         make_neg_idx <- sample(pos_idx, length(pos_idx) - max_pos_long)
#         for(i in make_neg_idx){
#           tmp_idx <- weight_seq[weight_seq < 0 & weight_seq >= min[i]]
#           if(length(tmp_idx) > 0){
#             tmp_w[i] <- sample(tmp_idx, 1)
#           } else {
#             # This should never happen if the correct weight_seq and min is specified
#             tmp_w[i] <- -tmp_w[i]
#           }
#         }
#       }
#     }
#     if(!is.null(max_pos_short)){
#       # Check if number of negative weights exceeds max_pos_short
#       if(length(neg_idx) > max_pos_short){
#         # Randomly sample negative weights that cause violation of max_pos_short
#         # and replace with randomly sampled positive weights from weight_seq
#         make_pos_idx <- sample(neg_idx, length(neg_idx) - max_pos_short)
#         for(i in make_pos_idx){
#           tmp_seq <- weight_seq[weight_seq > 0 & weight_seq <= max[i]]
#           if(length(tmp_seq) > 0){
#             tmp_w[i] <- sample(tmp_seq, 1)
#           } else {
#             # This should never happen if the correct weight_seq and max is specified
#             tmp_w[i] <- -tmp_w[i]
#           }
#         }
#       }
#     }
#     
#     i = 1
#     # We increase elements here if the sum of the weights exceeds max_sum or
#     # any of the other constraints are violated
#     while ((sum(tmp_w) < min_sum | 
#             any(tmp_w < tmp_min) | 
#             any(tmp_w > max) | 
#             any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | 
#             pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short) |
#             sum(abs(tmp_w)) > leverage) & 
#             i <= length(tmp_w)) {
#       # randomly permute and increase a random portfolio element
#       cur_index <- random_index[i]
#       cur_val <- tmp_w[cur_index]
#       tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
#       n_tmp_seq <- length(tmp_seq)
#       if (n_tmp_seq > 1) {
#         # randomly sample an element from weight_seq that is greater than cur_val and less than max
#         # tmp_w[cur_index] <- sample(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])], 1)
#         tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
#         # print(paste("new val:",tmp_w[cur_index]))
#       } else {
#         if (n_tmp_seq == 1) {
#           # tmp_w[cur_index] <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
#           tmp_w[cur_index] <- tmp_seq
#         }
#       }
#       i=i+1 # increment our counter
#     } # end increase loop
#     # need to reset i here otherwise the decreasing loop will be ignored
#     # group_fail does not test for direction of violation, just that group constraints were violated
#     i = 1 
#     # We decrease elements here if the sum of the weights exceeds max_sum or
#     # any of the other constraints are violated
#     while ((sum(tmp_w) > max_sum | 
#             any(tmp_w < tmp_min) | 
#             any(tmp_w > max) | 
#             any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | 
#             pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short) |
#             sum(abs(tmp_w)) > leverage) & 
#             i <= length(tmp_w)) {
#       # randomly permute and decrease a random portfolio element
#       cur_index <- random_index[i]
#       cur_val <- tmp_w[cur_index]
#       tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])]
#       n_tmp_seq <- length(tmp_seq)
#       if (n_tmp_seq > 1) {
#         # randomly sample an element from weight_seq that is less than cur_val and greater than tmp_min
#         # tmp_w[cur_index] <- sample(weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])] , 1)
#         tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
#       } else {
#         if (n_tmp_seq == 1) {
#           # tmp_w[cur_index] <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])]
#           tmp_w[cur_index] <- tmp_seq
#         }
#       }
#       i=i+1 # increment our counter
#     } # end decrease loop
#     #cat("permutations:", permutations, "\n")
#     #cat("weights:", tmp_w, "\n")
#     #cat("sum(weights):", sum(tmp_w), "\n")
#     #cat("sum(abs(weights)):", sum(abs(tmp_w)), "\n")
#   } # end final walk towards the edges
#   
#   portfolio <- tmp_w
#   
#   colnames(portfolio)<-colnames(w)
#   
#   # checks for infeasible portfolio
#   # Stop execution and return an error if an infeasible portfolio is created
#   # This will be useful in fn_map so that we can catch the error and take
#   # action (try again with more permutations, relax constraints, different
#   # method to normalize, etc.)
#   if (sum(portfolio) < min_sum | sum(portfolio) > max_sum){
#     portfolio <- w
#     stop("Infeasible portfolio created, perhaps increase max_permutations and/or adjust your parameters.")
#   }
# #   if(isTRUE(all.equal(w,portfolio))) {
# #     if (sum(w)>=min_sum & sum(w)<=max_sum) {
# #       warning("Unable to generate a feasible portfolio different from w, perhaps adjust your parameters.")
# #       return(w)
# #     } else {
# #       warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
# #       return(NULL)
# #     }
# #   }
#   return(portfolio)
# }

#' Test if group constraints have been violated
#' 
#' The function loops through each group and tests if cLO or cUP have been violated
#' for the given group. This is a helper function for \code{\link{rp_transform}}.
#' 
#' @param weights weights vector to test
#' @param groups list of vectors specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @param group_pos vector specifying the number of non-zero weights per group
#' @return logical vector: TRUE if group constraints are violated for a given group
#' @author Ross Bennett
group_fail <- function(weights, groups, cLO, cUP, group_pos=NULL){
  # return FALSE if groups, cLO, or cUP is NULL
  if(is.null(groups) | is.null(cLO) | is.null(cUP)) return(FALSE)
  group_count <- sapply(groups, length)
  # group_pos sets a limit on the number of non-zero weights by group
  # Set equal to groups if NULL
  if(is.null(group_pos)) group_pos <- group_count
  tolerance <- .Machine$double.eps^0.5
  
  n.groups <- length(groups)
  group_fail <- vector(mode="logical", length=n.groups)
  
  for(i in 1:n.groups){
    # sum of the weights for a given group
    tmp.w <- weights[groups[[i]]]
    group_fail[i] <- ( (sum(tmp.w) < cLO[i]) | (sum(tmp.w) > cUP[i]) | (sum(abs(tmp.w) > tolerance) > group_pos[i]) )
  }
  # returns logical vector of groups. TRUE if either cLO or cUP is violated
  return(group_fail)
}

#' function to check for violation of position limits constraints
#' 
#' This is used as a helper function for \code{\link{rp_transform}} to check
#' for violation of position limit constraints. The position limit constraints
#' checked are max_pos, max_pos_long, and max_pos_short. 
#' 
#' @param weights vector of weights to test
#' @param max_pos maximum number of assets with non-zero weights
#' @param max_pos_long maximum number of assets with long (i.e. buy) positions
#' @param max_pos_short maximum number of assets with short (i.e. sell) positions
#' @return TRUE if any position_limit is violated. FALSE if all position limits are satisfied
#' @export
pos_limit_fail <- function(weights, max_pos, max_pos_long, max_pos_short){
  # tolerance for "non-zero" definition
  tolerance <- .Machine$double.eps^0.5
  
  # Check if max_pos is violated
  if(!is.null(max_pos)){
    if(sum(abs(weights) > tolerance) > max_pos){
      return(TRUE)
    }
  }
  
  # Check if max_pos_long is violated
  if(!is.null(max_pos_long)){
    if(sum(weights > tolerance) > max_pos_long){
      return(TRUE)
    }
  }
  
  # Check if max_pos_short is violated
  if(!is.null(max_pos_short)){
    if(sum(weights < -tolerance) > max_pos_short){
      return(TRUE)
    }
  }
  # Return FALSE if nothing is violated
  return(FALSE)
}

min_sum_fail <- function(weights, min_sum){
  # return FALSE if min_sum is null
  if(is.null(min_sum)) return(FALSE)
  
  # sum of weights violate min_sum constraint
  return(sum(weights) < min_sum)
}

max_sum_fail <- function(weights, max_sum){
  # return FALSE if max_sum is null
  if(is.null(max_sum)) return(FALSE)
  
  # sum of weights violate max_sum constraint
  return(sum(weights) > max_sum)
}

leverage_fail <- function(weights, leverage){
  # return FALSE if leverage is null
  if(is.null(leverage)) return(FALSE)
  
  # sum of absolute value of weight violates leverage constraint
  return(sum(abs(weights)) > leverage)
}

rp_increase <- function(weights, min_sum, max_box, weight_seq){
  # randomly permute and increase a random portfolio element if the sum of
  # the weights is less than min_sum while respecting box constraints
  
  if(sum(weights) >= min_sum) return(weights)
  
  tmp_w <- weights
  n_weights <- length(weights)
  # random_index <- sample(1:length(weights), max_pos)
  random_index <- sample(1:n_weights, n_weights)
  i <- 1
  while (sum(tmp_w) < min_sum & i <= n_weights) {
    # print("min_sum violation loop")
    
    cur_index <- random_index[i]
    cur_val <- tmp_w[cur_index]
    tmp_seq <- weight_seq[(weight_seq > cur_val) & (weight_seq <= max_box[cur_index])]
    n_tmp_seq <- length(tmp_seq)
    if(n_tmp_seq > 1){
      # randomly sample one of the larger weights
      tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    } else if(n_tmp_seq == 1){
      tmp_w[cur_index] <- tmp_seq
    }
    i <- i + 1 # increment our counter
  } # end increase loop
  return(tmp_w)
}

rp_decrease <- function(weights, max_sum, min_box, weight_seq){
  # randomly permute and decrease a random portfolio element if the sum of
  # the weights is greater than max_sum while respecting box constraints
  
  if(sum(weights) <= max_sum) return(weights)
  
  tmp_w <- weights
  n_weights <- length(weights)
  # random_index <- sample(1:length(weights), max_pos)
  random_index <- sample(1:n_weights, n_weights)
  
  i <- 1
  while (sum(tmp_w) > max_sum & i <= n_weights) {
    # print("max_sum violation loop")
    
    cur_index <- random_index[i]
    cur_val <- tmp_w[cur_index]
    tmp_seq <- weight_seq[(weight_seq < cur_val) & (weight_seq >= min_box[cur_index])]
    n_tmp_seq <- length(tmp_seq)
    if(n_tmp_seq > 1){
      tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    } else if(n_tmp_seq == 1){
      tmp_w[cur_index] <- tmp_seq
    }
    i <- i + 1 # increment our counter
  } # end decrease loop
  return(tmp_w)
}

rp_decrease_leverage <- function(weights, max_box, min_box, leverage, weight_seq){
  # randomly permute and increae decrease a random portfolio element
  # according to leverage exposure while respecting box constraints
  
  tmp_w <- weights
  n_weights <- length(weights)
  # random_index <- sample(1:length(weights), max_pos)
  random_index <- sample(1:n_weights, n_weights)
  
  # set counter to 1 for leverage violation loop
  i <- 1
  while (sum(abs(tmp_w)) > leverage & i <= length(tmp_w)) {
    #print("leverage violation loop")
    
    cur_index <- random_index[i]
    cur_val <- tmp_w[cur_index]
    
    tmp_seq <- NULL
    # check the sign of the current value
    if(cur_val < 0){
      # if the current value is negative, we want to increase to lower 
      # sum(abs(weights)) while respecting uppper bound box constraint
      tmp_seq <- weight_seq[(weight_seq > cur_val) & (weight_seq <= max_box[cur_index])]
    } else if(cur_val > 0){
      # if the current value is positive, we want to decrease to lower 
      # sum(abs(weights)) while respecting lower bound box constraint
      tmp_seq <- weight_seq[(weight_seq < cur_val) & (weight_seq >= min_box[cur_index])]
    }
    # tmp_seq can be NULL if cur_val is zero 
    if(!is.null(tmp_seq)){
      n_tmp_seq <- length(tmp_seq)
      
      if(n_tmp_seq > 1) {
        # randomly sample one of the weights
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
      } else if(n_tmp_seq == 1){
        tmp_w[cur_index] <- tmp_seq
      }
    }
    i <- i + 1 # increment our counter
  } # end leverage violation loop
  return(tmp_w)
}

rp_position_limit <- function(weights, max_pos=NULL, max_pos_long=NULL, max_pos_short=NULL, min_box, max_box, weight_seq){
  tmp_w <- weights
  n_weights <- length(weights)
  # random_index <- sample(1:length(weights), max_pos)
  random_index <- sample(1:n_weights, n_weights)
  
  tolerance <- .Machine$double.eps^0.5
  
  # set counter to 1 for position limit violation loop
  i <- 1
  while ( pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short) & i <= length(tmp_w)) {
    #print("position limit violation loop")
    
    cur_index <- random_index[i]
    cur_val <- tmp_w[cur_index]
    
    if(!is.null(max_pos_long)){
      # Check if max_pos_long is violated
      # If max_pos_long is violated, we we grab a positive weight and set it
      # to be between min_box and 0
      if(sum(tmp_w > tolerance) > max_pos_long){
        if(cur_val > tolerance){
          # subset such that min_box_i <= weight_i <= 0
          tmp_seq <- weight_seq[(weight_seq <= 0) & (weight_seq >= min_box[cur_index])]
          n_tmp_seq <- length(tmp_seq)
          if(n_tmp_seq > 1){
            tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
          } else if(n_tmp_seq == 1){
            tmp_w[cur_index] <- tmp_seq
          }
        }
      } # end max_pos_long violation loop
    }
    
    if(!is.null(max_pos_short)){
      # Check if max_pos_short is violated
      # If max_pos_short is violated, we grab a negative weight and set it
      # to be between 0 and max_box
      if(sum(tmp_w < tolerance) > max_pos_short){
        if(cur_val < tolerance){
          # subset such that 0 <= weight_i <= max_box_i
          tmp_seq <- weight_seq[(weight_seq >= 0) & (weight_seq <= max_box[cur_index])]
          n_tmp_seq <- length(tmp_seq)
          if(n_tmp_seq > 1){
            tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
          } else if(n_tmp_seq == 1){
            tmp_w[cur_index] <- tmp_seq
          }
        }
      } # end max_pos_short violation loop
    }
    i <- i + 1 # increment our counter
  } # end position limit violation loop
  return(tmp_w)
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


###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2018 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
