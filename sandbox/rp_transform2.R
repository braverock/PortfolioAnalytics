

rp_transform2 <- function(weights, 
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
                          max_permutations=200){
  tmp_w <- weights
  
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
  weight_seq <- generatesequence(min=min(min_box), max=max(max_box), by=0.002)
  
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
  while ((  min_sum_fail(tmp_w, min_sum) | 
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
    
    # randomly permute and increase a random portfolio element if the sum of
    # the weights is less than min_sum
    # set counter to 1 for increase loop
    #     i <- 1
    #     while (sum(tmp_w) < min_sum & i <= length(tmp_w)) {
    #       print("min_sum violation loop")
    #       
    #       cur_index <- random_index[i]
    #       cur_val <- tmp_w[cur_index]
    #       tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max_box[cur_index])]
    #       n_tmp_seq <- length(tmp_seq)
    #       if(n_tmp_seq > 1){
    #         # randomly sample one of the larger weights
    #         tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    #         # print(paste("new val:",tmp_w[cur_index]))
    #       } else {
    #         if(n_tmp_seq == 1){
    #           tmp_w[cur_index] <- tmp_seq
    #         }
    #       }
    #       i <- i + 1 # increment our counter
    #     } # end increase loop
    
    # min_sum violation
    if(min_sum_fail(tmp_w, min_sum)){
      tmp_w <- rp_increase(weights=tmp_w, 
                           min_sum=min_sum, 
                           max_box=max_box, 
                           weight_seq=weight_seq)
    }
    
    # randomly permute and decrease a random portfolio element if the sum of
    # the weights is greater than max_sum
    # set counter to 1 for decrease loop
    #     i <- 1
    #     while (sum(tmp_w) > max_sum & i <= length(tmp_w)) {
    #       print("max_sum violation loop")
    #       
    #       cur_index <- random_index[i]
    #       cur_val <- tmp_w[cur_index]
    #       tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min_box[cur_index])]
    #       n_tmp_seq <- length(tmp_seq)
    #       if(n_tmp_seq > 1) {
    #         # randomly sample one of the smaller weights
    #         tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    #       } else {
    #         if(n_tmp_seq == 1){
    #           tmp_w[cur_index] <- tmp_seq
    #         }
    #       }
    #       i <- i + 1 # increment our counter
    #     } # end decrease loop
    
    # max_sum violation
    if(max_sum_fail(tmp_w, max_sum)){
      tmp_w <- rp_decrease(weights=tmp_w, 
                           max_sum=max_sum, 
                           min_box=min_box, 
                           weight_seq=weight_seq)
    }
    
    # set counter to 1 for leverage violation loop
    #     i <- 1
    #     while (sum(abs(tmp_w)) > leverage & i <= length(tmp_w)) {
    #       print("leverage violation loop")
    #       # randomly permute and increae decrease a random portfolio element
    #       # according to leverage exposure
    #       cur_index <- random_index[i]
    #       cur_val <- tmp_w[cur_index]
    #       
    #       # check the sign of the current value
    #       if(cur_val < 0){
    #         # if the current value is negative, we want to increase to lower 
    #         # sum(abs(weights)) while respecting uppper bound box constraint
    #         tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max_box[cur_index])]
    #       } else if(cur_val > 0){
    #         # if the current value is positive, we want to decrease to lower 
    #         # sum(abs(weights)) while respecting lower bound box constraint
    #         tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min_box[cur_index])]
    #       }
    #       n_tmp_seq <- length(tmp_seq)
    #       if(n_tmp_seq > 1) {
    #         # randomly sample one of the weights
    #         tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    #       } else {
    #         if(n_tmp_seq == 1){
    #           tmp_w[cur_index] <- tmp_seq
    #         }
    #       }
    #       i <- i + 1 # increment our counter
    #     } # end leverage violation loop
    
    # leverage violation
    if(leverage_fail(tmp_w, leverage)){
      tmp_w <- rp_decrease_leverage(weights=tmp_w, 
                                    max_box=max_box, 
                                    min_box=min_box, 
                                    leverage=leverage, 
                                    weight_seq=weight_seq)
    }
    
    # set counter to 1 for position limit violation loop
    #     i <- 1
    #     while (((sum(abs(tmp_w) > tolerance) > max_pos) |
    #               (sum(tmp_w >= 0) > max_pos_long) | 
    #               (sum(tmp_w >= 0) > max_pos_long)) & 
    #              i <= length(tmp_w)) {
    #       print("position limit violation loop")
    #       
    #       cur_index <- random_index[i]
    #       cur_val <- tmp_w[cur_index]
    #       
    #       # Check if max_pos_long is violated
    #       # If max_pos_long is violated, we we grab a positive weight and set it
    #       # to be between min_box and 0
    #       if(sum(tmp_w > tolerance) > max_pos_long){
    #         if(cur_val > tolerance){
    #           # subset such that min_box_i <= weight_i <= 0
    #           tmp_seq <- weight_seq[(weight_seq <= 0) & (weight_seq >= min_box[cur_index])]
    #         }
    #         n_tmp_seq <- length(tmp_seq)
    #         if(n_tmp_seq > 1){
    #           tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    #         } else if(n_tmp_seq == 1){
    #           tmp_w[cur_index] <- tmp_seq
    #         }
    #       } # end max_pos_long violation loop
    #       
    #       # Check if max_pos_short is violated
    #       # If max_pos_short is violated, we grab a negative weight and set it
    #       # to be between 0 and max_box
    #       if(sum(tmp_w < tolerance) > max_pos_short){
    #         if(cur_val < tolerance){
    #           # subset such that 0 <= weight_i <= max_box_i
    #           tmp_seq <- weight_seq[(weight_seq >= 0) & (weight_seq <= max_box[cur_index])]
    #         }
    #         n_tmp_seq <- length(tmp_seq)
    #         if(n_tmp_seq > 1){
    #           tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    #         } else if(n_tmp_seq == 1){
    #           tmp_w[cur_index] <- tmp_seq
    #         }
    #       } # end max_pos_short violation loop
    #       
    #       i <- i + 1 # increment our counter
    #     } # end position limit violation loop
    
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
        
        # treat this as if min_sum were violated
        if(sum(tmp_group_w) < cLO[j]){
          tmp_w[j_idx] <- rp_increase(weights=tmp_group_w, 
                                      min_sum=cLO[j], 
                                      max_box=max_box[j_idx], 
                                      weight_seq=weight_seq)
        }
        
        # treat this as if max_sum were violated
        if(sum(tmp_group_w) > cUP[j]){
          tmp_w[j_idx] <-  rp_decrease(weights=tmp_group_w, 
                                       max_sum=cUP[j], 
                                       min_box=min_box[j_idx], 
                                       weight_seq=weight_seq)
        }
      }
    } # end group violation loop
    
    cat("weights:", tmp_w, "\n")
    #cat("sum(weights):", sum(tmp_w), "\n")
    #cat("sum(abs(weights)):", sum(abs(tmp_w)), "\n")
  } # end final walk towards the edges
  portfolio <- tmp_w
  
  colnames(portfolio) <- colnames(weights)
  
  # checks for infeasible portfolio
  # Stop execution and return an error if an infeasible portfolio is created
  # This will be useful in fn_map so that we can catch the error and take
  # action (try again with more permutations, relax constraints, different
  # method to normalize, etc.)
  if (sum(portfolio) < min_sum | sum(portfolio) > max_sum){
    portfolio <- weights
    stop("Infeasible portfolio created, perhaps increase max_permutations and/or adjust your parameters.")
  }
  return(portfolio)
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
    n_tmp_seq <- length(tmp_seq)
    if(n_tmp_seq > 1) {
      # randomly sample one of the weights
      tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
    } else if(n_tmp_seq == 1){
      tmp_w[cur_index] <- tmp_seq
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

# grp_random_index <- sample(1:length(tmp_group_w), length(tmp_group_w))
# k <- 1
# while (sum(tmp_group_w) < cLO[j] & k <= length(tmp_group_w)) {
#   print("group cLO violation loop")
#   
#   cur_index <- grp_random_index[k]
#   cur_val <- tmp_group_w[cur_index]
#   tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max_box[cur_index])]
#   n_tmp_seq <- length(tmp_seq)
#   if(n_tmp_seq > 1){
#     # randomly sample one of the larger weights
#     tmp_group_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
#   } else {
#     if(n_tmp_seq == 1){
#       tmp_group_w[cur_index] <- tmp_seq
#     }
#   }
#   i <- i + 1 # increment our counter
# } # end increase loop
