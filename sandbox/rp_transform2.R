

rp_transform2 <- function(weights,
                          min_sum=NULL,
                          max_sum=NULL,
                          min_box=NULL,
                          max_box=NULL,
                          max_pos=NULL,
                          leverage=NULL,
                          max_permutations=200){
  tmp_w <- weights
  
  # Set some reasonable default values
  if(is.null(min_sum)) min_sum <- 0.99
  if(is.null(max_sum)) max_sum <- 1.01
  if(is.null(min_box)) min_box <- rep(-Inf, length(tmp_w))
  if(is.null(max_box)) max_box <- rep(Inf, length(tmp_w))
  if(is.null(max_pos)) max_pos <- length(tmp_w)
  if(is.null(leverage)) leverage <- Inf
  
  # Generate a weight sequence, we should check for portfolio$weight_seq
  weight_seq <- generatesequence(min=min(min_box), max=max(max_box), by=0.002)
  
  # tolerance for "non-zero" definition
  tolerance <- .Machine$double.eps^0.5
  
  # initialize the outer while loop
  permutations <- 1
  
  # while we have not reached max_permutations and the following constraints 
  # are violated:
  # - min_sum/max_sum
  # - leverage
  # - max_pos
  
  
  
  # Do we want to check all constraints in here?
  # Box constraints should be satisfied by construction so we should not need 
  # to check those here
  while ((  (sum(tmp_w) < min_sum) |
            (sum(tmp_w) > max_sum) |
            (sum(abs(tmp_w)) > leverage) |
            (sum(abs(tmp_w) > tolerance)) | 
              (sum(abs(tmp_w) > tolerance) > max_pos) ) & 
           (permutations < max_permutations)) {
    
    permutations <- permutations+1
    # check our box constraints on total portfolio weight
    # reduce(increase) total portfolio size till you get a match
    # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
    # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
    # 3> check and repeat
    
    # reset the random index 
    random_index <- sample(1:length(tmp_w), length(tmp_w))
    
    # randomly permute and increase a random portfolio element if the sum of
    # the weights is less than min_sum
    # set counter to 1 for increase loop
    i <- 1
    while (sum(tmp_w) <= min_sum & i <= length(tmp_w)) {
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max_box[cur_index])]
      n_tmp_seq <- length(tmp_seq)
      if(n_tmp_seq > 1){
        # randomly sample one of the larger weights
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
        # print(paste("new val:",tmp_w[cur_index]))
      } else {
        if(n_tmp_seq == 1){
          tmp_w[cur_index] <- tmp_seq
        }
      }
      i <- i + 1 # increment our counter
    } # end increase loop
    
    # randomly permute and decrease a random portfolio element if the sum of
    # the weights is greater than max_sum
    # set counter to 1 for decrease loop
    i <- 1
    while (sum(tmp_w) >= max_sum & i <= length(tmp_w)) {
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min_box[cur_index])]
      n_tmp_seq <- length(tmp_seq)
      if(n_tmp_seq > 1) {
        # randomly sample one of the smaller weights
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
      } else {
        if(n_tmp_seq == 1){
          tmp_w[cur_index] <- tmp_seq
        }
      }
      i <- i + 1 # increment our counter
    } # end decrease loop
    
    # set counter to 1 for leverage violation loop
    i <- 1
    while (sum(abs(tmp_w)) >= leverage & i <= length(tmp_w)) {
      # randomly permute and increae decrease a random portfolio element
      # according to leverage exposure
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      
      # check the sign of the current value
      if(cur_val < 0){
        # if the current value is negative, we want to increase to lower 
        # sum(abs(weights)) while respecting uppper bound box constraint
        tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max_box[cur_index])]
      } else if(cur_val > 0){
        # if the current value is positive, we want to decrease to lower 
        # sum(abs(weights)) while respecting lower bound box constraint
        tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min_box[cur_index])]
      }
      n_tmp_seq <- length(tmp_seq)
      if(n_tmp_seq > 1) {
        # randomly sample one of the weights
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
      } else {
        if(n_tmp_seq == 1){
          tmp_w[cur_index] <- tmp_seq
        }
      }
      i <- i + 1 # increment our counter
    } # end leverage violation loop
    
    # set counter to 1 for position limit violation loop
    i <- 1
    while (sum(abs(tmp_w) > tolerance) >= max_pos & i <= length(tmp_w)) {
      # TODO
      # check for positive weights for max_pos_long
      # check for negative weights for max_pos_short
      
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      
      # Can I just force a weight to 0?
      tmp_w[cur_index] <- 0
      
      i <- i + 1 # increment our counter
    } # end position limit violation loop
    
    cat("permutations:", permutations, "\n")
    cat("weights:", tmp_w, "\n")
    cat("sum(weights):", sum(tmp_w), "\n")
    cat("sum(abs(weights)):", sum(abs(tmp_w)), "\n")
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