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
constraint_fnMap <- function(weights, portfolio) {
  
  if (!is.portfolio(portfolio)) {
    stop("Portfolio passed in is not of class portfolio")
  }
  
  for(constraint in portfolio$constraints) {
    # Check for enabled constraints
    if(constraint$enabled){
      
      ## box constraint
      if(inherits(constraint, "box_constraint")){
        # TODO
      } # box constraint
      
      ## weight_sum constraint
      if(inherits(constraint, "weight_sum_constraint")){
        min_sum <- constraint$min_sum
        max_sum <- constraint$max_sum
        print(min_sum)
        print(max_sum)
        # normalize to max_sum
        if(sum(weights) > max_sum) { weights <- (max_sum / sum(weights)) * weights }
        # normalize to min_sum
        if(sum(weights) < min_sum) { weights <- (min_sum / sum(weights)) * weights }
      } # weight_sum constraint
      
      ## group constraint
      if(inherits(constraint, "group_constraint")){
        groups <- constraint$groups
        cLO <- constraint$cLO
        cUP <- constraint$cUP
        print(groups)
        print(cLO)
        print(cUP)
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
      } # group constraint
      
      # Turnover constraints
      # TODO
      
      # Diversification constraints
      # TODO
    }
  }
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
# (w <- constraint_fnMap(weights, pspec))
# sum(w)
