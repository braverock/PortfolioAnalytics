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

# functions to build portfolios for use by the optimizer

# this code may be made obsolete by the advanced (non-linear, MIP) fPortfolio or roi optimizers, but for now, they are beta code at best

# requireNamespace(LSPM) # for the un-exported .nPri functions
# generate all feasible portfolios
#LSPM:::.nPri(n=13,r=45,i=n^r,replace=TRUE)
# not likely to actually BE feasible for any portfolio of real size, but I'll write the grid generator anyway that will generate all the permutations, and kick out only the feasible portfolios


# random portfolios


#' create a sequence of possible weights for random or brute force portfolios
#' 
#' This function creates the sequence of min<->max weights for use by
#' random or brute force optimization engines.
#' 
#' The sequence created is not constrained by asset. 
#' 
#' @param min minimum value of the sequence
#' @param max maximum value of the sequence
#' @param by number to increment the sequence by
#' @param rounding integrer how many decimals should we round to
#' @author Peter Carl, Brian G. Peterson
#' @seealso \code{\link{constraint}}, \code{\link{objective}}
#' @export
generatesequence <- function (min=.01, max=1, by=min/max, rounding=3 )
{ 
  # this creates the sequence of possible weights, not constrained by asset
  ret <- seq(from = round(min,rounding), to = round(max,rounding), by = by)
  return(ret)
}

#' Random portfolio sample method
#' 
#' This function generates random permutations of a portfolio seed meeting 
#' leverage and box constraints. The final step is to run \code{\link{fn_map}}
#' on the random portfolio weights to transform the weights so they satisfy
#' other constraints such as group or position limit constraints. This is the 
#' 'sample' method for random portfolios and is based on an idea by Pat Burns.
#' 
#' @param rpconstraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @param rounding integer how many decimals should we round to
#' @return named weights vector
#' @author Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)
#' @export
randomize_portfolio_v1 <- function (rpconstraints, max_permutations=200, rounding=3)

{ # @author: Peter Carl, Brian Peterson (based on an idea by Pat Burns)
  # generate random permutations of a portfolio seed meeting your constraints on the weights of each asset
  # set the portfolio to the seed
  seed=rpconstraints$assets
  nassets= length(seed)
  min_mult=rpconstraints$min_mult
  if(is.null(min_mult)) min_mult= rep(-Inf,nassets)
  max_mult=rpconstraints$max_mult
  if(is.null(max_mult)) max_mult= rep(Inf,nassets)
  min_sum =rpconstraints$min_sum
  max_sum =rpconstraints$max_sum
  weight_seq=rpconstraints$weight_seq
  portfolio=as.vector(seed)
  max     = rpconstraints$max
  min     = rpconstraints$min 
  rownames(portfolio)<-NULL
  weight_seq=as.vector(weight_seq)
  # initialize our loop
  permutations=1

    # create a temporary portfolio so we don't return a non-feasible portfolio
    tportfolio=portfolio
    # first randomly permute each element of the temporary portfolio
    random_index <- sample(1:length(tportfolio),length(tportfolio))
    for (i in 1:length(tportfolio)) {
       cur_index<-random_index[i]
       cur_val <- tportfolio[cur_index]
       # randomly permute a random portfolio element
       tportfolio[cur_index]<-sample(weight_seq[(weight_seq>=cur_val*min_mult[cur_index]) & (weight_seq<=cur_val*max_mult[cur_index]) & (weight_seq<=max[cur_index]) & (weight_seq>=min[cur_index])],1)
    }
      
  #while portfolio is outside min/max sum and we have not reached max_permutations
  while ((sum(tportfolio)<=min_sum | sum(tportfolio)>=max_sum) & permutations<=max_permutations) {
        permutations=permutations+1
        # check our box constraints on total portfolio weight
        # reduce(increase) total portfolio size till you get a match
        # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
        # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
        # 3> check and repeat
        random_index <- sample(1:length(tportfolio), length(tportfolio))
        i = 1
        while (sum(tportfolio)<=min_sum & i<=length(tportfolio)) {
          # randomly permute and increase a random portfolio element
          cur_index<-random_index[i]
          cur_val <- tportfolio[cur_index]
            if (length(weight_seq[(weight_seq>=cur_val)&(weight_seq<=max[cur_index])])>1)
            {
              # randomly sample one of the larger weights
              tportfolio[cur_index]<-sample(weight_seq[(weight_seq>=cur_val)&(weight_seq<=max[cur_index])],1)
              # print(paste("new val:",tportfolio[cur_index]))
            } else {
              if (length(weight_seq[(weight_seq>=cur_val)&(weight_seq<=max[cur_index])]) == 1) {
                tportfolio[cur_index]<-weight_seq[(weight_seq>=cur_val)&(weight_seq<=max[cur_index])]
              }
            }
          i=i+1 # increment our counter
        } # end increase loop
        while (sum(tportfolio)>=max_sum & i<=length(tportfolio)) {
          # randomly permute and decrease a random portfolio element
          cur_index<-random_index[i]
          cur_val <- tportfolio[cur_index]
            if (length(weight_seq[(weight_seq<=cur_val) & (weight_seq>=min[cur_index])] )>1) {
              tportfolio[cur_index]<-sample(weight_seq[(weight_seq<=cur_val) & (weight_seq>=min[cur_index] )],1)
            } else {
              if (length(weight_seq[(weight_seq<=cur_val) & (weight_seq>=min[cur_index])] )==1) {
                tportfolio[cur_index]<-weight_seq[(weight_seq<=cur_val) & (weight_seq>=min[cur_index])]
              }
            }
          i=i+1 # increment our counter
        } # end decrease loop
  } # end final walk towards the edges

  portfolio<-tportfolio

  colnames(portfolio)<-colnames(seed)
  if (sum(portfolio)<=min_sum | sum(tportfolio)>=max_sum){
        portfolio <- seed
        warning("Infeasible portfolio created, defaulting to seed, perhaps increase max_permutations.")
  }
  if(isTRUE(all.equal(seed,portfolio))) {
    if (sum(seed)>=min_sum & sum(seed)<=max_sum) {
      warning("Unable to generate a feasible portfolio different from seed, perhaps adjust your parameters.")
      return(seed)
    } else {
      warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
      return(NULL)
    }
  }
  return(portfolio)
}

#' deprecated random portfolios wrapper until we write a random trades function
#' 
#' 
#' @param ... any other passthru parameters
#' @author bpeterson
#' @export
random_walk_portfolios <-function(...) {
  # wrapper function protect older code for now?
  random_portfolios(...=...)
}

#' generate an arbitary number of constrained random portfolios
#' 
#' repeatedly calls \code{\link{randomize_portfolio}} to generate an 
#' arbitrary number of constrained random portfolios.
#' 
#' @param rpconstraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param \dots any other passthru parameters 
#' @return matrix of random portfolio weights
#' @seealso \code{\link{constraint}}, \code{\link{objective}}, \code{\link{randomize_portfolio}}
#' @author Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)
#' @examples
#' rpconstraint<-constraint_v1(assets=10, 
#'                          min_mult=-Inf, 
#'                          max_mult=Inf, 
#'                          min_sum=.99, 
#'                          max_sum=1.01, 
#'                          min=.01, 
#'                          max=.4, 
#'                          weight_seq=generatesequence())
#'                          
#' rp<- random_portfolios_v1(rpconstraints=rpconstraint,permutations=1000)
#' head(rp)
#' @export
random_portfolios_v1 <- function (rpconstraints,permutations=100,...)
{ # 
  # this function generates a series of portfolios that are a "random walk" from the current portfolio
  seed=rpconstraints$assets
  result <- matrix(nrow=permutations, ncol=length(seed))
  result[1,]<-seed
  result[2,]<-rep(1/length(seed),length(seed))
#  rownames(result)[1]<-"seed.portfolio"
#  rownames(result)[2]<-"equal.weight"
  i <- 3
  while (i<=permutations) {
    result[i,] <- as.matrix(randomize_portfolio_v1(rpconstraints=rpconstraints, ...))
    if(i==permutations) {
      result = unique(result)
      i = nrow(result)
      result = rbind(result, matrix(nrow=(permutations-i),ncol=length(seed)))
    }
    i<-i+1
  }
  colnames(result)<-names(seed)
  return(result)
}

#' version 2 generate random permutations of a portfolio seed meeting your constraints on the weights of each asset
#' 
#' @param portfolio an object of type "portfolio" specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @return named weighting vector
#' @author Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)
#' @aliases randomize_portfolio randomize_portfolio_v2
#' @rdname randomize_portfolio
#' @export randomize_portfolio
#' @export randomize_portfolio_v2
randomize_portfolio <- randomize_portfolio_v2 <- function (portfolio, max_permutations=200) { 
  # generate random permutations of a portfolio seed meeting your constraints on the weights of each asset
  # set the portfolio to the seed
  seed <- portfolio$assets
  nassets <- length(seed)
  
  # get the constraints from the portfolio object
  constraints <- get_constraints(portfolio)
  
  min_mult <- constraints$min_mult
  if(is.null(min_mult)) min_mult <- rep(-Inf,nassets)
  max_mult <- constraints$max_mult
  if(is.null(max_mult)) max_mult <- rep(Inf,nassets)
  min_sum  <- constraints$min_sum
  max_sum  <- constraints$max_sum
  # randomize_portfolio will rarely find a feasible portfolio if there is not some 
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
  
  # box constraints
  max <- constraints$max
  min <- constraints$min
  
  # If any of the constraints below do not exist in the constraints object,
  # then they are NULL values which rp_transform can handle in its checks.
  
  # group constraints
  groups <- constraints$groups
  cLO <- constraints$cLO
  cUP <- constraints$cUP
  group_pos <- constraints$group_pos
  
  # position limit constraints
  max_pos <- constraints$max_pos
  max_pos_long <- constraints$max_pos_long
  max_pos_short <- constraints$max_pos_short
  
  # leverage constraint
  leverage <- constraints$leverage
  
  # initial portfolio
  iportfolio <- as.vector(seed)
  rownames(iportfolio) <- NULL
  
  # initialize our loop
  permutations <- 1
  
  # create a temporary portfolio so we don't return a non-feasible portfolio
  tportfolio <- iportfolio
  # first randomly permute each element of the temporary portfolio
  random_index <- sample(1:length(tportfolio), length(tportfolio))
  for (i in 1:length(tportfolio)) {
    cur_index <- random_index[i]
    cur_val <- tportfolio[cur_index]
    # randomly permute a random portfolio element
    tportfolio[cur_index] <- sample(weight_seq[(weight_seq >= cur_val * min_mult[cur_index]) & (weight_seq <= cur_val * max_mult[cur_index]) & (weight_seq <= max[cur_index]) & (weight_seq >= min[cur_index])], 1)
  }
  
  # random portfolios algorithm designed to handle multiple constraint types
  fportfolio <- rp_transform(w=tportfolio, 
                             min_sum=min_sum, 
                             max_sum=max_sum, 
                             min_box=min, 
                             max_box=max, 
                             groups=groups, 
                             cLO=cLO, 
                             cUP=cUP, 
                             max_pos=max_pos, 
                             group_pos=group_pos, 
                             max_pos_long=max_pos_long, 
                             max_pos_short=max_pos_short, 
                             leverage=leverage, 
                             weight_seq=weight_seq, 
                             max_permutations=max_permutations)
  
#   #while portfolio is outside min/max sum and we have not reached max_permutations
#   while ((sum(tportfolio) <= min_sum | sum(tportfolio) >= max_sum) & permutations <= max_permutations) {
#     permutations <- permutations+1
#     # check our box constraints on total portfolio weight
#     # reduce(increase) total portfolio size till you get a match
#     # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
#     # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
#     # 3> check and repeat
#     random_index <- sample(1:length(tportfolio), length(tportfolio))
#     i <- 1
#     while (sum(tportfolio) <= min_sum & i <= length(tportfolio)) {
#       # randomly permute and increase a random portfolio element
#       cur_index <- random_index[i]
#       cur_val <- tportfolio[cur_index]
#       tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
#       n_tmp_seq <- length(tmp_seq)
#       if(n_tmp_seq > 1){
#         # randomly sample one of the larger weights
#         tportfolio[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
#         # print(paste("new val:",tportfolio[cur_index]))
#       } else {
#         if(n_tmp_seq == 1){
#           tportfolio[cur_index] <- tmp_seq
#         }
#       }
#       i <- i + 1 # increment our counter
#     } # end increase loop
#     while (sum(tportfolio) >= max_sum & i <= length(tportfolio)) {
#       # randomly permute and decrease a random portfolio element
#       cur_index <- random_index[i]
#       cur_val <- tportfolio[cur_index]
#       tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index])]
#       n_tmp_seq <- length(tmp_seq)
#       if(n_tmp_seq > 1) {
#         # randomly sample one of the smaller weights
#         tportfolio[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
#       } else {
#         if(n_tmp_seq == 1){
#           tportfolio[cur_index] <- tmp_seq
#         }
#       }
#       i <- i + 1 # increment our counter
#     } # end decrease loop
#   } # end final walk towards the edges
#   # final portfolio
#   fportfolio <- fn_map(weights=tportfolio, portfolio=portfolio, relax=FALSE)$weights
  
  colnames(fportfolio) <- colnames(seed)
  if (sum(fportfolio) < min_sum | sum(fportfolio) > max_sum){
    fportfolio <- seed
    warning("Infeasible portfolio created, defaulting to seed, perhaps increase max_permutations.")
  }
  if(isTRUE(all.equal(seed, fportfolio))) {
    if (sum(seed) >= min_sum & sum(seed) <= max_sum) {
      warning("Unable to generate a feasible portfolio different from seed, perhaps adjust your parameters.")
      return(seed)
    } else {
      warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
      return(NULL)
    }
  }
  return(fportfolio)
}

#' version 2 generate an arbitary number of constrained random portfolios
#' 
#' Generate random portfolios using the 'sample', 'simplex', or 'grid' method. 
#' See details.
#' 
#' @details
#' Random portfolios can be generate using one of three methods.
#' \itemize{
#'   \item{sample: }{The 'sample' method to generate random portfolios is based
#'   on an idea pioneerd by Pat Burns. This is the most flexible method, but 
#'   also the slowest, and can generate portfolios to satisfy leverage, box, 
#'   group, position limit, and leverage exposure constraints.}
#'   \item{simplex: }{The 'simplex' method to generate random portfolios is 
#'   based on a paper by W. T. Shaw. The simplex method is useful to generate 
#'   random portfolios with the full investment constraint, where the sum of the 
#'   weights is equal to 1, and min box constraints. Values for \code{min_sum} 
#'   and \code{max_sum} of the leverage constraint will be ignored, the sum of 
#'   weights will equal 1. All other constraints such as group and position 
#'   limit constraints will be handled by elimination. If the constraints are 
#'   very restrictive, this may result in very few feasible portfolios remaining.}
#'   \item{grid: }{The 'grid' method to generate random portfolios is based on
#'   the \code{gridSearch} function in package 'NMOF'. The grid search method 
#'   only satisfies the \code{min} and \code{max} box constraints. The 
#'   \code{min_sum} and \code{max_sum} leverage constraints will likely be 
#'   violated and the weights in the random portfolios should be normalized. 
#'   Normalization may cause the box constraints to be violated and will be 
#'   penalized in \code{constrained_objective}.}
#' }
#' 
#' The constraint types checked are leverage, box, group, position limit, and 
#' leverage exposure. Any
#' portfolio that does not satisfy all these constraints will be eliminated. This
#' function is particularly sensitive to \code{min_sum} and \code{max_sum} 
#' leverage constraints. For the sample method, there should be some 
#' "wiggle room" between \code{min_sum} and \code{max_sum} in order to generate 
#' a sufficient number of feasible portfolios. For example, \code{min_sum=0.99} 
#' and \code{max_sum=1.01} is recommended instead of \code{min_sum=1} 
#' and \code{max_sum=1}. If \code{min_sum=1} and \code{max_sum=1}, the number of
#' feasible portfolios may be 1/3 or less depending on the other constraints.
#' 
#' 
#' @param portfolio an object of class 'portfolio' specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param \dots any other passthru parameters
#' @param rp_method method to generate random portfolios. Currently "sample", "simplex", or "grid". See Details.
#' @param eliminate TRUE/FALSE, eliminate portfolios that do not satisfy constraints
#' @return matrix of random portfolio weights
#' @seealso \code{\link{portfolio.spec}}, 
#' \code{\link{objective}}, 
#' \code{\link{rp_sample}},
#' \code{\link{rp_simplex}},
#' \code{\link{rp_grid}}
#' @author Peter Carl, Brian G. Peterson, Ross Bennett
#' @aliases random_portfolios random_portfolios_v2 
#' @rdname random_portfolios
#' @export random_portfolios
#' @export random_portfolios_v2
random_portfolios <- random_portfolios_v2 <- function( portfolio, permutations=100, rp_method="sample", eliminate=TRUE, ...){
  if(hasArg(fev)) fev=match.call(expand.dots=TRUE)$fev else fev=0:5
  if(hasArg(normalize)) normalize=match.call(expand.dots=TRUE)$normalize else normalize=TRUE
  switch(rp_method,
         sample = {rp <- rp_sample(portfolio, permutations)
                   },
         simplex = {rp <- rp_simplex(portfolio, permutations, fev)
                    },
         grid = {rp <- rp_grid(portfolio, permutations, normalize)
         }
  )
  if(eliminate){
    # eliminate portfolios that do not satisfy constraints
    check <- vector("numeric", nrow(rp))
    for(i in 1:nrow(rp)){
      check[i] <- check_constraints(weights=rp[i,], portfolio=portfolio)
    }
    # We probably don't need or want to do this part in parallel. It could
    # also interfere with optimize.portfolio.parallel since this function 
    # will likely be called. Not sure how foreach handles nested loops 
    # in parallel so it is best to avoid that altogether.
    #stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
    #check <- foreach(i=1:nrow(rp), .combine=c) %dopar% {
    #  # check_constraint returns TRUE if all constraints are satisfied
    #  check_constraints(weights=rp[i,], portfolio=portfolio)
    #}
    rp <- rp[which(check==TRUE),]
  }
  return(rp)
}

#' Generate random portfolios using the sample method
#' 
#' This function generates random portfolios based on an idea by Pat Burns.
#' 
#' @details
#' The 'sample' method to generate random portfolios is based
#' on an idea pioneerd by Pat Burns. This is the most flexible method, but also
#' the slowest, and can generate portfolios to satisfy leverage, box, group, 
#' and position limit constraints.
#' @param portfolio an object of type "portfolio" specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @return a matrix of random portfolio weights
#' @export
rp_sample <- function(portfolio, permutations, max_permutations=200){
  # this function generates a series of portfolios that are a "random walk" from the current portfolio
  seed <- portfolio$assets
  result <- matrix(nrow=permutations, ncol=length(seed))
  result[1,] <- seed
  result[2,] <- rep(1/length(seed),length(seed))
  # rownames(result)[1]<-"seed.portfolio"
  # rownames(result)[2]<-"equal.weight"
  for(i in 3:permutations) {
    #result[i,] <- as.matrix(randomize_portfolio_v2(portfolio=portfolio, ...))
    result[i,] <- randomize_portfolio_v2(portfolio=portfolio, max_permutations=max_permutations)
  }
  result <- unique(result)
  # i <- nrow(result)
  # result <- rbind(result, matrix(nrow=(permutations-i), ncol=length(seed)))
  colnames(result) <- names(seed)
  return(result)
}

#' Generate random portfolios using the simplex method
#' 
#' This function generates random portfolios based on the method outlined in the
#' Shaw paper. Need to add reference.
#' 
#' @details
#' The simplex method is useful to generate random portfolios with the full
#' investment constraint where the sum of the weights is equal to 1 and min 
#' box constraints with no upper bound on max constraints. Values for min_sum 
#' and max_sum will be ignored, the sum of weights will equal 1. All other 
#' constraints such as group and position limit constraints will be handled by 
#' elimination. If the constraints are very restrictive, this may result in 
#' very few feasible portfolios remaining. 
#' 
#' The random portfolios are created by first generating a set of uniform 
#' random numbers.
#' \deqn{U \sim [0, 1]}
#' The portfolio weights are then transformed to satisfy the min of the
#' box constraints.
#' \deqn{w_{i} = min_{i} + (1 - \sum_{j=1}^{N} min_{j}) \frac{log(U_{i}^{q}}{\sum_{k=1}^{N}log(U_{k}^{q}}}
#' 
#' \code{fev} controls the Face-Edge-Vertex (FEV) biasing where \deqn{q=2^{fev}}
#' As \code{q} approaches infinity, the set of weights will be concentrated in a 
#' single asset. To sample the interior and exterior, \code{fev} can be passed 
#' in as a vector. The number of portfolios, \code{permutations}, and the 
#' length of \code{fev} affect how the random portfolios are generated. For 
#' example, if \code{permutations=10000} and \code{fev=0:4}, 2000 portfolios will
#' be generated for each value of \code{fev}.
#' 
#' @param portfolio an object of class 'portfolio' specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param fev scalar or vector for FEV biasing
#' @return a matrix of random portfolio weights
#' @export
rp_simplex <- function(portfolio, permutations, fev=0:5){
  # get the assets from the portfolio
  assets <- portfolio$assets
  nassets <- length(assets)
  
  # get the constraints
  # the simplex method for generating random portfolios requires that the sum of weights is equal to 1
  # ignore the min_sum and max_sum constraints
  constraints <- get_constraints(portfolio)
  L <- constraints$min
  
  # number of portfolios for each fev to generate
  k <- ceiling(permutations / length(fev))
  
  # generate uniform[0, 1] random numbers
  U <- runif(n=k*nassets, 0, 1)
  Umat <- matrix(data=U, nrow=k, ncol=nassets)
  
  # do the transformation to the set of weights to satisfy lower bounds
  stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
  j <- 1
  i <- 1
  out <- foreach::foreach(j = 1:length(fev), .combine=c) %:% foreach::foreach(i=1:nrow(Umat)) %dopar% {
    q <- 2^fev[j]
    tmp <- L + (1 - sum(L)) * log(Umat[i,])^q / sum(log(Umat[i,])^q)
    tmp
  }
  # the foreach loop returns a list of each random portfolio
  out <- do.call(rbind, out)
  return(out)
}

#' Generate random portfolios based on grid search method
#' 
#' This function generates random portfolios based on the \code{gridSearch} 
#' function from the 'NMOF' package.
#' 
#' @details
#' The number of levels is calculated based on permutations and number of assets.
#' The number of levels must be an integer and may not result in the exact number
#' of permutations. We round up to the nearest integer for the levels so the
#' number of portfolios generated will be greater than or equal to permutations.
#' 
#' The grid search method only satisfies the \code{min} and \code{max} box 
#' constraints. The \code{min_sum} and \code{max_sum} leverage constraints will
#' likely be violated and the weights in the random portfolios should be 
#' normalized. Normalization may cause the box constraints to be violated and
#' will be penalized in \code{constrained_objective}.
#' 
#' @param portfolio an object of class 'portfolio' specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param normalize TRUE/FALSE to normalize the weghts to satisfy min_sum or max_sum
#' @return matrix of random portfolio weights
#' @export
rp_grid <- function(portfolio, permutations=2000, normalize=TRUE){
  
  # get the constraints from the portfolio
  constraints <- get_constraints(portfolio)
  
  # box constraints to generate the grid
  min <- constraints$min
  max <- constraints$max
  
  # number of parameters and length.out levels to generate
  npar <- length(min)
  n <- ceiling(exp(log(permutations) / npar))
  
  levels <- vector("list", length = length(min))
  for (i in seq_len(npar)){
    levels[[i]] <- seq(min[[i]], max[[i]], length.out = max(n, 2L))
  }
  np <- length(levels)
  res <- vector("list", np)
  rep.fac <- 1L
  nl <- sapply(levels, length)
  nlp <- prod(nl)
  
  # create the grid
  for (i in seq_len(np)) {
    x <- levels[[i]]
    nx <- length(x)
    nlp <- nlp/nx
    res[[i]] <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), nlp)]
    rep.fac <- rep.fac * nx
  }
  
  # create the random portfolios from the grid
  nlp <- prod(nl)
  lstLevels <- vector("list", length = nlp)
  for (r in seq_len(nlp)) {
    lstLevels[[r]] <- sapply(res, `[[`, r)
  }
  # lstLevels is a list of random portfolios, rbind into a matrix
  rp <- do.call(rbind, lstLevels)
  
  # min_sum and max_sum will likely be violated
  # Normalization will likely cause min and max to be violated. This can be
  # handled by the penalty in constrained_objective.
  if(normalize){
    normalize_weights <- function(weights){
      # normalize results if necessary
      if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
        # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
        # we'll normalize the weights passed in to whichever boundary condition has been violated
        # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
        # might violate your constraints, so you'd need to renormalize them after optimizing
        # we'll create functions for that so the user is less likely to mess it up.
        
        # NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
        # In Kris' original function, this was manifested as a full investment constraint
        if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
          max_sum=constraints$max_sum
          if(sum(weights)>max_sum) { weights<-(max_sum/sum(weights))*weights } # normalize to max_sum
        }
        
        if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
          min_sum=constraints$min_sum
          if(sum(weights)<min_sum) { weights<-(min_sum/sum(weights))*weights } # normalize to min_sum
        }
        
      } # end min_sum and max_sum normalization
      return(weights)
    }
    
    stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
    out <- foreach::foreach(i=1:nrow(rp)) %dopar% {
      tmp <- normalize_weights(weights=rp[i,])
      tmp
    }
    out <- do.call(rbind, out)
    out <- na.omit(out)
  }
  if(normalize) return(out) else return(rp)
}

# function to generate a set of random portfolios for each portfolio and return the superset
# this is primarily for use in optimize.portfolio.rebalancing
rp.regime.portfolios <- function(regime, permutations=100, rp_method="sample", eliminate=TRUE, ...){
  if(!inherits(regime, "regime.portfolios")) stop("regime must be an object of class 'regime.portfolios'")
  portf <- regime$portfolio.list
  nportf <- length(portf)
  rp.list <- vector("list", nportf)
  for(i in 1:nportf){
    rp.list[[i]] <- random_portfolios(portf[[i]], permutations=permutations, rp_method=rp_method, eliminate=eliminate, ...=...)
  }
  # rbind the list of matrices together and remove any duplicates
  out <- unique(do.call("rbind", rp.list))
  return(out)
}

# EXAMPLE: start_t<- Sys.time(); x=random_walk_portfolios(rep(1/5,5), generatesequence(min=0.01, max=0.30, by=0.01), max_permutations=500, permutations=5000, min_sum=.99, max_sum=1.01); end_t<-Sys.time(); end_t-start_t;
# > nrow(unique(x))
# [1] 4906
# > which(rowSums(x)<.99 | rowSums(x)>1.01)
# integer(0)

# start_t <- Sys.time(); s<-foreach(seed=iter(weights, by='row'),.combine=rbind) %dopar% random_walk_portfolios(seed,xseq,permutations=10000); end_t <- Sys.time(); save.image(); start_t-end_t;

# TODO: write a function for random trades that only makes n trades and increases/decreases other elements to compensate.
