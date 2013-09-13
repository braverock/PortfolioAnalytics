###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2012 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

# functions to build portfolios for use by the optimizer

# this code may be made obsolete by the advanced (non-linear, MIP) fPortfolio or roi optimizers, but for now, they are beta code at best

# require(LSPM) # for the un-exported .nPri functions
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

#randomize_portfolio <- function (seed, weight_seq, min_mult=-Inf,max_mult=Inf, min_sum=.99, max_sum=1.01, max_permutations=100,rounding=3)
#' generate random permutations of a portfolio seed meeting your constraints on the weights of each asset
#' 
#' @param rpconstraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @param rounding integer how many decimals should we round to
#' @return named weighting vector
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
#' @export
#' @examples
#' rpconstraint<-constraint(assets=10, min_mult=-Inf, max_mult=Inf, min_sum=.99, max_sum=1.01, min=.01, max=.4, weight_seq=generatesequence())
#' rp<- random_portfolios_v1(rpconstraints=rpconstraint,permutations=1000)
#' head(rp)
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
#' @aliases randomize_portfolio
#' @rdname randomize_portfolio
#' @export
randomize_portfolio_v2 <- function (portfolio, max_permutations=200) { 
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
  max <- constraints$max
  min <- constraints$min
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
  
  #while portfolio is outside min/max sum and we have not reached max_permutations
  while ((sum(tportfolio) <= min_sum | sum(tportfolio) >= max_sum) & permutations <= max_permutations) {
    permutations <- permutations+1
    # check our box constraints on total portfolio weight
    # reduce(increase) total portfolio size till you get a match
    # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
    # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
    # 3> check and repeat
    random_index <- sample(1:length(tportfolio), length(tportfolio))
    i <- 1
    while (sum(tportfolio) <= min_sum & i <= length(tportfolio)) {
      # randomly permute and increase a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tportfolio[cur_index]
      if (length(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]) > 1)
      {
        # randomly sample one of the larger weights
        tportfolio[cur_index] <- sample(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])], 1)
        # print(paste("new val:",tportfolio[cur_index]))
      } else {
        if (length(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]) == 1) {
          tportfolio[cur_index] <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
        }
      }
      i <- i + 1 # increment our counter
    } # end increase loop
    while (sum(tportfolio) >= max_sum & i <= length(tportfolio)) {
      # randomly permute and decrease a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tportfolio[cur_index]
      if (length(weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index])] ) > 1) {
        # randomly sample one of the smaller weights
        tportfolio[cur_index] <- sample(weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index] )], 1)
      } else {
        if (length(weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index])] ) == 1) {
          tportfolio[cur_index] <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= min[cur_index])]
        }
      }
      i <- i + 1 # increment our counter
    } # end decrease loop
  } # end final walk towards the edges
  
  # final portfolio
  fportfolio <- fn_map(weights=tportfolio, portfolio=portfolio, relax=FALSE)$weights
  
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
#' repeatedly calls \code{\link{randomize_portfolio}} to generate an 
#' arbitrary number of constrained random portfolios.
#' 
#' @param portfolio an object of type "portfolio" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param \dots any other passthru parameters 
#' @return matrix of random portfolio weights
#' @seealso \code{\link{portfolio.spec}}, \code{\link{objective}}, \code{\link{randomize_portfolio_v2}}
#' @author Peter Carl, Brian G. Peterson, (based on an idea by Pat Burns)
#' @aliases random_portfolios
#' @rdname random_portfolios
#' @export
random_portfolios_v2 <- function( portfolio, permutations=100, ...)
{ # 
  # this function generates a series of portfolios that are a "random walk" from the current portfolio
  seed <- portfolio$assets
  result <- matrix(nrow=permutations, ncol=length(seed))
  result[1,] <- seed
  result[2,] <- rep(1/length(seed),length(seed))
  # rownames(result)[1]<-"seed.portfolio"
  # rownames(result)[2]<-"equal.weight"
  i <- 3
  while (i <= permutations) {
    result[i,] <- as.matrix(randomize_portfolio_v2(portfolio=portfolio, ...))
    if(i == permutations) {
      result <- unique(result)
      i <- nrow(result)
      result <- rbind(result, matrix(nrow=(permutations-i), ncol=length(seed)))
    }
    i <- i + 1
  }
  colnames(result) <- names(seed)
  return(result)
}

# Alias randomize_portfolio_v2 to randomize_portfolio
#' @export
randomize_portfolio <- randomize_portfolio_v2

# Alias random_portfolios_v2 to random_portfolios
#' @export
random_portfolios <- random_portfolios_v2

#' Generate random portfolios using the simplex method
#' 
#' This function generates random portfolios based on the method outlined in the
#' Shaw paper. Need to add reference.
#' 
#' @details
#' The simplex method is useful to generate random portfolios with the full
#' investment constraint where the sum of the weights is equal to 1 and min and
#' max box constraints. All other constraints such as group and position limit 
#' constraints will be handled by elimination. If the constraints are very 
#' restrictive, this may result in very few feasible portfolios remaining. 
#' 
#' The random portfolios are created by first generating a set of uniform 
#' random numbers.
#' \deqn{U \sim [0, 1]}
#' The portfolio weights are then transformed to satisfy the min of the
#' box constraints.
#' \deqn{w_{i} = min_{i} + (1 - \sum_{j=1}^{N} min_{j}) \frac{log(U_{i}^{q}}{\sum_{k=1}^{N}log(U_{k}^{q}}}
#' 
#' \code{p} controls the Face-Edge-Vertex (FEV) biasing where \deqn{q=2^p}. As 
#' \code{q} approaches infinity, the set of weights will be concentrated in a 
#' single asset. To sample the interior and exterior, \code{p} can be passed 
#' in as a vector. The number of portfolios, \code{permutations}, and the 
#' length of \code{p} affect how the random portfolios are generated. For 
#' example if \code{permutations=10000} and \code{p=0:4}, 2000 portfolios will
#' be generated for each value of \code{p}.
#' 
#' @param portfolio an object of type "portfolio" specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param permutations integer: number of unique constrained random portfolios to generate
#' @param p scalar or vector for FEV biasing
#' @param \dots any other passthru parameters
#' @return a matrix of random portfolios
#' @export
rp_simplex <- function(portfolio, permutations, p=0:5, ...){
  # get the assets from the portfolio
  assets <- portfolio$assets
  nassets <- length(assets)
  
  # get the constraints
  # the simplex method for generating random portfolios requires that the sum of weights is equal to 1
  constraints <- get_constraints(portfolio)
  L <- constraints$min
  
  # number of portfolios for each p to generate
  k <- floor(permutations / length(p))
  
  # generate uniform[0, 1] random numbers
  U <- runif(n=k*permutations, 0, 1)
  Umat <- matrix(data=U, nrow=k, ncol=nassets)
  
  # do the transformation to the set of weights to satisfy lower bounds
  stopifnot("package:foreach" %in% search() || require("foreach",quietly = TRUE))
  out <- foreach(j = 1:length(p), .combine=c) %:% foreach(i=1:nrow(Umat)) %dopar% {
    q <- 2^p[j]
    tmp <- L + (1 - sum(L)) * log(Umat[i,])^q / sum(log(Umat[i,])^q)
    tmp
  }
  # the foreach loop returns a list of each random portfolio
  out <- do.call(rbind, out)
  return(out)
}

# EXAMPLE: start_t<- Sys.time(); x=random_walk_portfolios(rep(1/5,5), generatesequence(min=0.01, max=0.30, by=0.01), max_permutations=500, permutations=5000, min_sum=.99, max_sum=1.01); end_t<-Sys.time(); end_t-start_t;
# > nrow(unique(x))
# [1] 4906
# > which(rowSums(x)<.99 | rowSums(x)>1.01)
# integer(0)

# start_t <- Sys.time(); s<-foreach(seed=iter(weights, by='row'),.combine=rbind) %dopar% random_walk_portfolios(seed,xseq,permutations=10000); end_t <- Sys.time(); save.image(); start_t-end_t;

# TODO: write a function for random trades that only makes n trades and increases/decreases other elements to compensate.
