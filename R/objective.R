###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2009 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

constrained_objective <- function(w, constraints, R, mu, sigma, M3, M4)
{ #@author: Brian Peterson, Peter Carl

    # function to calculate a numeric return value for a portfolio based on a set of constraints,
    # we'll try to make as few assumptions as possible, and only run objectives that are required by the user

    penalty = 1e4;
    N = length(w);
    
    # check for valid constraints
    if (!is.constraint(constraints)) {stop("constraints passed in are not of class constraint")}

    # check that the constraints and the weighting vector have the same length
    if (N != length(constraints$assets)){
      warning("length of constraints asset list and weights vector do not match, results may be bogus")
    }

    out = -sum( w*mu ) #needs to be maximized

    if(!is.null(contraints$min_sum) | !is.null(constraints$max_sum)){
      # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
      # we'll normalize the weights passed in to whichever boundry condition has been violated
      # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
      # might violate your constraints, so you'd need to renormalize them after optimizing
      # we'll create functions for that so the user is less likely to mess it up.
      
      if(is.null(constraints$max_sum)) {
        max_sum=1
      } else {
        max_sum=constraints$max_sum
      }

      if(is.null(constraints$min_sum)) {
        min_sum=1
      } else {
        min_sum=constraints$min_sum
      }

      if(sum(w)>max_sum) { w<-(max_sum/sum(w))*w } # normalize to max_sum

      if(sum(w)<min_sum) { w<-(min_sum/sum(w))*w } # normalize to min_sum
    } # end min_sum and max_sum normalization

    # penalize weights outside my constraints
    out = out + sum(w[which(w>upper[1:N])]-upper[which(w>upper[1:N])])*penalty
    out = out + sum(lower[which(w<lower[1:N])]-w[which(w<lower[1:N])])*penalty

    if(is.null(constraints$objectives)) {
      stop("no objectives specified in constraints")
    } else{
      for (objective in constraints$objectives){
        switch(name,

        ) # end objective switch
      } # end loop over objectives
    } # end objectives processing

    #print(paste("output of objective function",out))

    #return
    return(out)
}

objective<-function(name , enabled=FALSE , ...){
  if(is.null(name)) stop("you must specify an objective name")
  ## now structure and return
  return(structure( list(name = name,
                         enabled = enabled
                        ),
                    class="objective"
                  ) # end structure
  )
}

is.objective <- function( x ) {
  inherits( x, "objective" )
}

portfolio_risk_objective <- function(name, enabled=FALSE, ... , p=.95){
  Objective <- objective(name,enabled)
  if (!is.numeric(p)) stop("your p value must be numeric")
  if (p>1) stop("p must be less than 1")
  if (p<0) stop("p must be greater than zero")
  ## now structure and return
  return(structure( list(name = Objective$name,
                         enabled = Objective$enabled,
                         p = p,
                         if(hasArg(clean)) clean = clean,
                         if(hasArg(method)) method = method,
                         if(hasArg(portfolio_method)) portfolio_method = portfolio_method,
                        ),
                    class=c("portfolio_risk_objective","objective")
                  ) # end structure
  )
}

risk_budget_objective <- function(name, enabled=FALSE, ... ){
  #if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }
}

KB_RBpaper_objective = function( w ){

    #        w = matrix( c( w , 1-sum(w) ) , ncol=1) # assume a cash asset

    # add weight constraint penalty, turn this into configuration from constraints
    # force weights to 1
    w <- (1/sum(w))*w
    N = length(w);
    percrisk = percriskcontrib( w );
    out = -sum( w*mu ) #needs to be maximized
    # add full investment constraint:
    penalty = 1e4;
    #out = out + penalty*( ((1-sum(w))>upper[N]) | ((1-sum(w))<lower[N]) )
    # penalize weights outside my constraints
    out = out + sum(w[which(w>upper[1:N])]-upper[which(w>upper[1:N])])*penalty
    out = out + sum(lower[which(w<lower[1:N])]-w[which(w<lower[1:N])])*penalty

    ##########
    # add portfolio risk constraint
    prw=prisk(w)
    # full penalty for violating risk upper limit
    if(  prw > Riskupper ){ out = out + penalty*( prw - Riskupper) }
    # half penalty for risk lower than target
    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }

    # add risk budget constraint
    out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
    #print(paste("output of objective function",out))

    #return
    return(out)
}
