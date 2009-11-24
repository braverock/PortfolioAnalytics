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

#' function to calculate a numeric return value for a portfolio based on a set of constraints
#' 
#' function to calculate a numeric return value for a portfolio based on a set of constraints,
#' we'll try to make as few assumptions as possible, and only run objectives that are required by the user
#' 
#' If the user has passed in either min_sum or max_sum constraints for the portfolio, or both,
#' we'll normalize the weights passed in to whichever boundary condition has been violated
#' NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
#' might violate your constraints, so you'd need to renormalize them after optimizing
#' We aspply the same normalization in \code{\link{optimize.portfolio}} so that the weights you see have been 
#' normalized to min_sum if the generated portfolio is smaller than min_sum or max_sum if the 
#' generated portfolio is larger than max_sum.  
#' This normalization increases the speed of optimization and convergence by several orders of magnitude.
#'  
#' @param R 
#' @param w 
#' @param constraints 
#' @param ... 
#' 
#' @author Peter Carl, Brian G. Peterson
#' @export
constrained_objective <- function(w, R, constraints, ..., trace=FALSE)
{ 
    if (ncol(R)>length(w)) {
        R=R[,1:length(w)]
    }
    if(!hasArg(penalty)) penalty = 1e4
    N = length(w)
    T = nrow(R)

    if(!hasArg(mu))    mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
    if(!hasArg(sigma)) sigma = cov(R);
    if(!hasArg(M3))    M3 = PerformanceAnalytics:::M3.MM(R)
    if(!hasArg(M4))    M4 = PerformanceAnalytics:::M4.MM(R)


    # check for valid constraints
    if (!is.constraint(constraints)) {stop("constraints passed in are not of class constraint")}

    # check that the constraints and the weighting vector have the same length
    if (N != length(constraints$assets)){
      warning("length of constraints asset list and weights vector do not match, results may be bogus")
    }

    # should be take care of by a return objective
    #out = sum( mu*w ) #needs to be maximized

    out=0
    
    if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
      # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
      # we'll normalize the weights passed in to whichever boundary condition has been violated
      # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
      # might violate your constraints, so you'd need to renormalize them after optimizing
      # we'll create functions for that so the user is less likely to mess it up.

      ##' NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
      ##' In Kris' original function, this was manifested as a full investment constraint
      if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
        max_sum=constraints$max_sum
        if(sum(w)>max_sum) { w<-(max_sum/sum(w))*w } # normalize to max_sum
      }

      if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
        min_sum=constraints$min_sum
        if(sum(w)<min_sum) { w<-(min_sum/sum(w))*w } # normalize to min_sum
      }

    } # end min_sum and max_sum normalization

    ##' penalize weights outside my constraints
    if (!is.null(constraints$max)){
      max = constraints$max
      out = out + sum(w[which(w>max[1:N])]-max[which(w>max[1:N])])*penalty
    }
    if (!is.null(constraints$min)){
      min = constraints$min
      out = out + sum(w[which(w<min[1:N])]-w[which(w<min[1:N])])*penalty
    }
    
    if(is.null(constraints$objectives)) {
      warning("no objectives specified in constraints")
    } else{
      if(trace) tmp_return<-list()
      for (objective in constraints$objectives){
        if(objective$enabled){
          tmp_measure = NULL
          multiplier  = objective$multiplier
          switch(objective$name,
            median =,
            mean   = { tmp_measure = match.fun(objective$name)(sum(mu*w))
                     },
            sd =,
            StdDev = { tmp_measure = StdDev(R,
                                            weights=w,
                                            mu=mu,
                                            sigma=sigma,
                                            portfolio_method=objective$portfolio_method,
                                            ...=...
                                           )
            },
            var =,
            mVaR =,
            VaR = {tmp_measure = VaR(R,
                                    method=objective$method,
                                    portfolio_method=objective$portfolio_method,
                                    p=objective$p,
                                    weights=w,
                                    mu=mu,
                                    sigma=sigma,
                                    M3=M3,
                                    M4=M4,
                                    ...=...
                                  )
                  },
            es =,
            mES =,
            CVaR =,
            cVaR =,
            ES = {tmp_measure = ES(R,
                                    method=objective$method,
                                    portfolio_method=objective$portfolio_method,
                                    p=objective$p,
                                    weights=w,
                                    mu=mu,
                                    sigma=sigma,
                                    M3=M3,
                                    M4=M4,
                                    ...=...
                                  )
                  },
            nomatch = { tmp_measure = try(match.fun(objective$name),silent=TRUE) }
          ) # end objective switch
          
          # now set the new value of the objective output
          if(inherits(tmp_measure,"try-error")) { 
              message(paste("objective name",objective$name,"appears to not match a known R function"))
              next()
              
          } else{
              if(trace) tmp_return[[]]<-tmp_measure
          }
          
          if(inherits(objective,"return_objective") | inherits(objective,"portfolio_risk_objective")){
            if (is.null(objective$target)){
              # target is null, just minimize
              out = out + (tmp_measure*multiplier)
            } else { # we have a target
              out = out + penalty*objective$multiplier*(tmp_measure-objective$target)
              #should we also penalize risk too low for risk targets? or is a range another objective?
            }
          } # end handling for return and univariate risk objuectives
          
          if(inherits(objective,"risk_budget_objective")){
            # setup
            
            # out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
            # add risk budget constraint
            if(!is.null(objective$target)){
              #in addition to a risk budget constraint, we have a univariate target
              # the first element of the returned list is the univariate measure
              # we'll use the  univariate measure exactly like we would as a separate objective
              out = out + penalty*objective$multiplier*(tmp_measure[[1]]-objective$target)
            }
            percrisk = tmp_measure[[3]] # third element is percent component contribution
            RBupper = objective$max_prisk
            RBlower = objective$min_prisk
            out = out + penalty*multiplier*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
          } # end handling of risk_budget objective

        } # end enabled check
      } # end loop over objectives
    } # end objectives processing

    #message(paste("output of objective function",out))
 
    #return
    if(!trace){
        return(out)
    } else {
        return(list(out=out,weights=w,objective_measures=tmp_return))
    }
}


#KB_RBpaper_objective = function( w ){
#
#    #        w = matrix( c( w , 1-sum(w) ) , ncol=1) # assume a cash asset
#
#    # add weight constraint penalty, turn this into configuration from constraints
#    # force weights to 1
#    w <- (1/sum(w))*w
#    N = length(w);
#    percrisk = percriskcontrib( w );
#    out = -sum( w*mu ) #needs to be maximized
#    # add full investment constraint:
#    penalty = 1e4;
#    #out = out + penalty*( ((1-sum(w))>upper[N]) | ((1-sum(w))<lower[N]) )
#    # penalize weights outside my constraints
#    out = out + sum(w[which(w>upper[1:N])]-upper[which(w>upper[1:N])])*penalty
#    out = out + sum(lower[which(w<lower[1:N])]-w[which(w<lower[1:N])])*penalty
#
#    ##########
#    # add portfolio risk constraint
#    prw=prisk(w)
#    # full penalty for violating risk upper limit
#    if(  prw > Riskupper ){ out = out + penalty*( prw - Riskupper) }
#    # half penalty for risk lower than target
#    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
#
#    # add risk budget constraint
#    out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
#    #print(paste("output of objective function",out))
#
#    #return
#    return(out)
#}
