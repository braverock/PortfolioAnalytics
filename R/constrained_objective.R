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

# TODO add examples
 
# TODO add more details about the nuances of the optimization engines


#' function to calculate a numeric return value for a portfolio based on a set of constraints
#' 
#' function to calculate a numeric return value for a portfolio based on a set of constraints,
#' we'll try to make as few assumptions as possible, and only run objectives that are required by the user
#' 
#' If the user has passed in either min_sum or max_sum constraints for the portfolio, or both, 
#' and are using a numerical optimization method like DEoptim, and normalize=TRUE, the default,
#' we'll normalize the weights passed in to whichever boundary condition has been violated.  
#' If using random portfolios, all the portfolios generated will meet the constraints by construction.
#' NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
#' might violate your constraints, so you'd need to renormalize them after optimizing
#' We apply the same normalization in \code{\link{optimize.portfolio}} so that the weights you see have been 
#' normalized to min_sum if the generated portfolio is smaller than min_sum or max_sum if the 
#' generated portfolio is larger than max_sum.  
#' This normalization increases the speed of optimization and convergence by several orders of magnitude in many cases.
#' 
#' You may find that for some portfolios, normalization is not desirable, if the algorithm 
#' cannot find a direction in which to move to head towards an optimal portfolio.  In these cases, 
#' it may be best to set normalize=FALSE, and penalize the portfolios if the sum of the weighting 
#' vector lies outside the min_sum and/or max_sum.
#' 
#' Whether or not we normalize the weights using min_sum and max_sum, and are using a numerical optimization 
#' engine like DEoptim, we will penalize portfolios that violate weight constraints in much the same way
#' we penalize other constraints.  If a min_sum/max_sum normalization has not occurred, convergence
#' can take a very long time.  We currently do not allow for a non-normalized full investment constraint.  
#' Future version of this function could include this additional constraint penalty. 
#'  
#' When you are optimizing a return objective, you must specify a negative multiplier 
#' for the return objective so that the function will maximize return.  If you specify a target return,
#' any return less than your target will be penalized.  If you do not specify a target return, 
#' you may need to specify a negative VTR (value to reach) , or the function will not converge.  
#' Try the maximum expected return times the multiplier (e.g. -1 or -10).  
#' Adding a return objective defaults the multiplier to -1.
#' 
#' Additional parameters for random portfolios or \code{\link[DEoptim]{DEoptim.control}} may be passed in via \dots
#' 
#'    
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param w a vector of weights to test
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param \dots any other passthru parameters 
#' @param trace TRUE/FALSE whether to include debugging and additional detail in the output list
#' @param normalize TRUE/FALSE whether to normalize results to min/max sum (TRUE), or let the optimizer penalize portfolios that do not conform (FALSE)
#' @param storage TRUE/FALSE default TRUE for DEoptim with trace, otherwise FALSE. not typically user-called
#' @seealso \code{\link{constraint}}, \code{\link{objective}}, \code{\link[DEoptim]{DEoptim.control}} 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
constrained_objective <- function(w, R, constraints, ..., trace=FALSE, normalize=TRUE, storage=FALSE)
{ 
    if (ncol(R)>length(w)) {
        R=R[,1:length(w)]
    }
    if(!hasArg(penalty)) penalty = 1e4
    N = length(w)
    T = nrow(R)
    if(hasArg(optimize_method)) 
    	optimize_method=match.call(expand.dots=TRUE)$optimize_method else optimize_method='' 
    if(hasArg(verbose)) 
    	verbose=match.call(expand.dots=TRUE)$verbose 
    else verbose=FALSE 
    
    # check for valid constraints
    if (!is.constraint(constraints)) {
    	stop("constraints passed in are not of class constraint")
    }

    # check that the constraints and the weighting vector have the same length
    if (N != length(constraints$assets)){
      warning("length of constraints asset list and weights vector do not match, results may be bogus")
    }

    out=0

    # do the get here
    store_output <- try(get('.objectivestorage',pos='.GlobalEnv'),silent=TRUE)
    if(inherits(store_output,"try-error")) storage=FALSE else storage=TRUE        
    
    if(isTRUE(normalize)){
        if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
            # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
            # we'll normalize the weights passed in to whichever boundary condition has been violated
            # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
            # might violate your constraints, so you'd need to renormalize them after optimizing
            # we'll create functions for that so the user is less likely to mess it up.
            
            #' NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
            #' In Kris' original function, this was manifested as a full investment constraint
            #' the normalization process produces much faster convergence, 
            #' and then we penalize parameters outside the constraints in the next block
            if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
                max_sum=constraints$max_sum
                if(sum(w)>max_sum) { w<-(max_sum/sum(w))*w } # normalize to max_sum
            }
            
            if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
                min_sum=constraints$min_sum
                if(sum(w)<min_sum) { w<-(min_sum/sum(w))*w } # normalize to min_sum
            }
            
        } # end min_sum and max_sum normalization
    } else {
        # the user wants the optimization algorithm to figure it out
        if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
            max_sum=constraints$max_sum
            if(sum(w)>max_sum) { out = out + penalty*(sum(w) - max_sum)  } # penalize difference to max_sum
        }
        if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
            min_sum=constraints$min_sum
            if(sum(w)<min_sum) { out = out + penalty*(min_sum - sum(w)) } # penalize difference to min_sum
        }
    }

    #' penalize weights outside my constraints (can be caused by normalization)
    if (!is.null(constraints$max)){
      max = constraints$max
      out = out + sum(w[which(w>max[1:N])]- constraints$max[which(w>max[1:N])])*penalty
    }
    if (!is.null(constraints$min)){
      min = constraints$min
      out = out + sum(constraints$min[which(w<min[1:N])] - w[which(w<min[1:N])])*penalty
    }
    
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        # rm('...')
        nargs=NULL
    }

    nargs<-set.portfolio.moments(R, constraints, momentargs=nargs)
    
    if(is.null(constraints$objectives)) {
      warning("no objectives specified in constraints")
    } else{
      if(isTRUE(trace) | isTRUE(storage)) tmp_return<-list()
      for (objective in constraints$objectives){
        #check for clean bits to pass in
        if(objective$enabled){
          tmp_measure = NULL
          multiplier  = objective$multiplier
          #if(is.null(objective$arguments) | !is.list(objective$arguments)) objective$arguments<-list()
          switch(objective$name,
              mean =,
              median = {
                  fun = match.fun(objective$name)  
                  nargs$x <- ( R %*% w ) #do the multivariate mean/median with Kroneker product
              },
              sd =,
              StdDev = { 
                  fun= match.fun(StdDev)
              },
              mVaR =,
              VaR = {
                  fun= match.fun(VaR) 
                  if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method) & is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
                  if(is.null(objective$arguments$invert)) objective$arguments$invert = FALSE
              },
              es =,
              mES =,
              CVaR =,
              cVaR =,
              ES = {
                  fun = match.fun(ES)
                  if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method)& is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
                  if(is.null(objective$arguments$invert)) objective$arguments$invert = FALSE
              },
              {   # see 'S Programming p. 67 for this matching
                  fun<-try(match.fun(objective$name))
              }
          )
          if(is.function(fun)){
              .formals  <- formals(fun)
              onames <- names(.formals)
              if(is.list(objective$arguments)){
                  #TODO FIXME only do this if R and weights are in the argument list of the fn
                  if(is.null(nargs$R) | !length(nargs$R)==length(R)) nargs$R <- R
                  
                  if(is.null(nargs$weights)) nargs$weights <- w
                  
                  pm <- pmatch(names(objective$arguments), onames, nomatch = 0L)
                  if (any(pm == 0L))
                      warning(paste("some arguments stored for",objective$name,"do not match"))
                  # this line overwrites the names of things stored in $arguments with names from formals.
                  # I'm not sure it's a good idea, so commenting for now, until we prove we need it
                  #names(objective$arguments[pm > 0L]) <- onames[pm]
                  .formals[pm] <- objective$arguments[pm > 0L]
                  #now add dots
                  if (length(nargs)) {
                      dargs<-nargs
                      pm <- pmatch(names(dargs), onames, nomatch = 0L)
                      names(dargs[pm > 0L]) <- onames[pm]
                      .formals[pm] <- dargs[pm > 0L]
                  }
                  .formals$... <- NULL
              }
          } # TODO do some funky return magic here on try-error
          
          tmp_measure = try((do.call(fun,.formals)) ,silent=TRUE)
          
          if(isTRUE(trace) | isTRUE(storage)) {
              if(is.null(names(tmp_measure))) names(tmp_measure)<-objective$name
              tmp_return[[objective$name]]<-tmp_measure
          }
          
          if(inherits(tmp_measure,"try-error")) { 
              message(paste("objective name",objective$name,"generated an error or warning:",tmp_measure))
              next()  
          } 
          
          # now set the new value of the objective output
          if(inherits(objective,"return_objective")){ 
              if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
                  out = out + penalty*abs(objective$multiplier)*abs(tmp_measure-objective$target)
              }  
              # target is null or doesn't exist, just maximize, or minimize violation of constraint
              out = out + abs(objective$multiplier)*tmp_measure
          } # end handling for return objectives

          if(inherits(objective,"portfolio_risk_objective")){
            if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
                out = out + penalty*abs(objective$multiplier)*abs(tmp_measure-objective$target)
                #should we also penalize risk too low for risk targets? or is a range another objective?
                #    # half penalty for risk lower than target
                #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
            }  
            # target is null or doesn't exist, just maximize, or minimize violation of constraint
            out = out + abs(objective$multiplier)*tmp_measure
          } #  univariate risk objectives
          
          if(inherits(objective,"risk_budget_objective")){
            # setup
            
            # out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
            # add risk budget constraint
            if(!is.null(objective$target) & is.numeric(objective$target)){
              #in addition to a risk budget constraint, we have a univariate target
              # the first element of the returned list is the univariate measure
              # we'll use the  univariate measure exactly like we would as a separate objective
                out = out + penalty*abs(objective$multiplier)*abs(tmp_measure[[1]]-objective$target)
              #should we also penalize risk too low for risk targets? or is a range another objective?
              #    # half penalty for risk lower than target
              #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
            }
            percrisk = tmp_measure[[3]] # third element is percent component contribution
            RBupper = objective$max_prisk
            RBlower = objective$min_prisk
            if(!is.null(RBupper) | !is.null(RBlower)){
                out = out + penalty * objective$multiplier * sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
            }
#             if(!is.null(objective$min_concentration)){
#                 if(isTRUE(objective$min_concentration)){
#                     max_conc<-max(tmp_measure[[2]]) #second element is the contribution in absolute terms
#                     # out=out + penalty * objective$multiplier * max_conc
#                     out = out + objective$multiplier * max_conc
#                 }
#             }
            # Combined min_con and min_dif to take advantage of a better concentration obj measure
            if(!is.null(objective$min_difference) || !is.null(objective$min_concentration)){
                if(isTRUE(objective$min_difference)){
#                     max_diff<-max(tmp_measure[[2]]-(sum(tmp_measure[[2]])/length(tmp_measure[[2]]))) #second element is the contribution in absolute terms
                  # Uses Herfindahl index to calculate concentration; added scaling perc diffs back to univariate numbers
                    max_diff <- sqrt(sum(tmp_measure[[3]]^2))/100  #third element is the contribution in percentage terms
                    # out = out + penalty * objective$multiplier * max_diff
                    out = out + penalty*objective$multiplier * max_diff
                }
            }
          } # end handling of risk_budget objective

        } # end enabled check
      } # end loop over objectives
    } # end objectives processing

    if(isTRUE(verbose)) {
        print('weights: ')
        print(paste(w,' '))
        print(paste("output of objective function",out))
        print(unlist(tmp_return))
    }
    
    if(is.na(out) | is.nan(out) | is.null(out)){
        #this should never happen
        warning('NA or NaN produced in objective function for weights ',w)
        out<-penalty
    }
    
    #return
    if (isTRUE(storage)){
        #add the new objective results
        store_output[[length(store_output)+1]]<-list(out=as.numeric(out),weights=w,objective_measures=tmp_return)
        # do the assign here
        assign('.objectivestorage', store_output, pos='.GlobalEnv')
    }
    if(!isTRUE(trace)){
        return(out)
    } else {
        return(list(out=as.numeric(out),weights=w,objective_measures=tmp_return))
    }
}