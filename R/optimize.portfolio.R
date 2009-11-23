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

#' wrapper for constrained optimization of portfolios
#' 
#' This function aims to provide a wrapper for constrained optimization of 
#' portfolios that allows the user to specify box constraints and business 
#' objectives.
#' 
#' This function currently supports DEoptim and random portfolios as back ends.
#' Additional back end contributions for Rmetrics, ghyp, etc. would be welcome.
#'  
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraints}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @callGraph 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ...)
{
  #store the call for later
  call <- match.call()

  R <- checkData(R)
  
  if (is.null(constraints) | !is.constraint(constraints)){
    stop("you must pass in an object of class constraints to control the optimization")
  }

  if(optimize_method=="DEoptim"){
    # DEoptim does 200 generations by default, so lets set the size of each generation to search_size/200)
    NP = round(search_size/200)
    if(!hasArg(mu))    mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
    if(!hasArg(sigma)) sigma = cov(R);
    if(!hasArg(M3))    M3 = M3.MM(R,mu)
    if(!hasArg(M4))    M4 = M4.MM(R,mu)

    # get upper and lower weights parameters from constraints
    upper = constraints$max
    lower = constraints$min

    minw = try(DEoptim( constrained_objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, ...)) # add ,silent=TRUE here?
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
        message(paste("Optimizer was unable to find a solution for target"))
        return (paste("Optimizer was unable to find a solution for target"))
    }

    w = as.vector( minw$optim$bestmem)
    # normalize results if necessary
    if(!is.null(contraints$min_sum) | !is.null(constraints$max_sum)){
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
    names(w) = colnames(R)

    out = list(weights=w , objective_measures=constrained_objective(w=w,R=R,constraints,trace=TRUE)$objective_measures,call=call) 
    if(trace){out$DEoutput=minw}
    
  } ## end case for DEoptim

    ## FIXME: CODE BELOW THIS  POINT HIGHLY QUESTIONABLE
    # print(c("combining results for target",Riskupper ))
    # outw = as.vector(c( minw$optim$bestmem , 1-sum(minw$optim$bestmem) )) ; #full investment constraint
    # names(outw) = c(colnames(R),"cash")
    # check
    # include some standard measures
#    out$stats=c(sum( outw*mu ) , StdDev(outw,sigma=sigma), VaR(outw,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4), ES(outw,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4))
#    names(out$stats)<-c("mean_ret","Std. Dev.","mVaR","mES")
#    # @TODO: change these to use PerformanceAnalytics functions
#    out$targets<-c(minriskcriterion,Riskupper,percriskcontribcriterion)
#    names(out$targets)<-c("Risk Fn","Risk Target","Risk Contribution Fn")

    end_t<-Sys.time()
    # print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
    print(c("elapsed time:",round(end_t-start_t,2)))
    return(out)
}