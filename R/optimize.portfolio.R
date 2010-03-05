###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2010 Kris Boudt, Peter Carl and Brian G. Peterson
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
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param rp matrix of random portfolio weights, default NULL, mostly for automated use by rebalancing optimization or repeated tests on same portfolios
#' @callGraph 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ..., rp=NULL)
{
  optimize_method=optimize_method[1]  
  start_t<-Sys.time()

  #store the call for later
  call <- match.call()

  if (is.null(constraints) | !is.constraint(constraints)){
      stop("you must pass in an object of class constraints to control the optimization")
  }
  
  R <- checkData(R)
  N = length(constraints$assets)
  if (ncol(R)>N) {
      R=R[,names(constraints$assets)]
  }
  T = nrow(R)
    
  out=list()
  
  weights=NULL
    
  dotargs <-list(...)    
  
  # set portfolio moments only once
  dotargs <- set.portfolio.moments(R, constraints, momentargs=dotargs)
  
  normalize_weights <- function(weights){
      # normalize results if necessary
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
              if(sum(weights)>max_sum) { weights<-(max_sum/sum(weights))*weights } # normalize to max_sum
          }
          
          if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
              min_sum=constraints$min_sum
              if(sum(weights)<min_sum) { weights<-(min_sum/sum(weights))*weights } # normalize to min_sum
          }
          
      } # end min_sum and max_sum normalization
      return(weights)
  }
  
  if(optimize_method=="DEoptim"){
    stopifnot("package:DEoptim" %in% search() || require("DEoptim",quietly = TRUE))
    # DEoptim does 200 generations by default, so lets set the size of each generation to search_size/200)
    if(hasArg(itermax)) itermax=match.call(expand.dots=TRUE)$itermax else itermax=200
    NP = round(search_size/itermax)
    if(NP>2000) NP=2000
    
    DEcformals  <- formals(DEoptim.control)
    DEcargs <- names(DEcformals)
    if( is.list(dotargs) ){
        pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- DEcargs[pm]
        DEcformals$NP <- NP
        DEcformals$itermax <- itermax
        DEcformals[pm] <- dotargs[pm > 0L]
        
        #TODO FIXME also check for a passed in controlDE list, including checking its class, and match formals
    }
    controlDE <- do.call(DEoptim.control,DEcformals)
    
    if(isTRUE(trace)) trace=FALSE #we can't pass trace=TRUE into constrained objective with DEoptim, because it expects a single numeric return
    
    # get upper and lower weights parameters from constraints
    upper = constraints$max
    lower = constraints$min

    minw = try(DEoptim( constrained_objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, ...=...)) # add ,silent=TRUE here?
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
        message(paste("Optimizer was unable to find a solution for target"))
        return (paste("Optimizer was unable to find a solution for target"))
    }

    weights = as.vector( minw$optim$bestmem)
    weights <- normalize_weights(weights)
    names(weights) = colnames(R)

    out = list(weights=weights, objective_measures=constrained_objective(w=weights,R=R,constraints,trace=TRUE)$objective_measures,call=call)
    if (isTRUE(trace)){out$DEoutput=minw}
    
  } ## end case for DEoptim
  if(optimize_method=="random"){
      #' call random_portfolios() with constraints and search_size to create matrix of portfolios
      if(missing(rp) | is.null(rp)){
          rp<-random_portfolios(rpconstraints=constraints,permutations=search_size)
      }
      #' store matrix in out if trace=TRUE
      if (isTRUE(trace)) out$random_portfolios<-rp
      #' write foreach loop to call constrained_objective() with each portfolio
      if ("package:foreach" %in% search() & !hasArg(parallel)){
          rp_objective_results<-foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective(w=rp[ii,],R,constraints,trace=trace,...=dotargs)
      } else {
          rp_objective_results<-apply(rp, 1, constrained_objective, R=R, constraints=constraints, trace=trace, ...=dotargs)
      }
      #' if trace=TRUE , store results of foreach in out$random_results
      if(isTRUE(trace)) out$random_portfolio_objective_results<-rp_objective_results
      #' loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
      search<-vector(length=length(rp_objective_results))
      # first we construct the vector of results
      for (i in 1:length(search)) {
          if (isTRUE(trace)) {
              search[i]<-ifelse(try(rp_objective_results[[i]]$out),rp_objective_results[[i]]$out,1e6)
          } else {
              search[i]<-as.numeric(rp_objective_results[[i]])
          }
      }
      # now find the weights that correspond to the minimum score from the constrained objective
      # and normalize_weights so that we meet our min_sum/max_sum constraints
      if (isTRUE(trace)) {
          min_objective_weights<- try(normalize_weights(rp_objective_results[[which.min(search)]]$weights))
      } else {
          min_objective_weights<- try(normalize_weights(rp[which.min(search),]))
      }
      #' re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
      out$weights<-min_objective_weights
      out$constrained_objective<-try(constrained_objective(w=min_objective_weights,R=R,constraints,trace=TRUE)$objective_measures)
      out$call<-call
      #' construct out list to be as similar as possible to DEoptim list, within reason
  }
    end_t<-Sys.time()
    # print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
    message(c("elapsed time:",end_t-start_t))
    out$constraints<-constraints
    out$data_summary<-list(first=first(R),last=last(R))
    out$elapsed_time<-end_t-start_t
    out$end_t<-as.character(Sys.time())
    class(out)<-c(paste("optimize.portfolio",optimize_method,sep='.'),"optimize.portfolio")
    return(out)
}

#' portfolio optimization with support for rebalancing or rolling periods
#' 
#' This function may eventually be wrapped into optimize.portfolio
#' 
#' For now, we'll set the rebalancing periods here, though I think they should eventually be part of the constraints object
#' 
#' This function is massively parallel, and will require 'foreach' and we suggest that you register a parallel backend.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param rebalance_on a periodicity as returned by xts function periodicity and usable by endpoints
#' @param training_period period to use as training in the front of the data
#' @param trailing_periods if set, an integer with the number of periods to roll over, default NULL will run from inception
#' @callGraph 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio.rebalancing <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ..., rebalance_on=NULL, training_period=NULL, trailing_periods=NULL)
{
    stopifnot("package:foreach" %in% search() || require("foreach",quietly=TRUE))
    start_t<-Sys.time()
      
    #store the call for later
    call <- match.call()
    if(optimize_method=="random"){
        #' call random_portfolios() with constraints and search_size to create matrix of portfolios
        rp<-random_portfolios(rpconstraints=constraints,permutations=search_size)
    } else {
        rp=NULL
    }    
    
    if(is.null(training_period)) {if(nrow(R)<36) training_period=nrow(R) else training_period=36}
    if (is.null(trailing_periods)){
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        out_list<-foreach(ep=iter(ep.i), .errorhandling='pass') %dopar% {
                    optimize.portfolio(R[1:ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    } else {
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        out_list<-foreach(ep=iter(ep.i), .errorhandling='pass') %dopar% {
                    optimize.portfolio(R[(ifelse(ep-trailing_periods>=1,ep-trailing_periods,1)):ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    }
    names(out_list)<-index(R[ep.i])
    
    end_t<-Sys.time()
    message(c("overall elapsed time:",end_t-start_t))
    class(out_list)<-c("optimize.portfolio.rebalancing")
    return(out_list)
}

#' set portfolio moments for use by lower level optimization functions
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
set.portfolio.moments <- function(R, constraints, momentargs=NULL){

    if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
    if(is.null(constraints$objectives)) {
        warning("no objectives specified in constraints")
        next()
    } else {
        lcl<-grep('clean',constraints)
        if(!identical(lcl,integer(0))) {
            for (objective in constraints[lcl]){
                if(!is.null(objective$arguments$clean)) {
                    cleanR<-try(Return.clean(R,method=objective$arguments$clean))
                    if(!inherits(cleanR,"try-error")) {
                        momentargs$mu = matrix( as.vector(apply(cleanR,2,'mean')),ncol=1);
                        momentargs$sigma = cov(cleanR);
                        momentargs$m3 = PerformanceAnalytics:::M3.MM(cleanR)
                        momentargs$m4 = PerformanceAnalytics:::M4.MM(cleanR)
                        #' FIXME NOTE: this isn't perfect as it overwrites the moments for all objectives, not just one with clean='boudt'
                    }
                }
            }    
        }
        for (objective in constraints$objectives){
            switch(objective$name,
                    sd =,
                    StdDev = { 
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                    },
                    var =,
                    mVaR =,
                    VaR = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
                    },
                    es =,
                    mES =,
                    CVaR =,
                    cVaR =,
                    ES = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
                    }
            ) # end switch on objectives    
        }    
    }    
    return(momentargs)
}

#'execute multiple optimize.portfolio calls, presumably in parallel
#' 
#' TODO write function to check sensitivity of optimal results by using optimize.portfolio.parallel results
#' 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param nodes how many processes to run in the foreach loop, default 4
#' @callGraph 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio.parallel <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ..., nodes=4)
{
    stopifnot("package:foreach" %in% search() || require("foreach",quietly=TRUE))
    optimize_method=optimize_method[1]  
    
    start_t<-Sys.time()
    
    #store the call for later
    call <- match.call()
    
    opt_out_list<-foreach(1:nodes) %dopar% optimize.portfolio(R=R,constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, ...)    

    end_t<-Sys.time()
    message(c("overall elapsed time:",end_t-start_t))
    class(opt_out_list)<-c("optimize.portfolio.parallel")
    return(opt_out_list)
    
}


#TODO write function to compute an efficient frontier of optimal portfolios

###############################################################################
# $Id$
###############################################################################
