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

#' extract some stats and weights from a portfolio run via \code{optimize.portfolio}
#' 
#' This function will dispatch to the appropriate class handler based on the
#' input class of the optimize.portfolio output object
#'  
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}
#' @export
extractStats <- function (object, prefix=NULL, ...){
    UseMethod('extractStats')
}

#' utility function to replace awkward named from unlist
#' @param rnames character vector of names to check for cleanup
name.replace <- function(rnames){
    rnames<-gsub("objective_measures.",'',rnames)
    matchvec<-c('mean.mean','median.median','ES.ES','CVaR.ES','ES.MES','CVaR.MES','VaR.MVaR','maxDrawdown.maxDrawdown','sd.sd','StdDev.StdDev')
    for(str in matchvec){
        pos<-pmatch(str,rnames)
        if(!is.na(pos)){
            switch(str,
                    mean.mean = {rnames[pos]<-'mean'},
                    median.median = {rnames[pos]<-'median'},
                    CVaR.MES =, CVaR.ES = {rnames[pos]<-'CVaR'}, 
                    ES.MES =, ES.ES =  {rnames[pos]<-'ES'},
                    VaR.MVaR = {rnames[pos]<-'VaR'},
                    maxDrawdown.maxDrawdown = {rnames[pos]<-'maxDrawdown'},
                    sd.sd=, StdDev.StdDev = {rnames[pos]<-'StdDev'},
                    #pamean={rnames[pos]<-'mean'}
            )
        }
    } 
    return(rnames)
}

#' extract some stats from a portfolio list run with DEoptim via
#' \code{\link{optimize.portfolio}}
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}
#' @export 
extractStats.optimize.portfolio.DEoptim <- function(object, prefix=NULL, ...) {

    # first pull out the optimal portfolio
    trow<-c(unlist(object$objective_measures),out=object$out,object$weights)
    #colnames(trow)<-c(colnames(unlist(object$objective_measures)),'out',names(object$weights))
    result<-trow
    l = length(object$DEoptim_objective_results)
    nobj<-length(unlist(object$DEoptim_objective_results[[1]]$objective_measures))
    result=matrix(nrow=l,ncol=(nobj+length(object$weights))+1)
    ncols<-ncol(result)
    
    for (i in 1:l) {
        if(!is.atomic(object$DEoptim_objective_results[[i]])) {
            result[i,1:nobj]<-unlist(object$DEoptim_objective_results[[i]]$objective_measures)
            result[i,(nobj+1)]<-object$DEoptim_objective_results[[i]]$out
            result[i,(nobj+2):ncols]<-object$DEoptim_objective_results[[i]]$weights
        }
    }
    
    rnames<-c(names(unlist(object$DEoptim_objective_results[[1]]$objective_measures)),'out',paste('w',names(object$weights),sep='.'))
    rnames<-name.replace(rnames)
    colnames(result)<-rnames
    rownames(result) = paste(prefix,"DE.portf", index(object$DEoptim_objective_results), sep=".")
    #rownames(result) = paste("DE.portf.", index(result), sep="")
    return(result)
}


#' extract some stats from a portfolio list run via foreach in optimize.portfolio.parallel
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso 
#' \code{\link{optimize.portfolio}}
#' \code{\link{optimize.portfolio.parallel}}
#' \code{\link{extractStats}}
#' @export
extractStats.optimize.portfolio.parallel <- function(object,prefix=NULL,...) {
    resultlist<-object
    l = length(resultlist)
    result=NULL
    for (i in 1:l) {
        if(is.null(result)) result<-extractStats(resultlist[[i]])
        else result <- rbind(result,extractStats(resultlist[[i]]))
    }
    
    rownames(result) = paste("par", index(result), rownames(result), sep=".")
    return(result)
}

#' extract stats from random portfolio results
#' 
#' This just flattens the $random_portfolio_objective_results part of the object
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso 
#' \code{\link{optimize.portfolio}}
#' \code{\link{random_portfolios}}
#' \code{\link{extractStats}}
#' @export
extractStats.optimize.portfolio.random <- function(object, prefix=NULL, ...){
# This just flattens the $random_portfolio_objective_results part of the
# object
# @TODO: add a class check for the input object
  OptimResults<-object  

  l = length(OptimResults$random_portfolio_objective_results)
  nobj<-length(unlist(OptimResults$random_portfolio_objective_results[[1]]$objective_measures))
  result=matrix(nrow=l,ncol=(nobj+length(OptimResults$weights))+1)
  ncols<-ncol(result)
  
  for (i in 1:l) {
      if(!is.atomic(OptimResults$random_portfolio_objective_results[[i]])) {
          result[i,1:nobj]<-unlist(OptimResults$random_portfolio_objective_results[[i]]$objective_measures)
          result[i,(nobj+1)]<-OptimResults$random_portfolio_objective_results[[i]]$out
          result[i,(nobj+2):ncols]<-OptimResults$random_portfolio_objective_results[[i]]$weights
      }
  }
  
  rnames<-c(names(unlist(OptimResults$random_portfolio_objective_results[[1]]$objective_measures)),'out',paste('w',names(OptimResults$weights),sep='.'))
  rnames<-name.replace(rnames)
  colnames(result)<-rnames
  rownames(result) = paste(prefix,"rnd.portf", index(OptimResults$random_portfolio_objective_results), sep=".")

  return(result)
}

#' extract weights from a portfolio run via \code{optimize.portfolio} or \code{optimize.portfolio.rebalancing}
#' 
#' This function will dispatch to the appropriate class handler based on the
#' input class of the optimize.portfolio or optimize.portfolio.rebalancing output object
#'  
#' @param object list returned by optimize.portfolio
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}, \code{\link{optimize.portfolio.rebalancing}}
#' @export
extractWeights <- function (object, ...){
  UseMethod('extractWeights')
}

#' extract weights from output of optimize.portfolio
#' 
#' @param object object of type optimize.portfolio to extract weights from
#' @seealso 
#' \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
extractWeights.optimize.portfolio <- function(object){
  if(!inherits(object, "optimize.portfolio")){
    stop("object must be of class 'optimize.portfolio'")
  }
  return(object$weights)
}

#' extract time series of weights from output of optimize.portfolio.rebalancing
#' 
#' \code{\link{optimize.portfolio.rebalancing}} outputs a list of
#' \code{\link{optimize.portfolio}} objects, one for each rebalancing period
#' 
#' The output list is indexed by the dates of the rebalancing periods, as determined by \code{endpoints}
#' 
#' @param RebalResults object of type optimize.portfolio.rebalancing to extract weights from
#' @param ... any other passthru parameters
#' @seealso 
#' \code{\link{optimize.portfolio.rebalancing}}
#' @export
extractWeights.optimize.portfolio.rebalancing <- function(RebalResults, ...){
# @TODO: add a class check for the input object
# FIXED
  if(!inherits(RebalResults, "optimize.portfolio.rebalancing")){
    stop("Object passed in must be of class 'optimize.portfolio.rebalancing'")
  }
  
  numColumns = length(RebalResults[[1]]$weights)
  numRows = length(RebalResults)

  result <- matrix(nrow=numRows, ncol=numColumns)

  for(i in 1:numRows)
    result[i,] = unlist(RebalResults[[i]]$weights)

  colnames(result) = names(unlist(RebalResults[[1]]$weights))
  rownames(result) = names(RebalResults)
  result = as.xts(result)
  return(result)
}


#' extract some stats from a portfolio list run with ROI via
#' \code{\link{optimize.portfolio}}
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @export 
extractStats.optimize.portfolio.ROI <- function(object, prefix=NULL, ...) {
  
  trow<-c(out=object$out, object$weights)
  result<-trow
  
  rnames<-c('out',paste('w',names(object$weights),sep='.'))
  names(result)<-rnames
  return(result)
}

#' extract some stats from a portfolio list run with pso via
#' \code{\link{optimize.portfolio}}
#' 
#' This function will extract the weights (swarm positions) from the PSO output
#' and the out value (swarm fitness values) for each iteration of the optimization.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @author Ross Bennett
#' @export 
extractStats.optimize.portfolio.pso <- function(object, prefix=NULL, ...){
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class optimize.portfolio.pso")
  
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
  
  # get the constraints for min_sum and max_sum normalization
  constraints <- get_constraints(object$portfolio)
  
  # optimal portfolio
  # trow <- c(unlist(object$objective_measures), out=object$out, object$weights)
  
  # get the weights of each iteration from PSOoutput
  psoweights <- do.call(rbind, lapply(object$PSOoutput$stats$x, t))
  
  # need to normalize so that psoweights are valid portfolios
  psoweights <- t(apply(psoweights, 1, normalize_weights))
  
  # bind the optimal weights to psoweights
  psoweights <- rbind(object$weights, psoweights)
  
  # get swarm fitness values (i.e. out value of the function evaluated with the swarm positions)
  tmpout <- unlist(object$PSOoutput$stats$f)
  
  # combine the optimal out value to the vector of out values
  tmpout <- c(object$out, tmpout)
  
  result <- cbind(tmpout, psoweights)
  colnames(result) <- c("out", paste('w',names(object$weights),sep='.'))
  rownames(result) <- paste(prefix, "pso.portf", index(tmp), sep=".")
  return(result)
}
