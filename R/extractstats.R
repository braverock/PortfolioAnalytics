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


#' extract some stats from a portfolio list run via \code{foreach} via
#' \code{\link{optimize.portfolio.parallel}}
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' TODO: Rewrite this function to test the input object and direct to the correct parse function
#' 
#' @param resultlist list returned by optimize.portfolio
#' @seealso \code{\link{optimize.portfolio}}
#' @export
extractstats <- function(resultlist) {
    l = length(resultlist)
    nobj<-length(unlist(resultlist[[1]]$objective_measures))
    result=matrix(nrow=l,ncol=(nobj+length(resultlist[[1]]$weights)))
    cnames<-c(names(unlist(resultlist[[1]]$objective_measures)),names(resultlist[[1]]$weights))
    matchvec<-c('mean.mean','median.median','ES.MES','VaR.MVaR')
    for(str in matchvec){
        pos<-pmatch(str,cnames)
        if(!is.na(pos)){
            switch(str,
                    mean.mean = {cnames[pos]<-'mean'},
                    median.median = {cnames[pos]<-'median'},
                    ES.MES = {cnames[pos]<-'ES'},
                    VaR.MVaR = {cnames[pos]<-'VaR'}
            )
        }
    }
    colnames(result)<-cnames
    ncols<-ncol(result)
    for (i in 1:l) {
        if(!is.atomic(resultlist[[i]])) {
            result[i,1:nobj]<-unlist(resultlist[[i]]$objective_measures)
            result[i,(nobj+1):ncols]<-resultlist[[i]]$weights
        }
    }
    
    rownames(result) = paste("opt.portf.", index(resultlist), sep="")
    return(result)
}

#' unlist random portfolio resultsoops
#' 
#' This just flattens the $random_portfolio_objective_results part of the object
#' 
#' @param OptimResults list returned by optimize.portfolio
#' @param ... any other passthru parameters
#' @seealso \code{\link{random_portfolios}}
#' @export
extractStats.rp <-
function(OptimResults, ...){
# This just flattens the $random_portfolio_objective_results part of the
# object
# @TODO: add a class check for the input object
  numColumns = length(unlist(OptimResults$random_portfolio_objective_results[[1]]))
  numRows = length(OptimResults$random_portfolio_objective_results)

  result <- matrix(nrow=numRows, ncol=numColumns)

  for(i in 1:numRows)
    result[i,] = unlist(OptimResults$random_portfolio_objective_results[[i]])

  colnames(result) = names(unlist(OptimResults$random_portfolio_objective_results[[1]]))
  rownames(result) = paste("rnd.portf.", index(OptimResults$random_portfolio_objective_results), sep="")

  return(result)
}

#' extract time series of weights from output of \code{\link{optimize.portfolio.rebalancing}}
#' @param RebalResults object of type optimize.portfolio.rebalancing to extract weights from
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio.rebalancing}}
#' @export
extractWeights.rebal <-
function(RebalResults, ...){
# @TODO: add a class check for the input object
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