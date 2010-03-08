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


#' extract some stats from a DEoptim portfolio list run via foreach
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
    nobj<-length(resultlist[[1]]$objective_measures)
    result=matrix(nrow=l,ncol=(nobj+length(resultlist[[1]]$weights)))
    colnames(result)<-c(names(resultlist[[1]]$objective_measures),names(resultlist[[1]]$weights))
    ncols<-ncol(result)
    for (i in 1:l) {
        if(!is.atomic(resultlist[[i]])) {
            result[i,1:nobj]<-unlist(resultlist[[i]]$objective_measures)
            result[i,(nobj+1):ncols]<-resultlist[[i]]$weights
        }
    }
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