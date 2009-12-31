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

### @TODO: Rewrite this function to test the input object and direct to the correct parse function
extractstats <- function(resultlist) {
    l = length(resultlist)
    result=matrix(nrow=l,ncol=49)
    colnames(result)<-c("mean_ret","sd","mVaR","mES",names(resultlist[[1]]$weights))
    for (i in 1:l) {
        if(!is.atomic(resultlist[[i]])){
            if(!is.null(resultlist[[i]]$stats)){
                result[i,1:4]<-t(resultlist[[i]]$stats)
            } else if(!is.null(resultlist[[i]]$mean_ret)){
                result[i,1]<-resultlist[[i]]$mean_ret
                result[i,2]<-resultlist[[i]]$sd
                result[i,3]<-resultlist[[i]]$VaR
                result[i,4]<-resultlist[[i]]$ES
            }
            result[i,]<-t(c(result[i,1:4],resultlist[[i]]$weights))
        }
    }
    return(result)
}

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