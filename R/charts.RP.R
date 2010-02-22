###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2010 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id:$
#
###############################################################################

#' boxplot of the weight distributions in the random portfolios 
#' @param RP set of random portfolios created by \code{\link{random_portfolios}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param ... any other passthru parameters 
#' @seealso \code{\link{random_portfolios}}
#' @export
chart.Weights.RP <- function(RP, neighbors = NA, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    columnnames = names(RP$weights)
    numassets = length(columnnames)

    if(!is.na(neighbors)){
        xtract=extractStats.rp(RP)
        orderx = order(xtract[,"out.mean"])
        subsetx = head(xtract[orderx,2:(numassets+1)], n=neighbors)
    }
    par( mar=c(5,4,4,2)) # set margin to accomodate vertical names
    plot(RP$random_portfolios[1,], type="b", col="orange", axes=FALSE, xlab="", ylim=c(0,max(RP$constraints$max)), ...)
    if(!is.na(neighbors)){
        for(i in 1:neighbors) points(subsetx[i,], type="b", col="lightblue")
        points(RP$random_portfolios[1,], type="b", col="orange", pch=16) # to overprint neighbors
    }

    points(RP$weights, type="b", col="red", pch=16)
    points(RP$constraints$min, type="b", col="darkgray", lty="dashed", pch=24)
    points(RP$constraints$max, type="b", col="darkgray", lty="dashed", pch=25)
    box()
    axis(2)
    axis(1, labels=names(RP$weights), at=1:numassets, las=3)
}

#' classic risk return scatter of random portfolios
#' @param RP set of random portfolios created by \code{\link{random_portfolios}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param ... any other passthru parameters 
#' @seealso \code{\link{random_portfolios}}
#' @export
chart.Scatter.RP <- function(RP, neighbors = NA, return.col='mean', risk.col='ES', ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    xtract = extractStats.rp(RP)
    columnnames = colnames(xtract)
    return.column = grep(paste("objective_measures",return.col,sep='.'),columnnames)
    ## @TODO: Generalize this to find column containing the "risk" metric
    risk.column = grep(paste("objective_measures",risk.col,sep='.'),columnnames)

    plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="gray", ...)

    if(!is.na(neighbors)){ # overplot nearby portfolios
        orderx = order(xtract[,"out.mean"]) #TODO this won't work if the objective is anything othchart.Scatter.er than mean
        subsetx = head(xtract[orderx,], n=neighbors)
        points(subsetx[,risk.column], subsetx[,return.column], col="lightblue", pch=1)
    }
    points(xtract[1,risk.column],xtract[1,return.column], col="orange", pch=16) # overplot the equal weighted (or seed)
    #check to see if portfolio 1 is EW  RP$random_portoflios[1,] all weights should be the same
    if(!isTRUE(all.equal(RP$random_portfolios[1,][1],1/length(RP$random_portfolios[1,] ) ))){
        #show both the seed and EW if they are different 
        #NOTE the all.equal comparison could fail above if the first element of the first portfolio is the same as the EW weight, 
        #but the rest is not, shouldn't happen often with real portfolios, only toy examples
        points(xtract[2,risk.column],xtract[2,return.column], col="green", pch=16) # overplot the equal weighted (or seed)
    }
    ## @TODO: Generalize this to find column containing the "risk" metric
    points(RP$constrained_objective[risk.col], RP$constrained_objective[return.col], col="blue", pch=16) # optimal
}

#' scatter and weights chart  for random portfolios
#' @param RP set of random portfolios created by \code{\link{random_portfolios}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param ... any other passthru parameters 
#' @seealso \code{\link{random_portfolios}}
#' @export
charts.RP <- function(RP, neighbors = NA, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    layout(matrix(c(1,2)),height=c(2,1),width=1)
    chart.Scatter.RP(RP, neighbors, ...)
    chart.Weights.RP(RP, neighbors, ...)
}
