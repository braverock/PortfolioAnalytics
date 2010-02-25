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
chart.Weights.RP <- function(RP, neighbors = NA, las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, main="Weights", ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    columnnames = names(RP$weights)
    numassets = length(columnnames)

    if(!is.na(neighbors)){
        xtract=extractStats.rp(RP)
        orderx = order(xtract[,"out"])
        subsetx = head(xtract[orderx,2:(numassets+1)], n=neighbors)
    }
    if(is.null(xlab))
        minmargin = 3
    else
        minmargin = 5
    if(main=="") topmargin=1 else topmargin=4
    if(las > 1) {# set the bottom border to accomodate labels
        bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
    }
    else {
        bottommargin = 5
    }
    par(mar = c(bottommargin, 4, topmargin, 2) +.1)
    plot(RP$random_portfolios[1,], type="b", col="orange", axes=FALSE, xlab="", ylim=c(0,max(RP$constraints$max)), ylab="Weights", main=main, ...)
    points(RP$constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
    points(RP$constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
    if(!is.na(neighbors)){
        for(i in 1:neighbors) points(subsetx[i,], type="b", col="lightblue")
        points(RP$random_portfolios[1,], type="b", col="orange", pch=16) # to overprint neighbors
    }
    points(RP$weights, type="b", col="red", pch=16)
    axis(2, cex.axis = cex.axis, col = element.color)
    axis(1, labels=names(RP$weights), at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
    box(col = element.color)

}

#' classic risk return scatter of random portfolios
#' @param RP set of random portfolios created by \code{\link{random_portfolios}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param ... any other passthru parameters 
#' @seealso \code{\link{random_portfolios}}
#' @export
chart.Scatter.RP <- function(RP, neighbors = NA, return.col='mean', risk.col='ES', element.color = "darkgray", cex.axis=0.8, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    xtract = extractStats.rp(RP)
    columnnames = colnames(xtract)
    return.column = grep(paste("objective_measures",return.col,sep='.'),columnnames)
    ## @TODO: Generalize this to find column containing the "risk" metric
    risk.column = grep(paste("objective_measures",risk.col,sep='.'),columnnames)

    plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="darkgray", axes=FALSE, ...)

    if(!is.na(neighbors)){ # overplot nearby portfolios
        orderx = order(xtract[,"out"]) #TODO this won't work if the objective is anything othchart.Scatter.er than mean
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
    points(RP$constrained_objective[risk.col], RP$constrained_objective[return.col], col="red", pch=16) # optimal
    axis(1, cex.axis = cex.axis, col = element.color)
    axis(2, cex.axis = cex.axis, col = element.color)
    box(col = element.color)
}

#' scatter and weights chart  for random portfolios
#' @param RP set of random portfolios created by \code{\link{random_portfolios}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param ... any other passthru parameters 
#' @seealso \code{\link{random_portfolios}}
#' @export
charts.RP <- function(RP, risk.col, return.col, neighbors=NA, las=3, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2)),height=c(2,1.5),width=1)
    par(mar=c(4,4,4,2))
    chart.Scatter.RP(RP, risk.col=risk.col, return.col=return.col, neighbors=neighbors, ...)
    par(mar=c(2,4,0,2))
    chart.Weights.RP(RP, main="", las=las, neighbors=neighbors, ...)
    par(op)

}