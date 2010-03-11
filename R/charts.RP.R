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

#' boxplot of the weight distributions in the random portfolios 
#' @param RP set of random portfolios created by \code{\link{optimize.portfolio}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param ... any other passthru parameters 
#' @seealso \code{\link{optimize.portfolio}}
#' @export
chart.Weights.RP <- function(RP, neighbors = NULL, las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, main="Weights", ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    columnnames = names(RP$weights)
    numassets = length(columnnames)

    if(is.null(xlab))
        minmargin = 3
    else
        minmargin = 5
    if(main=="") topmargin=1 else topmargin=4
    if(las > 1) {# set the bottom border to accommodate labels
        bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
        if(bottommargin > 10 ) {
            bottommargin<-10
            columnnames<-substr(columnnames,1,19)
            # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
        }
    }
    else {
        bottommargin = minmargin
    }
    par(mar = c(bottommargin, 4, topmargin, 2) +.1)
    plot(RP$random_portfolios[1,], type="b", col="orange", axes=FALSE, xlab='', ylim=c(0,max(RP$constraints$max)), ylab="Weights", main=main, ...)
    points(RP$constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
    points(RP$constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
    if(!is.null(neighbors)){ 
        if(is.vector(neighbors)){
            xtract=extractStats(RP)
            weightcols<-grep('w\\.',colnames(xtract)) #need \\. to get the dot 
            if(length(neighbors)==1){
                # overplot nearby portfolios defined by 'out'
                orderx = order(xtract[,"out"])
                subsetx = head(xtract[orderx,], n=neighbors)
                for(i in 1:neighbors) points(subsetx[i,weightcols], type="b", col="lightblue")
            } else{
                # assume we have a vector of portfolio numbers
                subsetx = xtract[neighbors,weightcols]
                for(i in 1:length(neighbors)) points(subsetx[i,weightcols], type="b", col="lightblue")
            }      
        }
        if(is.matrix(neighbors) | is.data.frame(neighbors)){
            # the user has likely passed in a matrix containing calculated values for risk.col and return.col
            nbweights<-grep('w\\.',colnames(neighbors)) #need \\. to get the dot
            for(i in 1:nrow(neighbors)) points(as.numeric(neighbors[i,nbweights]), type="b", col="lightblue")
            # note that here we need to get weight cols separately from the matrix, not from xtract
            # also note the need for as.numeric.  points() doesn't like matrix inputs
        }
    }
    
    points(RP$random_portfolios[1,], type="b", col="orange", pch=16) # to overprint neighbors
    points(RP$weights, type="b", col="blue", pch=16)
    axis(2, cex.axis = cex.axis, col = element.color)
    axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
    box(col = element.color)

}

#' classic risk return scatter of random portfolios
#' 
#' @param RP set of random portfolios created by \code{\link{optimize.portfolio}}
#' @param neighbors set of 'neighbor' portfolios to overplot, see Details
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param ... any other passthru parameters 
#' @seealso \code{\link{optimize.portfolio}}
#' @export
chart.Scatter.RP <- function(RP, neighbors = NULL, return.col='mean', risk.col='ES', element.color = "darkgray", cex.axis=0.8, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    xtract = extractStats(RP)
    columnnames = colnames(xtract)
    #return.column = grep(paste("objective_measures",return.col,sep='.'),columnnames)
    return.column = pmatch(return.col,columnnames)
    #risk.column = grep(paste("objective_measures",risk.col,sep='.'),columnnames)
    risk.column = pmatch(risk.col,columnnames)
    
    plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="darkgray", axes=FALSE, ...)

    if(!is.null(neighbors)){ 
        if(is.vector(neighbors)){
            if(length(neighbors)==1){
                # overplot nearby portfolios defined by 'out'
                orderx = order(xtract[,"out"]) #TODO this won't work if the objective is anything othchart.Scatter.er than mean
                subsetx = head(xtract[orderx,], n=neighbors)
            } else{
                # assume we have a vector of portfolio numbers
                subsetx = xtract[neighbors,]
            }
            points(subsetx[,risk.column], subsetx[,return.column], col="lightblue", pch=1)
        }
        if(is.matrix(neighbors) | is.data.frame(neighbors)){
            # the user has likely passed in a matrix containing calculated values for risk.col and return.col
            for(i in 1:nrow(neighbors)) points(neighbors[i,risk.col], neighbors[i,return.col], col="lightblue", pch=1)
        }
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
    if(length(names(RP)[which(names(RP)=='constrained_objective')])) {
        result.slot<-'constrained_objective'
    } else {
        result.slot<-'objective_measures'
    }
    points(RP[[result.slot]][risk.col], RP[[result.slot]][return.col], col="blue", pch=16) # optimal
    axis(1, cex.axis = cex.axis, col = element.color)
    axis(2, cex.axis = cex.axis, col = element.color)
    box(col = element.color)
}

#' scatter and weights chart  for random portfolios
#' 
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors.  This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' \code{risk.col},\code{return.col}, and weights columns all properly named.  
#' 
#' @param RP set of random portfolios created by \code{\link{optimize.portfolio}}
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param ... any other passthru parameters 
#' @seealso 
#' \code{\link{optimize.portfolio}}
#' \code{\link{extractStats}}
#' @export
charts.RP <- function(RP, risk.col, return.col, neighbors=NA, las=3, main="Random Portfolios", ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2)),height=c(2,1.5),width=1)
    par(mar=c(4,4,4,2))
    chart.Scatter.RP(RP, risk.col=risk.col, return.col=return.col, neighbors=neighbors, main=main, ...)
    par(mar=c(2,4,0,2))
    chart.Weights.RP(RP, main="", las=las, neighbors=neighbors, ...)
    par(op)
}

#TODO make chart.RP into a plot() method or methods

#' plot method
#' @export
plot.optimize.portfolio <- function(x,y,...,  return.col='mean', risk.col='ES',  neighbors=NA, main='portfolio plot') {
    charts.RP(RP=x, risk.col=risk.col, return.col=return.col, neighbors=neighbors, main=main, ...)
}