
#' boxplot of the weights of the optimal portfolios
#' 
#' This function charts the optimal weights of a portfolio run via 
#' \code{\link{optimize.portfolio}} or \code{\link{optimize.portfolio.rebalancing}}.
#' The upper and lower bounds on weights can be plotted for single period optimizations.
#' The optimal weights will be charted through time for \code{optimize.portfolio.rebalancing}
#' objects. For \code{optimize.portfolio.rebalancing} objects, the weights are
#' plotted with \code{\link[PerformanceAnalytics]{chart.StackedBar}}.
#' 
#' @param object optimal portfolio object created by \code{\link{optimize.portfolio}}.
#' @param neighbors set of 'neighbor' portfolios to overplot. See Details.
#' @param \dots any other passthru parameters .
#' @param main an overall title for the plot: see \code{\link{title}}
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis,}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical [\emph{default}].}
#'       }
#' @param xlab a title for the x axis: see \code{\link{title}}
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of \code{cex}
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param colorset color palette or vector of colors to use.
#' @param legend.loc location of the legend. If NULL, the legend will not be plotted.
#' @param cex.legend The magnification to be used for legend annotation relative to the current setting of \code{cex}.
#' @param plot.type "line" or "barplot" to plot.
#' @seealso \code{\link{optimize.portfolio}} \code{\link{optimize.portfolio.rebalancing}} \code{\link[PerformanceAnalytics]{chart.StackedBar}}
#' @name chart.Weights
#' @export
chart.Weights <- function(object, ...){
  UseMethod("chart.Weights")
}

barplotWeights <- function(object, ..., main="Weights", las=3, xlab=NULL, cex.lab=1, element.color="darkgray", cex.axis=0.8, legend.loc="topright", cex.legend=0.8, colorset=NULL){
  weights <- object$weights
  columnnames <- names(weights)
  
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
  
  if(is.null(colorset)) colorset <- 1:length(weights)
  barplot(height=weights, las=las, main=main, xlab=xlab, ylab="Weights", cex.axis=cex.axis, cex.names=cex.lab, col=colorset, ...)
  if(!is.null(legend.loc)){
    legend(legend.loc, legend=names(weights), cex=cex.legend, fill=colorset, bty="n")
  }
  box(col=element.color)
}


barplotWeights <- function(object, ..., main="Weights", las=3, xlab=NULL, cex.lab=1, element.color="darkgray", cex.axis=0.8, legend.loc="topright", cex.legend=0.8, colorset=NULL){
  weights <- object$weights
  columnnames <- names(weights)
  
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
  
  if(is.null(colorset)) colorset <- 1:length(weights)
  barplot(height=weights, las=las, main=main, xlab=xlab, ylab="Weights", cex.axis=cex.axis, cex.names=cex.lab, col=colorset, ...)
  if(!is.null(legend.loc)){
    legend(legend.loc, legend=names(weights), cex=cex.legend, fill=colorset, bty="n")
  }
  box(col=element.color)
}

#' @rdname chart.Weights
#' @method chart.Weights optimize.portfolio.rebalancing
#' @S3method chart.Weights optimize.portfolio.rebalancing
chart.Weights.optimize.portfolio.rebalancing <- function(object, ..., main="Weights"){
  rebal.weights <- extractWeights(object)
  chart.StackedBar(w=rebal.weights, main=main, ...)
}


###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2018 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
