#' Chart weights by group or category
#' 
#' @param object object of class \code{optimize.portfolio}.
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param grouping
#' \itemize{
#'   \item{groups: }{group the weights by group constraints.}
#'   \item{category_labels: }{group the weights by category_labels in the \code{portfolio} object.}
#' }
#' @param plot.type "line" or "barplot".
#' @param main an overall title for the plot: see \code{\link{title}}.
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis,}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical[\emph{default}].}
#'       }
#' @param xlab a title for the x axis: see \code{\link{title}}.
#' @param cex.lab the magnification to be used for x and y labels relative to the current setting of \code{cex}.
#' @param element.color color for the default border and axis.
#' @param cex.axis the magnification to be used for x and y axis relative to the current setting of \code{cex}.
#' @author Ross Bennett
#' @export
chart.GroupWeights <- function(object,  ..., grouping=c("groups", "category"), plot.type="line", main="Group Weights", las=3, xlab=NULL, cex.lab=0.8, element.color="darkgray", cex.axis=0.8){
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class 'optimize.portfolio'")
  
  if(plot.type %in% c("bar", "barplot")){
    barplotGroupWeights(object=object, ...=..., grouping=grouping, main=main, 
                        las=las, xlab=xlab, cex.lab=cex.lab, 
                        element.color=element.color, cex.axis=cex.axis)
  } else if(plot.type == "line"){
    constraints <- get_constraints(object$portfolio)
    tmp <- extractGroups(object)
    grouping <- grouping[1]
    
    if(grouping == "groups"){
      weights <- tmp$group_weights
      if(is.null(weights)) stop("No weights detected for groups")
      if(any(is.infinite(constraints$cUP)) | any(is.infinite(constraints$cLO))){
        # set ylim based on weights if box constraints contain Inf or -Inf
        ylim <- range(weights)
      } else {
        # set ylim based on the range of box constraints min and max
        ylim <- range(c(constraints$cLO, constraints$cUP))
      }
    }
    
    if(grouping == "category"){
      weights <- tmp$category_weights
      if(is.null(weights)) stop("No weights detected for category")
      ylim <- range(weights)
    }
    
    columnnames = names(weights)
    numgroups = length(columnnames)
    
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
      }
    }
    else {
      bottommargin = minmargin
    }
    par(mar = c(bottommargin, 4, topmargin, 2) +.1)
    
    plot(weights, axes=FALSE, xlab='', ylim=ylim, ylab="Weights", main=main, ...)
    if(grouping == "groups"){
      if(!any(is.infinite(constraints$cLO))){
        points(constraints$cLO, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
      }
      if(!any(is.infinite(constraints$cUP))){
        points(constraints$cUP, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
      }
    }
    axis(2, cex.axis = cex.axis, col = element.color)
    axis(1, labels=columnnames, at=1:numgroups, las=las, cex.axis = cex.axis, col = element.color)
    box(col = element.color)
  }
}

#' barplot of group weights by group or category
#' 
#' This function is called by chart.GroupWeights function if chart.type="barplot"
#' 
#' @param object object of class \code{optimize.portfolio}
#' @param ... passthrough parameters to \code{\link{plot}}
#' @param grouping
#' \itemize{
#'   \item{groups: }{group the weights by group constraints}
#'   \item{category_labels: }{group the weights by category_labels in portfolio object}
#' }
#' @param main an overall title for the plot: see \code{\link{title}}
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis [\emph{default}],}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical.}
#'       }
#' @param xlab a title for the x axis: see \code{\link{title}}
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of \code{cex}
#' @param element.color color for the default border and axis
#' @param cex.axis The magnification to be used for x and y axis relative to the current setting of \code{cex}
#' @author Ross Bennett
barplotGroupWeights <- function(object,  ..., grouping=c("groups", "category"), main="Group Weights", las=3, xlab=NULL, cex.lab=0.8, element.color="darkgray", cex.axis=0.8){
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class 'optimize.portfolio'")
  
  constraints <- get_constraints(object$portfolio)
  tmp <- extractGroups(object)
  
  if(grouping == "groups"){
    weights <- tmp$group_weights
    if(is.null(weights)) stop("No weights detected for groups")
  }
  
  if(grouping == "category"){
    weights <- tmp$category_weights
    if(is.null(weights)) stop("No weights detected for category")
  }
  
  columnnames = names(weights)
  numgroups = length(columnnames)
  
  barplot(weights, ylab = "", names.arg=columnnames,
          border=element.color, cex.axis=cex.axis, main=main, las=las, 
          cex.names=cex.lab, xlab=xlab, ...)
  box(col=element.color)
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
