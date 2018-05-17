

#' classic risk reward scatter
#' 
#' This function charts the \code{optimize.portfolio} object in risk-return space.
#' 
#' @details
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors.  This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' \code{risk.col},\code{return.col}, and weights columns all properly named. 
#' 
#' @param object optimal portfolio created by \code{\link{optimize.portfolio}}.
#' @param neighbors set of 'neighbor' portfolios to overplot, see Details.
#' @param \dots any other passthru parameters.
#' @param return.col string matching the objective of a 'return' objective, on vertical axis.
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis.
#' @param chart.assets TRUE/FALSE. Includes a risk reward scatter of the assets in the chart.
#' @param element.color color for the default plot scatter points.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}.
#' @param rp TRUE/FALSE to generate random portfolios to plot the feasible space
#' @param main a main title for the plot.
#' @param labels.assets TRUE/FALSE to include the names in the plot. 
#' @param pch.assets plotting character of the assets, same as in \code{\link{plot}}
#' @param cex.assets numerical value giving the amount by which the asset points should be magnified relative to the default.
#' @param cex.lab numerical value giving the amount by which the labels should be magnified relative to the default.
#' @param colorset color palette or vector of colors to use.
#' @seealso \code{\link{optimize.portfolio}}
#' @export
chart.RiskReward <- function(object, ...){
  UseMethod("chart.RiskReward")
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
