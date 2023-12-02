###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2022-2032 Xinran Zhao
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' Generate efficient frontiers plot by providing frontiers.
#' 
#' @details
#' This function provides the ability to plot frontiers. 
#' 
#' When using \code{meanvar.efficient.frontier}, \code{meanetl.efficient.frontier} 
#' and \code{meaneqs.efficient.frontier}, the result will be frontiers data,
#' including the weights for each point on the mean-risk efficient frontiers. 
#' Before using this function, user should declare which risk that they want to 
#' compare, and what parameters that they want to use to calculate the risk, 
#' e.g. \code{ES_alpha} for ES, \code{moment_setting} for var. Then this function 
#' will calculate back mean and risk based on the weight, and draw a plot.
#' 
#' Default settings use colors and line types to differentiate portfolios, and set 
#' the portfolio name as 'Portfolio 1' and so on. Users could customize col, lty, 
#' lwd and legend.labels to better the plot.
#' 
#' @param R an xts object of asset returns
#' @param frontiers a list of frontiers, for example, list(ef1=meanvar.efficient.frontier(), ef2=meanvar.efficient.frontier())
#' @param risk type of risk that you want to compare, could be 'StdDev', 'ES', 'EQS'
#' @param ES_alpha the default value is 0.05, but could be specified as any value between 0 and 1
#' @param EQS_alpha the default value is 0.05, but could be specified as any value between 0 and 1
#' @param moment_setting the default is NULL, if customize momentFUN please provide moment_setting=list(mu=, sigma=) 
#' @param main title used in the plot.
#' @param plot_type define the plot_type, default is "l"
#' @param cex.axis the magnification to be used for sizing the axis text relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param legend.loc location of the legend; NULL, "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param legend.labels character vector to use for the legend labels.
#' @param cex.legend The magnification to be used for sizing the legend relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}.
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param labels.assets TRUE/FALSE to include the asset names in the plot.
#' @param pch.assets plotting character of the assets, same as in \code{\link{plot}}.
#' @param cex.assets A numerical value giving the amount by which the asset points and labels should be magnified relative to the default.
#' @param col vector of colors with length equal to the number of portfolios in \code{frontiers}.
#' @param lty vector of line types with length equal to the number of portfolios in \code{frontiers}.
#' @param lwd vector of line widths with length equal to the number of portfolios in \code{frontiers}.
#' @author Xinran Zhao
#' @export plotFrontiers
#' 
plotFrontiers <- function(R, frontiers, risk, ES_alpha = 0.05, EQS_alpha = 0.05, moment_setting = NULL, main="Efficient Frontiers", plot_type = "l", cex.axis=0.5, element.color="darkgray", legend.loc=NULL, legend.labels=NULL, cex.legend=0.8, xlim=NULL, ylim=NULL, ..., labels.assets=TRUE, pch.assets=21, cex.assets=0.8, col=NULL, lty=NULL, lwd=NULL){
  if(risk %in% c('StdDev', 'var', 'stdDev')){
    risk = 'StdDev'
  } else if(risk %in% c('etl', 'ES', 'es')){
    risk = 'ES'
  } else if(risk %in% c('EQS', 'eqs')){
    risk = 'EQS'
  } else {print('please give the right risk type')}
  
  wname = paste0('w.', colnames(R))
  n = length(frontiers)
  mean_list = list()
  risk_list = list()
  mean_scale = list('max' = c(), 'min' = c())
  risk_scale = list('max' = c(), 'min' = c())
  
  for(i in 1:n){
    if(risk %in% colnames(frontiers[[i]])){
      mean_value = as.numeric(frontiers[[i]][,'mean'])
      risk_value = as.numeric(frontiers[[i]][,risk])
    } else {
      w = frontiers[[i]][, wname]
      mean_value = c()
      risk_value = c()
      for(j in 1:dim(w)[1]){
        risk_measures = extract_risk(R, w[j,], ES_alpha = ES_alpha, EQS_alpha = EQS_alpha, moment_setting = moment_setting)
        mean_value = append(mean_value, risk_measures$mean)
        risk_value = append(risk_value, risk_measures[risk])
      }
    }
    mean_list = c(mean_list, list(mean_value))
    risk_list = c(risk_list, list(risk_value))
    mean_scale$max = append(mean_scale$max, max(mean_value))
    mean_scale$min = append(mean_scale$min, min(mean_value))
    risk_scale$max = append(risk_scale$max, max(risk_value))
    risk_scale$min = append(risk_scale$min, min(risk_value))
  }
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- c(0, 0)
    xlim[1] <- min(risk_scale$min) * 0.9
    xlim[2] <- max(risk_scale$max) * 1.1
  }
  if(is.null(ylim)){
    ylim <- c(0, 0)
    ylim[1] <- min(mean_scale$min) * 0.7
    ylim[2] <- max(mean_scale$max) * 1.1
  }
  
  # plot the assets
  plot(x=1, y=1, xlab=risk, ylab="Mean", main=main, xlim=xlim, ylim=ylim, axes=FALSE, type="n", ...)
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
  
  # set some basic plot parameters
  if(is.null(col)) col <- 1:n
  if(is.null(lty)) lty <- 1:n
  if(is.null(lwd)) lwd <- rep(1, n)
  
  for(i in 1:n){
    lines(x=risk_list[[i]], y=mean_list[[i]], col=col[i], lty=lty[i], lwd=lwd[i], type = plot_type, ...)
  }
  
  # legend
  if(!is.null(legend.loc)){
    legend.loc = "bottomright"
  }
  if(is.null(legend.labels)){
    legend.labels <- paste("Portfolio", 1:n)
  }
  legend("bottomright", legend=legend.labels, col=col, lty=lty, lwd=lwd, cex=cex.legend, bty="n")
  out = list(mean = mean_list, risk = risk_list)
  return(invisible(out))
}

