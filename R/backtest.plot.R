###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2022-2032 Peter Carl, Yifu Kang, Xinran Zhao, Doug Martin
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


#' generate plots of the cumulative returns and drawdown for back-testing
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param log_return arithmetic return or log return, the default is arithmetic return
#' @param plotType "cumRet", "drawdown", or the default is both
#' @param colorSet users can design the color by providing a vector of color
#' @param ltySet users can design lty by providing a vector of lty
#' @param lwdSet users can design lwd by providing a vector of lwd
#' @importFrom stats end
#' @importFrom grDevices col2rgb
#' @author Peter Carl, Xinran Zhao, Yifu Kang
#' @export backtest.plot
backtest.plot <- function(R, log_return = FALSE, plotType='both', colorSet=NULL, ltySet=NULL, lwdSet=NULL){
  ## Cumulative Returns
  c.xts <- if (log_return) {
    1 + cumsum(R)
  } else {
    cumprod(1+R)
  }
  n = dim(c.xts)[2] ## number of portfolio
  
  ## sort xts
  legendOrder = order(c.xts[end(c.xts)], decreasing = TRUE)
  
  ## Drawdowns
  d.xts <- PerformanceAnalytics::Drawdowns(R)
  
  ## get longest drawdown dates for xts object, which is the worst drawdown
  dt <- table.Drawdowns(R, top = 1)
  if(is.na(dt$To) == TRUE){
    dt$To = index(R)[dim(R)[1]]
  }
  dt2 <- t(dt[,c("From", "To")])
  x <- as.vector(dt2[,NCOL(dt2)])
  
  ## style set
  if (is.null(colorSet))
    colorSet <- 1:n
  if (is.null(ltySet))
    ltySet <- rep(1, n)
  if (is.null(lwdSet))
    lwdSet <- rep(2, n)
  
  ## plots of return and drawdown
  if (plotType == 'both'){
    p <- xts::plot.xts(c.xts[,1], main = "Cumulative Returns",
                       grid.ticks.lwd=1, grid.ticks.on = "years",
                       labels.col="grey20", col = colorSet[1], lty = ltySet[1],
                       lwd = lwdSet[1], cex.axis=0.8, format.labels = "%b\n%Y", 
                       ylim = c(min(c.xts), max(c.xts)))
    p <- xts::addSeries(d.xts[,1], main="Drawdown", ylim = c(min(d.xts), 0),
                        col = colorSet[1], lty = ltySet[1], lwd = lwdSet[1])
    if(n > 1){
      for(i in 2:n){
        p <- xts::addSeries(c.xts[,i], on=1, col = colorSet[i], lty = ltySet[i], lwd = lwdSet[i])
        p <- xts::addSeries(d.xts[,i], on=2, col = colorSet[i], lty = ltySet[i], lwd = lwdSet[i])
      }
    }
    p <- xts::addLegend("topleft", on = 1, legend.names = names(c.xts)[legendOrder], bty = "o", 
                        box.col = "white", col = colorSet[legendOrder], lty = ltySet[legendOrder], 
                        lwd = lwdSet[legendOrder],
                        bg=rgb(t(col2rgb("white")), alpha = 200, maxColorValue = 255))
    
    ## ylim panel
    ylim1 <- c(min(c.xts), max(c.xts))
    ylim2 <- c(min(d.xts), 0)
    xy1 <- as.xts(matrix(rep(ylim1, length(x)),ncol=length(ylim1), byrow=TRUE), 
                       order.by=as.Date(x))
    xy2 <- as.xts(matrix(rep(ylim2, length(x)),ncol=length(ylim2), byrow=TRUE), 
                       order.by=as.Date(x))
    p <- xts::addPolygon(xy1, on=-1, col="lightgrey") # top panel
    p <- xts::addPolygon(xy2, on=-2, col="lightgrey") # lower panel
  }
  
  ## plot of returns
  if (plotType == 'ret' || plotType == 'cumGrossRet' || plotType == 'cumRet' || plotType == 'cumret'){
    p <- xts::plot.xts(c.xts[,1], main = "Cumulative Returns",
                       grid.ticks.lwd=1, grid.ticks.on = "years", cex.axis=0.8, 
                       col = colorSet[1], lty = ltySet[1], lwd = lwdSet[1],
                       format.labels = "%b\n%Y", labels.col="grey20", 
                       ylim = c(min(c.xts), max(c.xts)))
    if(n > 1){
      for(i in 2:n){
        p <- xts::addSeries(c.xts[,i], on=1, col = colorSet[i], lty = ltySet[i], lwd = lwdSet[i])
      }
    }
    p <- xts::addLegend("topleft", on = 1, legend.names = names(c.xts)[legendOrder], bty = "o",
                        box.col = "white", col = colorSet[legendOrder], lty = ltySet[legendOrder], 
                        lwd = lwdSet[legendOrder],
                        bg=rgb(t(col2rgb("white")), alpha = 200, maxColorValue = 255))
    
    ## ylim panel
    ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2])
    xy1 <- as.xts(matrix(rep(ylim1, length(x)),ncol=length(ylim1), byrow=TRUE), 
                  order.by=as.Date(x))
    p <- xts::addPolygon(xy1, on=-1, col="lightgrey")
  }
  
  ## plot of drawdown
  if (plotType == 'drawdown'){
    p <- xts::plot.xts(d.xts[,1], main="Drawdown",
                       grid.ticks.lwd=1, grid.ticks.on = "years", cex.axis=0.8, 
                       col = colorSet[1], lty = ltySet[1], lwd = lwdSet[1], 
                       format.labels = "%b\n%Y", labels.col="grey20", 
                       ylim = c(min(d.xts), 0.1))
    if(n > 1){
      for(i in 2:n){
        p <- xts::addSeries(d.xts[,i], on=1, col = colorSet[i], lty = ltySet[i], lwd = lwdSet[i])
      }
    }
    p <- xts::addLegend("topleft", on = 1, legend.names = names(c.xts)[legendOrder], bty = "o",
                        box.col = "white", col = colorSet[legendOrder], lty = ltySet[legendOrder], 
                        lwd = lwdSet[legendOrder],
                        bg=rgb(t(col2rgb("white")), alpha = 200, maxColorValue = 255))
    
    ## ylim panel
    ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2])
    xy1 <- as.xts(matrix(rep(ylim1, length(x)),ncol=length(ylim1), byrow=TRUE), 
                  order.by=as.Date(x))
    p <- xts::addPolygon(xy1, on=-1, col="lightgrey") # top panel
  }
  
  return(p)
}
