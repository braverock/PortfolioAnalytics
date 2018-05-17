
chart.Weights.GenSA <- function(object, ..., neighbors = NULL, main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8, plot.type="line"){
  
  if(!inherits(object, "optimize.portfolio.GenSA")) stop("object must be of class 'optimize.portfolio.GenSA'")
  
  if(plot.type %in% c("bar", "barplot")){
    barplotWeights(object=object, ..., main=main, las=las, xlab=xlab, cex.lab=cex.lab, element.color=element.color, cex.axis=cex.axis, legend.loc=legend.loc, cex.legend=cex.legend, colorset=colorset)
  } else if(plot.type == "line"){
    
    columnnames = names(object$weights)
    numassets = length(columnnames)
    
    constraints <- get_constraints(object$portfolio)
    
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
    if(any(is.infinite(constraints$max)) | any(is.infinite(constraints$min))){
      # set ylim based on weights if box constraints contain Inf or -Inf
      ylim <- range(object$weights)
    } else {
      # set ylim based on the range of box constraints min and max
      ylim <- range(c(constraints$min, constraints$max))
    }
    plot(object$weights, type="b", col="blue", axes=FALSE, xlab='', ylim=ylim, ylab="Weights", main=main, pch=16, ...)
    if(!any(is.infinite(constraints$min))){
      points(constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
    }
    if(!any(is.infinite(constraints$max))){
      points(constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
    }
    #     if(!is.null(neighbors)){ 
    #         if(is.vector(neighbors)){
    #             xtract=extractStats(ROI)
    #             weightcols<-grep('w\\.',colnames(xtract)) #need \\. to get the dot 
    #             if(length(neighbors)==1){
    #                 # overplot nearby portfolios defined by 'out'
    #                 orderx = order(xtract[,"out"])
    #                 subsetx = head(xtract[orderx,], n=neighbors)
    #                 for(i in 1:neighbors) points(subsetx[i,weightcols], type="b", col="lightblue")
    #             } else{
    #                 # assume we have a vector of portfolio numbers
    #                 subsetx = xtract[neighbors,weightcols]
    #                 for(i in 1:length(neighbors)) points(subsetx[i,], type="b", col="lightblue")
    #             }      
    #         }
    #         if(is.matrix(neighbors) | is.data.frame(neighbors)){
    #             # the user has likely passed in a matrix containing calculated values for risk.col and return.col
    #             nbweights<-grep('w\\.',colnames(neighbors)) #need \\. to get the dot
    #             for(i in 1:nrow(neighbors)) points(as.numeric(neighbors[i,nbweights]), type="b", col="lightblue")
    #             # note that here we need to get weight cols separately from the matrix, not from xtract
    #             # also note the need for as.numeric.  points() doesn't like matrix inputs
    #         }
    #     }
    #     points(ROI$weights, type="b", col="blue", pch=16)
    axis(2, cex.axis = cex.axis, col = element.color)
    axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
    box(col = element.color)
  }
}

#' @rdname chart.Weights
#' @method chart.Weights optimize.portfolio.GenSA
#' @S3method chart.Weights optimize.portfolio.GenSA
chart.Weights.optimize.portfolio.GenSA <- chart.Weights.GenSA

chart.Scatter.GenSA <- function(object, ..., neighbors=NULL, return.col="mean", risk.col="ES", chart.assets=FALSE, element.color="darkgray", cex.axis=0.8, ylim=NULL, xlim=NULL, rp=FALSE){
  
  if(!inherits(object, "optimize.portfolio.GenSA")) stop("object must be of class 'optimize.portfolio.GenSA'")
  
  R <- object$R
  if(is.null(R)) stop("Returns object not detected, must run optimize.portfolio with trace=TRUE")
  # If the user does not pass in rp, then we will generate random portfolios
  if(rp){
    permutations <- match.call(expand.dots=TRUE)$permutations
    if(is.null(permutations)) permutations <- 2000
    rp <- random_portfolios(portfolio=object$portfolio, permutations=permutations)
  } else {
    rp = NULL
  }
  
  # Get the optimal weights from the output of optimize.portfolio
  wts <- object$weights
  
  # cbind the optimal weights and random portfolio weights
  rp <- rbind(wts, rp)
  
  # Get the arguments from the optimize.portfolio$portfolio object
  # to calculate the risk and return metrics for the scatter plot
  tmp.args <- unlist(lapply(object$portfolio$objectives, function(x) x$arguments), recursive=FALSE)
  tmp.args <- tmp.args[!duplicated(names(tmp.args))]
  if(!is.null(tmp.args$portfolio_method)) tmp.args$portfolio_method <- "single"
  arguments <- tmp.args
  
  returnpoints <- applyFUN(R=R, weights=rp, FUN=return.col, arguments)
  riskpoints <- applyFUN(R=R, weights=rp, FUN=risk.col, arguments)
  
  if(chart.assets){
    # Include risk reward scatter of asset returns
    asset_ret <- scatterFUN(R=R, FUN=return.col, arguments)
    asset_risk <- scatterFUN(R=R, FUN=risk.col, arguments)
  } else {
    asset_ret <- NULL
    asset_risk <- NULL
  }
  
  # get limits for x and y axis
  if(is.null(ylim)){
    ylim <- range(returnpoints, asset_ret)
  }
  if(is.null(xlim)){
    xlim <- range(riskpoints, asset_risk)
  }
  
  # Plot the portfolios
  plot(x=riskpoints, y=returnpoints, xlab=risk.col, ylab=return.col, col="darkgray", ylim=ylim, xlim=xlim, axes=FALSE, ...)
  points(x=riskpoints[1], y=returnpoints[1], col="blue", pch=16) # optimal
  text(x=riskpoints[1], y=returnpoints[1], labels="Optimal",col="blue", pos=4, cex=0.8)
  
  # plot the risk-reward scatter of the assets
  if(chart.assets){
    points(x=asset_risk, y=asset_ret)
    text(x=asset_risk, y=asset_ret, labels=colnames(R), pos=4, cex=0.8)
  }
  
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.RiskReward
#' @method chart.RiskReward optimize.portfolio.GenSA
#' @S3method chart.RiskReward optimize.portfolio.GenSA
chart.RiskReward.optimize.portfolio.GenSA <- chart.Scatter.GenSA


charts.GenSA <- function(GenSA, rp=FALSE, return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="GenSA.Portfolios", xlim=NULL, ylim=NULL, ...){
  # Specific to the output of the optimize_method=GenSA
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),heights=c(2,2),widths=1)
  par(mar=c(4,4,4,2))
  chart.Scatter.GenSA(object=GenSA, rp=rp, return.col=return.col, risk.col=risk.col, chart.assets=chart.assets, element.color=element.color, cex.axis=cex.axis, main=main, xlim=xlim, ylim=ylim, ...=...)
  par(mar=c(2,4,0,2))
  chart.Weights.GenSA(object=GenSA, neighbors=neighbors, las=3, xlab=NULL, cex.lab=1, element.color=element.color, cex.axis=cex.axis, ...=..., main="")
  par(op)
}


#' @rdname plot
#' @method plot optimize.portfolio.GenSA
#' @S3method plot optimize.portfolio.GenSA
plot.optimize.portfolio.GenSA <- function(x, ..., rp=FALSE, return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="GenSA.Portfolios", xlim=NULL, ylim=NULL){
  charts.GenSA(GenSA=x, rp=rp, return.col=return.col, risk.col=risk.col, chart.assets=chart.assets, cex.axis=cex.axis, element.color=element.color, neighbors=neighbors, main=main, xlim=xlim, ylim=ylim, ...=...)
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
