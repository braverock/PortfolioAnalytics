
#' @rdname chart.Weights
#' @export
chart.Weights.ROI <- function(object, neighbors = NULL, ..., main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8){

  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class 'optimize.portfolio.ROI'")
  
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
  #             xtract=extractStats(object)
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
  #     points(object$weights, type="b", col="blue", pch=16)
  axis(2, cex.axis = cex.axis, col = element.color)
  axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.Weights
#' @export
chart.Weights.optimize.portfolio.ROI <- chart.Weights.ROI

#' @rdname chart.RiskReward
#' @export
chart.Scatter.ROI <- function(object, neighbors=NULL, ..., rp=FALSE, return.col="mean", risk.col="ES", chart.assets=FALSE, element.color = "darkgray", cex.axis=0.8, xlim=NULL, ylim=NULL){
  
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class 'optimize.portfolio.ROI'")
  
  R <- object$R
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
  
  returnpoints <- applyFUN(R=R, weights=rp, FUN=return.col, ...=...)
  riskpoints <- applyFUN(R=R, weights=rp, FUN=risk.col, ...=...)
  
  if(chart.assets){
  # Include risk reward scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN=return.col, ...=...)
  asset_risk <- scatterFUN(R=R, FUN=risk.col, ...=...)
  rnames <- colnames(R)
  } else {
    asset_ret <- NULL
    asset_risk <- NULL
  }
  
  # get limits for x and y axis
  if(is.null(ylim)){
    ylim <- range(c(returnpoints, asset_ret))
  }
  if(is.null(xlim)){
    xlim <- range(c(riskpoints, asset_risk))
  }
  
  # Plot the portfolios
  plot(x=riskpoints, y=returnpoints, xlab=risk.col, ylab=return.col, col="darkgray", xlim=xlim, ylim=ylim, axes=FALSE, ...)
  # Plot the optimal portfolio
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
#' @export
chart.RiskReward.optimize.portfolio.ROI <- chart.Scatter.ROI

#' scatter and weights chart for portfolios
#' 
#' The ROI optimizers do not store the portfolio weights like DEoptim or random
#' portfolios so we will generate random portfolios for the scatter plot. 
#' 
#' \code{return.col} must be the name of a function used to compute the return metric on the random portfolio weights
#' \code{risk.col} must be the name of a function used to compute the risk metric on the random portfolio weights
#' 
#' @param ROI object created by \code{\link{optimize.portfolio}}
#' @param rp set of weights generated by \code{\link{random_portfolio}}
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param ... any other passthru parameters 
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param element.color color for the default plot scatter points
#'  @param neighbors set of 'neighbor' portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
charts.ROI <- function(ROI, rp=FALSE, risk.col="ES", return.col="mean", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="ROI.Portfolios", xlim=NULL, ylim=NULL, ...){
  # Specific to the output of the optimize_method=ROI
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),height=c(2,1.5),width=1)
  par(mar=c(4,4,4,2))
  chart.Scatter.ROI(object=ROI, rp=rp, return.col=return.col, risk.col=risk.col, ..., chart.assets=chart.assets, element.color=element.color, cex.axis=cex.axis, main=main, xlim=xlim, ylim=ylim)
  par(mar=c(2,4,0,2))
  chart.Weights.ROI(object=ROI, neighbors=neighbors, ..., main="", las=3, xlab=NULL, cex.lab=1, element.color=element.color, cex.axis=cex.axis)
  par(op)
}

#' scatter and weights chart for portfolios
#' 
#' The ROI optimizers do not store the portfolio weights like DEoptim or random
#' portfolios so we will generate random portfolios for the scatter plot. 
#' 
#' \code{return.col} must be the name of a function used to compute the return metric on the random portfolio weights
#' \code{risk.col} must be the name of a function used to compute the risk metric on the random portfolio weights
#' 
#' @param ROI object created by \code{\link{optimize.portfolio}}
#' @param rp set of weights generated by \code{\link{random_portfolio}}
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param ... any other passthru parameters 
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param element.color color for the default plot scatter points
#'  @param neighbors set of 'neighbor' portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
plot.optimize.portfolio.ROI <- function(ROI, rp=FALSE, risk.col="ES", return.col="mean", chart.assets=chart.assets, element.color="darkgray", neighbors=NULL, main="ROI.Portfolios", xlim=NULL, ylim=NULL, ...){
  charts.ROI(ROI=ROI, rp=rp, risk.col=risk.col, return.col=return.col, chart.assets=chart.assets, main=main, xlim=xlim, ylim=ylim, ...)
}
