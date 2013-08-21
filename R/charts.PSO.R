
#' @rdname chart.Weights
#' @export
chart.Weights.pso <- function(object, neighbors = NULL, ..., main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8){
  
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class 'optimize.portfolio.pso'")
  
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
  plot(object$weights, type="b", col="blue", axes=FALSE, xlab='', ylim=c(0,max(constraints$max)), ylab="Weights", main=main, pch=16, ...)
  points(constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
  points(constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
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

#' @rdname chart.Weights
#' @export
chart.Weights.optimize.portfolio.pso <- chart.Weights.pso

#' @rdname chart.RiskReward
#' @export
chart.Scatter.pso <- function(object, neighbors=NULL, ..., return.col="mean", risk.col="ES", chart.assets=FALSE, element.color = "darkgray", cex.axis=0.8, xlim=NULL, ylim=NULL){
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class 'optimize.portfolio.pso'")
  R <- object$R
  # Object with the "out" value in the first column and the normalized weights
  # The first row is the optimal "out" value and the optimal weights
  tmp <- extractStats(object)
  
  # Get the weights
  wts <- tmp[,-1]
  
  returnpoints <- applyFUN(R=R, weights=wts, FUN=return.col, ...=...)
  riskpoints <- applyFUN(R=R, weights=wts, FUN=risk.col, ...=...)
  
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
    ylim <- range(returnpoints, asset_ret)
  }
  if(is.null(xlim)){
    xlim <- range(riskpoints, asset_risk)
  }
  
  # plot the portfolios
  plot(x=riskpoints, y=returnpoints, xlab=risk.col, ylab=return.col, xlim=xlim, ylim=ylim, col="darkgray", axes=FALSE, ...)
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
chart.RiskReward.optimize.portfolio.pso <- chart.Scatter.pso

#' scatter and weights chart for portfolios
#' 
#' \code{return.col} must be the name of a function used to compute the return metric on the random portfolio weights
#' \code{risk.col} must be the name of a function used to compute the risk metric on the random portfolio weights
#' 
#' @param pso object created by \code{\link{optimize.portfolio}}
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param ... any other passthru parameters 
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param element.color color for the default plot scatter points
#'  @param neighbors set of 'neighbor' portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
charts.pso <- function(pso, return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="PSO.Portfolios", xlim=NULL, ylim=NULL, ...){
  # Specific to the output of the optimize_method=pso
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),height=c(2,2),width=1)
  par(mar=c(4,4,4,2))
  chart.Scatter.pso(object=pso, return.col=return.col, risk.col=risk.col, chart.assets=chart.assets, element.color=element.color, cex.axis=cex.axis, main=main, xlim=xlim, ylim=ylim, ...=...)
  par(mar=c(2,4,0,2))
  chart.Weights.pso(object=pso, neighbors=neighbors, las=3, xlab=NULL, cex.lab=1, element.color=element.color, cex.axis=cex.axis, ...=..., main="")
  par(op)
}

#' scatter and weights chart for portfolios
#' 
#' \code{return.col} must be the name of a function used to compute the return metric on the random portfolio weights
#' \code{risk.col} must be the name of a function used to compute the risk metric on the random portfolio weights
#' 
#' @param pso object created by \code{\link{optimize.portfolio}}
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param ... any other passthru parameters 
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param element.color color for the default plot scatter points
#'  @param neighbors set of 'neighbor' portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
plot.optimize.portfolio.pso <- function(pso, return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="PSO.Portfolios", xlim=NULL, ylim=NULL, ...){
  charts.pso(pso=pso, return.col=return.col, risk.col=risk.col, chart.assets=FALSE, cex.axis=cex.axis, element.color=element.color, neighbors=neighbors, main=main, xlim=xlim, ylim=ylim, ...=...)
}
