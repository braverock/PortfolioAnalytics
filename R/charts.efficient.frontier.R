
#' chart the efficient frontier and risk-return scatter plot of the assets
#' 
#' This function charts the efficient frontier and risk-return scatter plot of 
#' the assets given an object created by \code{optimize.portfolio}.
#' 
#' For objects created by optimize.portfolio with 'DEoptim', 'random', or 'pso'
#' specified as the optimize_method:
#' \itemize{
#'   \item The efficient frontier plotted is based on the the trace information (sets of 
#'   portfolios tested by the solver at each iteration) in objects created by 
#'   \code{optimize.portfolio}.
#' }
#' 
#' For objects created by optimize.portfolio with 'ROI' specified as the 
#' optimize_method:
#' \itemize{
#'   \item The mean-var or mean-etl efficient frontier can be plotted for optimal
#'   portfolio objects created by \code{optimize.portfolio}.
#' 
#'   \item If \code{match.col="var"}, the mean-variance efficient frontier is plotted.
#' 
#'   \item If \code{match.col="ETL"} (also "ES" or "CVaR"), the mean-etl efficient frontier is plotted.
#' }
#' 
#' Note that \code{trace=TRUE} must be specified in \code{\link{optimize.portfolio}}
#' 
#' GenSA does not return any useable trace information for portfolios tested at 
#' each iteration, therfore we cannot extract and chart an efficient frontier.
#' 
#' @param object optimal portfolio created by \code{\link{optimize.portfolio}}
#' @param match.col string name of column to use for risk (horizontal axis). 
#' \code{match.col} must match the name of an objective in the \code{portfolio}
#' object.
#' @param n.portfolios number of portfolios to use to plot the efficient frontier
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param cex.axis
#' @param element.color 
#' @param main a main title for the plot
#' @param ... passthrough parameters to \code{\link{plot}}
#' @author Ross Bennett
#' @export
chart.EfficientFrontier <- function(object, match.col="ES", n.portfolios=25, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", ...){
  UseMethod("chart.EfficientFrontier")
}

#' @rdname chart.EfficientFrontier
#' @export
chart.EfficientFrontier.optimize.portfolio.ROI <- function(object, match.col="ES", n.portfolios=25, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", ...){
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class optimize.portfolio.ROI")
  
  portf <- object$portfolio
  R <- object$R
  if(is.null(R)) stop(paste("Not able to get asset returns from", object))
  wts <- object$weights
  objectclass <- class(object)[1]
  
  objnames <- unlist(lapply(portf$objectives, function(x) x$name))
  if(!(match.col %in% objnames)){
    stop("match.col must match an objective name")
  }
  
  # get the optimal return and risk metrics
  xtract <- extractStats(object=object)
  columnames <- colnames(xtract)
  if(!(("mean") %in% columnames)){
    # we need to calculate the mean given the optimal weights
    opt_ret <- applyFUN(R=R, weights=wts, FUN="mean")
  } else {
    opt_ret <- xtract["mean"]
  }
  opt_risk <- xtract[match.col]
  
  # get the data to plot scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN="mean")
  asset_risk <- scatterFUN(R=R, FUN=match.col)
  rnames <- colnames(R)
  
  if(match.col %in% c("ETL", "ES", "CVaR")){
    frontier <- meanetl.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
  }
  if(match.col %in% objnames){
    frontier <- meanvar.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
  }
  # data points to plot the frontier
  x.f <- frontier[, match.col]
  y.f <- frontier[, "mean"]
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(c(x.f, asset_risk))
  }
  if(is.null(ylim)){
    ylim <- range(c(y.f, asset_ret))
  }
  
  # plot a scatter of the assets
  plot(x=asset_risk, y=asset_ret, xlab=match.col, ylab="mean", main=main, xlim=xlim, ylim=ylim, pch=5, axes=FALSE, ...)
  text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=0.8)
  # plot the efficient line
  lines(x=x.f, y=y.f, col="darkgray", lwd=2)
  # plot the optimal portfolio
  points(opt_risk, opt_ret, col="blue", pch=16) # optimal
  text(x=opt_risk, y=opt_ret, labels="Optimal",col="blue", pos=4, cex=0.8)
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.EfficientFrontier
#' @export
chart.EfficientFrontier.optimize.portfolio <- function(object, match.col="ES", n.portfolios=25, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", ...){
  # This function will work with objects of class optimize.portfolio.DEoptim,
  # optimize.portfolio.random, and optimize.portfolio.pso
  
  if(inherits(object, "optimize.portfolio.GenSA")){
    stop("GenSA does not return any useable trace information for portfolios tested, thus we cannot extract an efficient frontier.")
  }
  
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class optimize.portfolio")
  
  portf <- object$portfolio
  R <- object$R
  if(is.null(R)) stop(paste("Not able to get asset returns from", object))
  wts <- object$weights
  
  # get the stats from the object
  xtract <- extractStats(object=object)
  columnames <- colnames(xtract)
  
  # Check if match.col is in extractStats output
  if(!(match.col %in% columnames)){
    stop(paste(match.col, "is not a column in extractStats output"))
  }
  
  # check if 'mean' is in extractStats output
  if(!("mean" %in% columnames)){
    stop("mean is not a column in extractStats output")
  }
  
  # get the stats of the optimal portfolio
  optstats <- xtract[which.min(xtract[, "out"]), ]
  opt_ret <- optstats["mean"]
  opt_risk <- optstats[match.col]
  
  # get the data to plot scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN="mean")
  asset_risk <- scatterFUN(R=R, FUN=match.col)
  rnames <- colnames(R)
  
  # get the data of the efficient frontier
  frontier <- extract.efficient.frontier(object=object, match.col=match.col, n.portfolios=n.portfolios)
  
  # data points to plot the frontier
  x.f <- frontier[, match.col]
  y.f <- frontier[, "mean"]
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(c(x.f, asset_risk))
  }
  if(is.null(ylim)){
    ylim <- range(c(y.f, asset_ret))
  }
  
  # plot a scatter of the assets
  plot(x=asset_risk, y=asset_ret, xlab=match.col, ylab="mean", main=main, xlim=xlim, ylim=ylim, pch=5, axes=FALSE, ...)
  text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=0.8)
  # plot the efficient line
  lines(x=x.f, y=y.f, col="darkgray", lwd=2)
  # plot the optimal portfolio
  points(opt_risk, opt_ret, col="blue", pch=16) # optimal
  text(x=opt_risk, y=opt_ret, labels="Optimal",col="blue", pos=4, cex=0.8)
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' chart the weights along the efficient frontier
#' 
#' This creates a stacked column chart of the weights of portfolios along the efficient frontier.
#' 
#' @param object object of class 'efficient.frontier' created by \code{\link{create.EfficientFrontier}}.
#' @param colorset color palette to use.
#' @param ... passthrough parameters to \code{barplot}.
#' @param match.col match.col string name of column to use for risk (horizontal axis).
#' Must match the name of an objective.
#' @param main main title used in the plot.
#' @param cex.lab The magnification to be used for x- and y-axis labels relative to the current setting of 'cex'.
#' @param cex.axis The magnification to be used for sizing the axis text relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param cex.legend The magnification to be used for sizing the legend relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param legend.labels character vector to use for the legend labels
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @author Ross Bennett
#' @export
chart.Weights.EF <- function(object, colorset=NULL, ..., match.col="ES", main="EF Weights", cex.lab=0.8, cex.axis=0.8, cex.legend=0.8, legend.labels=NULL, element.color="darkgray"){
  # using ideas from weightsPlot.R in fPortfolio package
  
  if(!inherits(object, "efficient.frontier")) stop("object must be of class 'efficient.frontier'")
  
  if(is.list(object)){
    # Objects created with create.EfficientFrontier will be a list of 2 elements
    frontier <- object$frontier
  } else {
    # Objects created with extractEfficientFrontier will only be an efficient.frontier object
    frontier <- object
  }
  
  
  # get the columns with weights
  cnames <- colnames(frontier)
  wts_idx <- grep(pattern="^w\\.", cnames)
  wts <- frontier[, wts_idx]
  
  # compute the weights for the barplot
  pos.weights <- +0.5 * (abs(wts) + wts)
  neg.weights <- -0.5 * (abs(wts) - wts)
  
  # Define Plot Range:
  ymax <- max(rowSums(pos.weights))
  ymin <- min(rowSums(neg.weights))
  range <- ymax - ymin
  ymax <- ymax + 0.005 * range
  ymin <- ymin - 0.005 * range
  dim <- dim(wts)
  range <- dim[1]
  xmin <- 0
  xmax <- range + 0.2 * range
  
  # set the colorset if no colorset is passed in
  if(is.null(colorset))
    colorset <- 1:dim[2]
  
  # plot the positive weights
  barplot(t(pos.weights), col = colorset, space = 0, ylab = "",
          xlim = c(xmin, xmax), ylim = c(ymin, ymax),
          border = element.color, cex.axis=cex.axis, 
          axisnames=FALSE,...)
  
  # set the legend information
  if(is.null(legend.labels)){
    legend.labels <- gsub(pattern="^w\\.", replacement="", cnames[wts_idx])
  }
  legend("topright", legend = legend.labels, bty = "n", cex = cex.legend, fill = colorset)
  
  # plot the negative weights
  barplot(t(neg.weights), col = colorset, space = 0, add = TRUE, border = element.color, 
          cex.axis=cex.axis, axes=FALSE, axisnames=FALSE, ...)
  
  # return along the efficient frontier
  # get the "mean" column
  mean.mtc <- pmatch("mean", cnames)
  if(is.na(mean.mtc)) {
    mean.mtc <- pmatch("mean.mean", cnames)
  }
  if(is.na(mean.mtc)) stop("could not match 'mean' with column name of extractStats output")
  
  # risk along the efficient frontier
  # get the match.col column
  mtc <- pmatch(match.col, cnames)
  if(is.na(mtc)) {
    mtc <- pmatch(paste(match.col,match.col,sep='.'),cnames)
  }
  if(is.na(mtc)) stop("could not match match.col with column name of extractStats output")
  
  # Add labels
  ef.return <- frontier[, mean.mtc]
  ef.risk <- frontier[, mtc]
  n.risk <- length(ef.risk)
  n.labels <- 6
  M <- c(0, ( 1:(n.risk %/% n.labels) ) ) * n.labels + 1
  # use 3 significant digits
  axis(3, at = M, labels = signif(ef.risk[M], 3), cex.axis=cex.axis)
  axis(1, at = M, labels = signif(ef.return[M], 3), cex.axis=cex.axis)
  
  # axis labels and titles
  mtext("Risk", side = 3, line = 2, adj = 1, cex = cex.lab)
  mtext("Return", side = 1, line = 2, adj = 1, cex = cex.lab)
  mtext("Weight", side = 2, line = 2, adj = 1, cex = cex.lab)
  # add title
  mtext(main, adj = 0, line = 2.5, font = 2, cex = 0.8)
  box(col=element.color)
}

#' @rdname chart.EfficientFrontier
#' @export
chart.EfficientFrontier.efficient.frontier <- function(object, chart.assets=TRUE, match.col="ES", n.portfolios=NULL, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", ...){
  if(!inherits(object, "efficient.frontier")) stop("object must be of class 'efficient.frontier'")
  
  # get the returns and efficient frontier object
  R <- object$R
  frontier <- object$frontier
  
  # get the column names from the frontier object
  cnames <- colnames(frontier)
  
  # get the "mean" column
  mean.mtc <- pmatch("mean", cnames)
  if(is.na(mean.mtc)) {
    mean.mtc <- pmatch("mean.mean", cnames)
  }
  if(is.na(mean.mtc)) stop("could not match 'mean' with column name of extractStats output")
  
  # get the match.col column
  mtc <- pmatch(match.col, cnames)
  if(is.na(mtc)) {
    mtc <- pmatch(paste(match.col,match.col,sep='.'),cnames)
  }
  if(is.na(mtc)) stop("could not match match.col with column name of extractStats output")
  
  if(chart.assets){
    # get the data to plot scatter of asset returns
    asset_ret <- scatterFUN(R=R, FUN="mean")
    asset_risk <- scatterFUN(R=R, FUN=match.col)
    rnames <- colnames(R)
    
    # set the x and y limits
    if(is.null(xlim)){
      xlim <- range(c(frontier[, mtc], asset_risk))
    }
    if(is.null(ylim)){
      ylim <- range(c(frontier[, mean.mtc], asset_ret))
    }
  }
  
  # plot the efficient frontier line
  plot(x=frontier[, mtc], y=frontier[, mean.mtc], ylab="mean", xlab=match.col, main=main, xlim=xlim, ylim=ylim, pch=5, axes=FALSE, ...)
  if(chart.assets){
    # risk-return scatter of the assets
    points(x=asset_risk, y=asset_ret)
    text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=0.8)
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

