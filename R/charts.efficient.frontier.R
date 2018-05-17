
#' Chart the efficient frontier and risk-return scatter
#' 
#' Chart the efficient frontier and risk-return scatter of the assets for 
#' \code{optimize.portfolio} or \code{efficient.frontier} objects
#' 
#' @details
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
#'   \item The mean-StdDev or mean-ETL efficient frontier can be plotted for optimal
#'   portfolio objects created by \code{optimize.portfolio}.
#' 
#'   \item If \code{match.col="StdDev"}, the mean-StdDev efficient frontier is plotted.
#' 
#'   \item If \code{match.col="ETL"} (also "ES" or "CVaR"), the mean-ETL efficient frontier is plotted.
#' }
#' 
#' Note that \code{trace=TRUE} must be specified in \code{\link{optimize.portfolio}}
#' 
#' GenSA does not return any useable trace information for portfolios tested at 
#' each iteration, therfore we cannot extract and chart an efficient frontier.
#' 
#' By default, the tangency portfolio (maximum Sharpe Ratio or modified Sharpe Ratio)
#' will be plotted using a risk free rate of 0. Set \code{rf=NULL} to omit 
#' this from the plot. 
#' 
#' @param object object to chart.
#' @param \dots passthru parameters to \code{\link{plot}}
#' @param match.col string name of column to use for risk (horizontal axis).
#' \code{match.col} must match the name of an objective measure in the 
#' \code{objective_measures} or \code{opt_values} slot in the object created 
#' by \code{\link{optimize.portfolio}}.
#' @param n.portfolios number of portfolios to use to plot the efficient frontier.
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}.
#' @param cex.axis numerical value giving the amount by which the axis should be magnified relative to the default.
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param main a main title for the plot.
#' @param RAR.text string name for risk adjusted return text to plot in the legend.
#' @param rf risk free rate. If \code{rf} is not null, the maximum Sharpe Ratio or modified Sharpe Ratio tangency portfolio will be plotted.
#' @param tangent.line TRUE/FALSE to plot the tangent line.
#' @param cex.legend numerical value giving the amount by which the legend should be magnified relative to the default.
#' @param chart.assets TRUE/FALSE to include the assets.
#' @param labels.assets TRUE/FALSE to include the asset names in the plot. 
#' \code{chart.assets} must be \code{TRUE} to plot asset names.
#' @param pch.assets plotting character of the assets, same as in \code{\link{plot}}.
#' @param cex.assets numerical value giving the amount by which the asset points and labels should be magnified relative to the default.
#' @author Ross Bennett
#' @rdname chart.EfficientFrontier
#' @export
chart.EfficientFrontier <- function(object, ...){
  UseMethod("chart.EfficientFrontier")
}

#' @rdname chart.EfficientFrontier
#' @method chart.EfficientFrontier optimize.portfolio.ROI
#' @S3method chart.EfficientFrontier optimize.portfolio.ROI
chart.EfficientFrontier.optimize.portfolio.ROI <- function(object, ..., match.col="ES", n.portfolios=25, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", RAR.text="SR", rf=0, tangent.line=TRUE, cex.legend=0.8, chart.assets=TRUE, labels.assets=TRUE, pch.assets=21, cex.assets=0.8){
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class optimize.portfolio.ROI")
  
  portf <- object$portfolio
  R <- object$R
  if(is.null(R)) stop(paste("Not able to get asset returns from", object))
  wts <- object$weights
  objectclass <- class(object)[1]
  
  # objnames <- unlist(lapply(portf$objectives, function(x) x$name))
  # if(!(match.col %in% objnames)){
  #   stop("match.col must match an objective name")
  # }
  
  # get the optimal return and risk metrics
  xtract <- extractStats(object=object)
  columnames <- names(xtract)
  if(!(("mean") %in% columnames)){
    # we need to calculate the mean given the optimal weights
    opt_ret <- applyFUN(R=R, weights=wts, FUN="mean")
  } else {
    opt_ret <- xtract["mean"]
  }
  # get the match.col column
  mtc <- pmatch(match.col, columnames)
  if(is.na(mtc)) {
    mtc <- pmatch(paste(match.col,match.col,sep='.'), columnames)
  }
  if(is.na(mtc)){
    # if(is.na(mtc)) stop("could not match match.col with column name of extractStats output")
    opt_risk <- applyFUN(R=R, weights=wts, FUN=match.col)
  } else {
    opt_risk <- xtract[mtc]
  }
  
  # get the data to plot scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN="mean")
  asset_risk <- scatterFUN(R=R, FUN=match.col)
  rnames <- colnames(R)
  
  if(match.col %in% c("ETL", "ES", "CVaR")){
    frontier <- meanetl.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
    rar <- "STARR"
  }
  if(match.col == "StdDev"){
    frontier <- meanvar.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
    rar <- "SR"
  }
  # data points to plot the frontier
  x.f <- frontier[, match.col]
  y.f <- frontier[, "mean"]
  
  # Points for the Sharpe Ratio ((mu - rf) / StdDev) or STARR ((mu - rf) / ETL)
  if(!is.null(rf)){
    sr <- (y.f - rf) / (x.f)
    idx.maxsr <- which.max(sr)
    srmax <- sr[idx.maxsr]
  }
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(c(x.f, asset_risk))
    # xlim[1] <- xlim[1] * 0.8
    xlim[1] <- 0
    xlim[2] <- xlim[2] * 1.15
  }
  if(is.null(ylim)){
    ylim <- range(c(y.f, asset_ret))
    # ylim[1] <- ylim[1] * 0.9
    ylim[1] <- 0
    ylim[2] <- ylim[2] * 1.1
  }
  
  # plot the efficient frontier line
  plot(x=x.f, y=y.f, ylab="Mean", xlab=match.col, main=main, xlim=xlim, ylim=ylim, axes=FALSE, ...)
  
  # Add the global minimum variance or global minimum ETL portfolio
  points(x=x.f[1], y=y.f[1], pch=16)
  
  if(chart.assets){
    # risk-return scatter of the assets
    points(x=asset_risk, y=asset_ret, pch=pch.assets, cex=cex.assets)
    if(labels.assets) text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=cex.assets)
  }
  
  # plot the optimal portfolio
  points(opt_risk, opt_ret, col="blue", pch=16) # optimal
  text(x=opt_risk, y=opt_ret, labels="Optimal",col="blue", pos=4, cex=0.8)
  if(!is.null(rf)){
    # Plot tangency line and points at risk-free rate and tangency portfolio
    if(tangent.line) abline(rf, srmax, lty=2)
    points(0, rf, pch=16)
    points(x.f[idx.maxsr], y.f[idx.maxsr], pch=16)
    # text(x=x.f[idx.maxsr], y=y.f[idx.maxsr], labels="T", pos=4, cex=0.8)
    # Add lengend with max Sharpe Ratio and risk-free rate
    legend("topleft", paste(RAR.text, " = ", signif(srmax,3), sep = ""), bty = "n", cex=cex.legend)
    legend("topleft", inset = c(0,0.05), paste("rf = ", signif(rf,3), sep = ""), bty = "n", cex=cex.legend)
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.EfficientFrontier
#' @method chart.EfficientFrontier optimize.portfolio
#' @S3method chart.EfficientFrontier optimize.portfolio
chart.EfficientFrontier.optimize.portfolio <- function(object, ..., match.col="ES", n.portfolios=25, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", RAR.text="SR", rf=0, tangent.line=TRUE, cex.legend=0.8, chart.assets=TRUE, labels.assets=TRUE, pch.assets=21, cex.assets=0.8){
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
  
  # Points for the Sharpe or Modified Sharpe Ratio
  if(!is.null(rf)){
    sr <- (y.f - rf) / (x.f)
    idx.maxsr <- which.max(sr)
    srmax <- sr[idx.maxsr]
  }
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(c(x.f, asset_risk))
    # xlim[1] <- xlim[1] * 0.8
    xlim[1] <- 0
    xlim[2] <- xlim[2] * 1.15
  }
  if(is.null(ylim)){
    ylim <- range(c(y.f, asset_ret))
    # ylim[1] <- ylim[1] * 0.9
    ylim[1] <- 0
    ylim[2] <- ylim[2] * 1.1
  }
  
  # plot the efficient frontier line
  plot(x=x.f, y=y.f, ylab="Mean", xlab=match.col, main=main, xlim=xlim, ylim=ylim, axes=FALSE, ...)
  
  # Add the global minimum variance or global minimum ETL portfolio
  points(x=x.f[1], y=y.f[1], pch=16)
  
  if(chart.assets){
    # risk-return scatter of the assets
    points(x=asset_risk, y=asset_ret, pch=pch.assets, cex=cex.assets)
    if(labels.assets) text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=cex.assets)
  }
  
  # plot the optimal portfolio
  points(opt_risk, opt_ret, col="blue", pch=16) # optimal
  text(x=opt_risk, y=opt_ret, labels="Optimal",col="blue", pos=4, cex=0.8)
  if(!is.null(rf)){
    # Plot tangency line and points at risk-free rate and tangency portfolio
    if(tangent.line) abline(rf, srmax, lty=2)
    points(0, rf, pch=16)
    points(x.f[idx.maxsr], y.f[idx.maxsr], pch=16)
    # text(x=x.f[idx.maxsr], y=y.f[idx.maxsr], labels="T", pos=4, cex=0.8)
    # Add lengend with max Sharpe Ratio and risk-free rate
    legend("topleft", paste(RAR.text, " = ", signif(srmax,3), sep = ""), bty = "n", cex=cex.legend)
    legend("topleft", inset = c(0,0.05), paste("rf = ", signif(rf,3), sep = ""), bty = "n", cex=cex.legend)
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}


#' Chart weights along an efficient frontier
#' 
#' This function produces a stacked barplot of weights along an efficient frontier.
#' 
#' @param object object of class \code{efficient.frontier} or \code{optimize.portfolio}.
#' @param \dots passthru parameters to \code{barplot}.
#' @param colorset color palette or vector of colors to use.
#' @param n.portfolios number of portfolios to extract along the efficient frontier.
#' @param by.groups TRUE/FALSE. If TRUE, the group weights are charted.
#' @param match.col string name of column to use for risk (horizontal axis). Must match the name of an objective.
#' @param main title used in the plot.
#' @param cex.lab the magnification to be used for x-axis and y-axis labels relative to the current setting of 'cex'.
#' @param cex.axis the magnification to be used for sizing the axis text relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param cex.legend the magnification to be used for sizing the legend relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param legend.labels character vector to use for the legend labels.
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param legend.loc NULL, "topright", "right", or "bottomright". If legend.loc is NULL, the legend will not be plotted.
#' @author Ross Bennett
#' @rdname chart.EF.Weights
#' @export
chart.EF.Weights <- function(object, ...){
  UseMethod("chart.EF.Weights")
}


#' @rdname chart.EF.Weights
#' @method chart.EF.Weights efficient.frontier
#' @S3method chart.EF.Weights efficient.frontier
chart.EF.Weights.efficient.frontier <- function(object, ..., colorset=NULL, n.portfolios=25, by.groups=FALSE, match.col="ES", main="", cex.lab=0.8, cex.axis=0.8, cex.legend=0.8, legend.labels=NULL, element.color="darkgray", legend.loc="topright"){
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
  
  if(by.groups){
    constraints <- get_constraints(object$portfolio)
    groups <- constraints$groups
    if(is.null(groups)) stop("group constraints not in portfolio object")
    if(!is.null(groups)){
      groupfun <- function(weights, groups){
        # This function is to calculate weights by group given the group list
        # and a matrix of weights along the efficient frontier
        ngroups <- length(groups)
        group_weights <- rep(0, ngroups)
        for(i in 1:ngroups){
          group_weights[i] <- sum(weights[groups[[i]]])
        }
        group_weights
      }
      wts <- t(apply(wts, 1, groupfun, groups=groups))
    }
  }
  
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
  if(is.null(legend.loc)){
    xmax <- range
  } else {
    xmax <- range + 0.3 * range
  }
  
  # set the colorset if no colorset is passed in
  if(is.null(colorset))
    colorset <- 1:dim[2]
  
  # plot the positive weights
  barplot(t(pos.weights), col = colorset, space = 0, ylab = "",
          xlim = c(xmin, xmax), ylim = c(ymin, ymax),
          border = element.color, cex.axis=cex.axis, 
          axisnames=FALSE, ...)
  
  if(!is.null(legend.loc)){
    if(legend.loc %in% c("topright", "right", "bottomright")){
      # set the legend information
      if(is.null(legend.labels)){
        if(by.groups){
          legend.labels <- names(groups)
          if(is.null(legend.labels)) legend.labels <- constraints$group_labels
        } else {
          legend.labels <- gsub(pattern="^w\\.", replacement="", cnames[wts_idx])
        }
      }
      legend(legend.loc, legend = legend.labels, bty = "n", cex = cex.legend, fill = colorset)
    }
  }
  # plot the negative weights
  barplot(t(neg.weights), col = colorset, space = 0, add = TRUE, border = element.color, 
          cex.axis=cex.axis, axes=FALSE, axisnames=FALSE, ...)
  
  
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
  mtext(match.col, side = 3, line = 2, adj = 0.5, cex = cex.lab)
  mtext("Mean", side = 1, line = 2, adj = 0.5, cex = cex.lab)
  mtext("Weight", side = 2, line = 2, adj = 0.5, cex = cex.lab)
  # add title
  title(main=main, line=3)
  # mtext(main, adj = 0, line = 2.5, font = 2, cex = 0.8)
  box(col=element.color)
}

#' @rdname chart.EF.Weights
#' @method chart.EF.Weights optimize.portfolio
#' @S3method chart.EF.Weights optimize.portfolio
chart.EF.Weights.optimize.portfolio <- function(object, ..., colorset=NULL, n.portfolios=25, by.groups=FALSE, match.col="ES", main="", cex.lab=0.8, cex.axis=0.8, cex.legend=0.8, legend.labels=NULL, element.color="darkgray", legend.loc="topright"){
  # chart the weights along the efficient frontier of an objected created by optimize.portfolio
  
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class optimize.portfolio")
  
  frontier <- extractEfficientFrontier(object=object, match.col=match.col, n.portfolios=n.portfolios)
  chart.EF.Weights(object=frontier, colorset=colorset, ..., 
                   match.col=match.col, by.groups=by.groups, main=main, cex.lab=cex.lab, 
                   cex.axis=cex.axis, cex.legend=cex.legend, 
                   legend.labels=legend.labels, element.color=element.color,
                   legend.loc=legend.loc)
}

#' @rdname chart.EfficientFrontier
#' @method chart.EfficientFrontier efficient.frontier
#' @S3method chart.EfficientFrontier efficient.frontier
chart.EfficientFrontier.efficient.frontier <- function(object, ..., match.col="ES", n.portfolios=NULL, xlim=NULL, ylim=NULL, cex.axis=0.8, element.color="darkgray", main="Efficient Frontier", RAR.text="SR", rf=0, tangent.line=TRUE, cex.legend=0.8, chart.assets=TRUE, labels.assets=TRUE, pch.assets=21, cex.assets=0.8){
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
  if(is.na(mean.mtc)) stop("could not match 'mean' with column name of efficient frontier")
  
  # get the match.col column
  mtc <- pmatch(match.col, cnames)
  if(is.na(mtc)) {
    mtc <- pmatch(paste(match.col,match.col,sep='.'),cnames)
  }
  if(is.na(mtc)) stop("could not match match.col with column name of efficient frontier")
  
  if(chart.assets){
    # get the data to plot scatter of asset returns
    asset_ret <- scatterFUN(R=R, FUN="mean")
    asset_risk <- scatterFUN(R=R, FUN=match.col)
    rnames <- colnames(R)
  } else {
    asset_ret <- NULL
    asset_risk <- NULL
  }
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(c(frontier[, mtc], asset_risk))
    # xlim[1] <- xlim[1] * 0.8
    xlim[1] <- 0
    xlim[2] <- xlim[2] * 1.15
  }
  if(is.null(ylim)){
    ylim <- range(c(frontier[, mean.mtc], asset_ret))
    # ylim[1] <- ylim[1] * 0.9
    ylim[1] <- 0
    ylim[2] <- ylim[2] * 1.1
  }
  
  if(!is.null(rf)){
    sr <- (frontier[, mean.mtc] - rf) / (frontier[, mtc])
    idx.maxsr <- which.max(sr)
    srmax <- sr[idx.maxsr]
  }
  
  # plot the efficient frontier line
  plot(x=frontier[, mtc], y=frontier[, mean.mtc], ylab="Mean", xlab=match.col, main=main, xlim=xlim, ylim=ylim, axes=FALSE, ...)
  
  # Add the global minimum variance or global minimum ETL portfolio
  points(x=frontier[1, mtc], y=frontier[1, mean.mtc], pch=16)
  
  if(chart.assets){
    # risk-return scatter of the assets
    points(x=asset_risk, y=asset_ret, pch=pch.assets, cex=cex.assets)
    if(labels.assets) text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=cex.assets)
  }
  
  if(!is.null(rf)){
    # Plot tangency line and points at risk-free rate and tangency portfolio
    if(tangent.line) abline(rf, srmax, lty=2)
    points(0, rf, pch=16)
    points(frontier[idx.maxsr, mtc], frontier[idx.maxsr, mean.mtc], pch=16)
    # text(x=frontier[idx.maxsr], y=frontier[idx.maxsr], labels="T", pos=4, cex=0.8)
    # Add legend with max Risk adjusted Return ratio and risk-free rate
    legend("topleft", paste(RAR.text, " = ", signif(srmax,3), sep = ""), bty = "n", cex=cex.legend)
    legend("topleft", inset = c(0,0.05), paste("rf = ", signif(rf,3), sep = ""), bty = "n", cex=cex.legend)
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' Plot multiple efficient frontiers
#' 
#' Overlay the efficient frontiers of multiple portfolio objects on a single plot.
#' 
#' @param R an xts object of asset returns
#' @param portfolio_list list of portfolio objects created by 
#' \code{\link{portfolio.spec}} and combined with \code{\link{combine.portfolios}}
#' @param type type of efficient frontier, see \code{\link{create.EfficientFrontier}}
#' @param n.portfolios number of portfolios to extract along the efficient frontier.
#' This is only used for objects of class \code{optimize.portfolio}
#' @param match.col string name of column to use for risk (horizontal axis).
#' Must match the name of an objective.
#' @param search_size passed to optimize.portfolio for type="DEoptim" or type="random".
#' @param main title used in the plot.
#' @param cex.axis the magnification to be used for sizing the axis text relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param legend.loc location of the legend; NULL, "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param legend.labels character vector to use for the legend labels.
#' @param cex.legend The magnification to be used for sizing the legend relative to the current setting of 'cex', similar to \code{\link{plot}}.
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}.
#' @param \dots passthrough parameters to \code{\link{plot}}.
#' @param chart.assets TRUE/FALSE to include the assets.
#' @param labels.assets TRUE/FALSE to include the asset names in the plot.
#' @param pch.assets plotting character of the assets, same as in \code{\link{plot}}.
#' @param cex.assets A numerical value giving the amount by which the asset points and labels should be magnified relative to the default.
#' @param col vector of colors with length equal to the number of portfolios in \code{portfolio_list}.
#' @param lty vector of line types with length equal to the number of portfolios in \code{portfolio_list}.
#' @param lwd vector of line widths with length equal to the number of portfolios in \code{portfolio_list}.
#' @author Ross Bennett
#' @export
chart.EfficientFrontierOverlay <- function(R, portfolio_list, type, n.portfolios=25, match.col="ES", search_size=2000, main="Efficient Frontiers", cex.axis=0.8, element.color="darkgray", legend.loc=NULL, legend.labels=NULL, cex.legend=0.8, xlim=NULL, ylim=NULL, ..., chart.assets=TRUE, labels.assets=TRUE, pch.assets=21, cex.assets=0.8, col=NULL, lty=NULL, lwd=NULL){
  # create multiple efficient frontier objects (one per portfolio in portfolio_list)
  if(!inherits(portfolio_list, "portfolio.list")) stop("portfolio_list must be passed in as a list")
  if(length(portfolio_list) == 1) warning("Only one portfolio object in portfolio_list")
  # store in out
  out <- list()
  for(i in 1:length(portfolio_list)){
    if(!is.portfolio(portfolio_list[[i]])) stop("portfolio in portfolio_list must be of class 'portfolio'")
    out[[i]] <- create.EfficientFrontier(R=R, portfolio=portfolio_list[[i]], type=type, n.portfolios=n.portfolios, match.col=match.col, search_size=search_size)
  }
  # get the data to plot scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN="mean")
  asset_risk <- scatterFUN(R=R, FUN=match.col)
  rnames <- colnames(R)
  
  # set the x and y limits
  if(is.null(xlim)){
    xlim <- range(asset_risk)
    # xlim[1] <- xlim[1] * 0.8
    xlim[1] <- 0
    xlim[2] <- xlim[2] * 1.15
  }
  if(is.null(ylim)){
    ylim <- range(asset_ret)
    # ylim[1] <- ylim[1] * 0.9
    ylim[1] <- 0
    ylim[2] <- ylim[2] * 1.1
  }
  
  # plot the assets
  plot(x=asset_risk, y=asset_ret, xlab=match.col, ylab="Mean", main=main, xlim=xlim, ylim=ylim, axes=FALSE, type="n", ...)
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
  
  if(chart.assets){
    # risk-return scatter of the assets
    points(x=asset_risk, y=asset_ret, pch=pch.assets, cex=cex.assets)
    if(labels.assets) text(x=asset_risk, y=asset_ret, labels=rnames, pos=4, cex=cex.assets)
  }
  
  # set some basic plot parameters
  if(is.null(col)) col <- 1:length(out)
  if(is.null(lty)) lty <- 1:length(out)
  if(is.null(lwd)) lwd <- rep(1, length(out))
  
  for(i in 1:length(out)){
    tmp <- out[[i]]
    tmpfrontier <- tmp$frontier
    cnames <- colnames(tmpfrontier)
    
    # get the "mean" column
    mean.mtc <- pmatch("mean", cnames)
    if(is.na(mean.mtc)) {
      mean.mtc <- pmatch("mean.mean", cnames)
    }
    if(is.na(mean.mtc)) stop("could not match 'mean' with column name of extractStats output")
    
    # get the match.col column
    mtc <- pmatch(match.col, cnames)
    if(is.na(mtc)) {
      mtc <- pmatch(paste(match.col, match.col, sep='.'),cnames)
    }
    if(is.na(mtc)) stop("could not match match.col with column name of extractStats output")
    # Add the efficient frontier lines to the plot
    lines(x=tmpfrontier[, mtc], y=tmpfrontier[, mean.mtc], col=col[i], lty=lty[i], lwd=lwd[i])
  }
  if(!is.null(legend.loc)){
    if(is.null(legend.labels)){
      legend.labels <- paste("Portfolio", 1:length(out), sep=".")
    }
    legend(legend.loc, legend=legend.labels, col=col, lty=lty, lwd=lwd, cex=cex.legend, bty="n") 
  }
  return(invisible(out))
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
