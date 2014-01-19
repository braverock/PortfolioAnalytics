#' Apply a risk or return function to a set of weights
#' 
#' This function is used to calculate risk or return metrics given a matrix of
#' weights and is primarily used as a convenience function used in chart.Scatter functions
#' 
#' @param R xts object of asset returns
#' @param weights a matrix of weights generated from random_portfolios or \code{optimize.portfolio}
#' @param FUN name of a function
#' @param arguments named list of arguments to FUN
#' @author Ross Bennett
#' @export
applyFUN <- function(R, weights, FUN="mean", arguments){
  nargs <- arguments
  
  moments <- function(R){
    momentargs <- list()
    momentargs$mu <- matrix(as.vector(apply(R, 2, "mean")), ncol = 1)
    momentargs$sigma <- cov(R)
    momentargs$m3 <- PerformanceAnalytics:::M3.MM(R)
    momentargs$m4 <- PerformanceAnalytics:::M4.MM(R)
    return(momentargs)
  }
  
  nargs <- c(nargs, moments(R))
  nargs$R <- R
  #nargs$invert=FALSE
  
  # match the FUN arg to a risk or return function
  switch(FUN,
         mean = {
           fun = match.fun(mean)
         },
         sd =,
         StdDev = { 
           fun = match.fun(StdDev)
         },
         mVaR =,
         VaR = {
           fun = match.fun(VaR) 
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
         es =,
         mES =,
         CVaR =,
         cVaR =,
         ETL=,
         mETL=,
         ES = {
           fun = match.fun(ES)
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
{   # see 'S Programming p. 67 for this matching
  fun <- try(match.fun(FUN))
}
  ) # end switch block
  
  if(!is.null(nrow(weights))){
    # case for matrix of weights
    out <- rep(0, nrow(weights))
    .formals  <- formals(fun)
    onames <- names(.formals)
    for(i in 1:nrow(weights)){
      nargs$weights <- as.numeric(weights[i,])
      nargs$x <- R %*% as.numeric(weights[i,])
      dargs <- nargs
      pm <- pmatch(names(dargs), onames, nomatch = 0L)
      names(dargs[pm > 0L]) <- onames[pm]
      .formals[pm] <- dargs[pm > 0L]
      out[i] <- try(do.call(fun, .formals))
    }
  } else {
    # case for single vector of weights
    .formals  <- formals(fun)
    onames <- names(.formals)
    nargs$weights <- as.numeric(weights)
    nargs$x <- R %*% as.numeric(weights)
    dargs <- nargs
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
    out <- try(do.call(fun, .formals))
  }
     return(out)
}

#' Apply a risk or return function to asset returns
#' 
#' This function is used to calculate risk or return metrics given a matrix of
#' asset returns and will be used for a risk-reward scatter plot of the assets
#' 
#' @param R xts object of asset returns
#' @param FUN name of function
#' @param arguments named list of arguments to FUN
#' @author Ross Bennett
#' @export
scatterFUN <- function(R, FUN, arguments=NULL){
  if(is.null(arguments)){
    nargs <- list()
  } else{
    nargs <- arguments
  }
  
  # match the FUN arg to a risk or return function
  switch(FUN,
         mean = {
           return(as.numeric(apply(R, 2, mean)))
           #fun = match.fun(mean)
           #nargs$x = R
         },
         var = {
           return(as.numeric(apply(R, 2, var)))
           #fun = match.fun(mean)
           #nargs$x = R
         },
         sd =,
         StdDev = { 
           fun = match.fun(StdDev)
         },
         mVaR =,
         VaR = {
           fun = match.fun(VaR) 
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
         es =,
         mES =,
         CVaR =,
         cVaR =,
         ETL =,
         mETL =,
         ES = {
           fun = match.fun(ES)
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
{   # see 'S Programming p. 67 for this matching
  fun <- try(match.fun(FUN))
}
  ) # end switch block
  
  # calculate FUN on R
  out <- rep(0, ncol(R))
  .formals  <- formals(fun)
  onames <- names(.formals)
  for(i in 1:ncol(R)){
    nargs$R <- R[, i]
    dargs <- nargs
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
    out[i] <- try(do.call(fun, .formals))
  }
  return(out)
}

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
#' @rdname chart.Weights.EF
#' @export
chart.Weights.EF <- function(object, ...){
  UseMethod("chart.Weights.EF")
}


#' @rdname chart.Weights.EF
#' @method chart.Weights.EF efficient.frontier
#' @S3method chart.Weights.EF efficient.frontier
chart.Weights.EF.efficient.frontier <- function(object, ..., colorset=NULL, n.portfolios=25, by.groups=FALSE, match.col="ES", main="", cex.lab=0.8, cex.axis=0.8, cex.legend=0.8, legend.labels=NULL, element.color="darkgray", legend.loc="topright"){
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

#' @rdname chart.Weights.EF
#' @method chart.Weights.EF optimize.portfolio
#' @S3method chart.Weights.EF optimize.portfolio
chart.Weights.EF.optimize.portfolio <- function(object, ..., colorset=NULL, n.portfolios=25, by.groups=FALSE, match.col="ES", main="", cex.lab=0.8, cex.axis=0.8, cex.legend=0.8, legend.labels=NULL, element.color="darkgray", legend.loc="topright"){
  # chart the weights along the efficient frontier of an objected created by optimize.portfolio
  
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class optimize.portfolio")
  
  frontier <- extractEfficientFrontier(object=object, match.col=match.col, n.portfolios=n.portfolios)
  PortfolioAnalytics:::chart.Weights.EF(object=frontier, colorset=colorset, ..., 
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
  
  # get the data to plot scatter of asset returns
  asset_ret <- scatterFUN(R=R, FUN="mean")
  asset_risk <- scatterFUN(R=R, FUN=match.col)
  rnames <- colnames(R)
  
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
#' @param portfolio_list list of portfolio objects created by \code{\link{portfolio.spec}}
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
  if(!is.list(portfolio_list)) stop("portfolio_list must be passed in as a list")
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
# compare optimal weights of multiple portfolios

#' @rdname chart.Weights
#' @method chart.Weights opt.list
#' @S3method chart.Weights opt.list
chart.Weights.opt.list <- function(object, neighbors=NULL, ..., main="Weights", las=3, xlab=NULL, cex.lab=1, element.color="darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8, plot.type="line"){
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  
  if(plot.type %in% c("bar", "barplot")){
    barplotOptWeights(object=object, main=main, las=las, xlab=xlab, cex.lab=cex.lab, element.color=element.color, cex.axis=cex.axis, colorset=colorset, legend.loc=legend.loc, cex.legend=cex.legend, ...)
  } else if(plot.type == "line"){
    
    # get the optimal weights in a matrix
    weights_mat <- extractWeights.opt.list(object)
    opt_names <- rownames(weights_mat)
    
    columnnames <- colnames(weights_mat)
    numassets <- length(columnnames)
    
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
    
    if(is.null(colorset)) colorset=1:nrow(weights_mat)
    if(length(colorset) != nrow(weights_mat)) colorset <- rep(colorset[1], nrow(weights_mat))
    plot(weights_mat[1,], type="n", axes=FALSE, xlab='', ylab="Weights", main=main, ...)
    for(i in 1:nrow(weights_mat)){
      points(weights_mat[i,], type="b", col=colorset[i], lty=1)
    }
    if(!is.null(legend.loc)) legend(legend.loc, legend=opt_names, col=colorset, bty="n", lty=1, cex=cex.legend)
    axis(2, cex.axis=cex.axis, col=element.color)
    axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis=cex.axis, col=element.color)
    box(col=element.color)
  }
}

barplotOptWeights <- function(object, ..., main="Weights", las=3, xlab=NULL, cex.lab=1, element.color="darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8){
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  
  # get the optimal weights in a matrix
  weights_mat <- extractWeights.opt.list(object)
  opt_names <- rownames(weights_mat)
  
  if(is.null(colorset)) colorset <- 1:nrow(weights_mat)
  
  barplot(weights_mat, beside=TRUE, main=main, cex.axis=cex.axis, cex.names=cex.lab, las=las, col=colorset, ...)
  if(!is.null(legend.loc)){
    legend(legend.loc, legend=opt_names, fill=colorset, bty="n", cex=cex.legend)
  }
  box(col=element.color)
}

#' @rdname chart.RiskReward
#' @method chart.RiskReward opt.list
#' @S3method chart.RiskReward opt.list
chart.RiskReward.opt.list <- function(object, ..., risk.col="ES", return.col="mean", main="", ylim=NULL, xlim=NULL, labels.assets=TRUE, chart.assets=FALSE, pch.assets=1, cex.assets=0.8, cex.axis=0.8, cex.lab=0.8, colorset=NULL, element.color="darkgray"){
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  # Get the objective measures
  obj <- extractObjectiveMeasures(object)
  
  # check if risk.col and return.col are valid objective measures
  columnnames <- colnames(obj)
  if(!(risk.col %in% columnnames)) stop(paste(risk.col, "not in column names"))
  if(!(return.col %in% columnnames)) stop(paste(return.col, "not in column names"))
  
  if(chart.assets){
    # Get the returns from the firts opt.list object
    R <- object[[1]]$R
    if(is.null(R)) stop("Returns object not detected, must run optimize.portfolio with trace=TRUE")
    if(!all(sapply(X=object, FUN=function(x) identical(x=R, y=x$R)))){
      message("Not all returns objects are identical, using returns object from first optimize.portfolio object")
    }
    # Get the arguments from the optimize.portfolio objects 
    # to calculate the risk and return metrics for the scatter plot. 
    # (e.g. arguments=list(p=0.925, clean="boudt")
    arguments <- NULL # maybe an option to let the user pass in an arguments list?
    if(is.null(arguments)){
      # get all the arguments from the portfolio in each optimize.portfolio object
      tmp <- lapply(X=object, function(x) {
        lapply(x$portfolio$objectives, function(u) u$arguments)
      })
      # Flatten the nested lists
      tmp.args <- do.call(c, unlist(tmp, recursive=FALSE))
      # Remove the name that gets added with unlist
      names(tmp.args) <- gsub("^.*\\.", replacement="", names(tmp.args))
      # Remove any duplicate arguments
      # if(any(duplicated(names(tmp.args)))) message("Multiple duplicate arguments, using first valid argument")
      tmp.args <- tmp.args[!duplicated(names(tmp.args))]
      if(!is.null(tmp.args$portfolio_method)) tmp.args$portfolio_method <- "single"
      arguments <- tmp.args
    }
    asset_ret <- scatterFUN(R=R, FUN=return.col, arguments)
    asset_risk <- scatterFUN(R=R, FUN=risk.col, arguments)
  } else {
    asset_ret <- NULL
    asset_risk <- NULL
  }
  
  # data to plot
  dat <- na.omit(obj[, c(risk.col, return.col)])
  if(ncol(dat) < 1) stop("No data to plot after na.omit")
  dat_names <- rownames(dat)
  
  # colors to plot
  if(is.null(colorset)){
    colorset <- 1:nrow(dat)
  }
  
  # set xlim and ylim
  if(is.null(xlim)){
    xlim <- range(c(dat[, risk.col], asset_risk))
    xlim[1] <- 0
    xlim[2] <- xlim[2] * 1.25
  }
  
  if(is.null(ylim)){
    ylim <- range(c(dat[, return.col], asset_ret))
    ylim[1] <- 0
    ylim[2] <- ylim[2] * 1.15
  }
  
  # plot the points
  plot(x=dat[, risk.col], y=dat[, return.col], cex.lab=cex.lab, main=main, ylab=return.col, xlab=risk.col, xlim=xlim, ylim=ylim, pch=pch.assets, col=colorset, ..., axes=FALSE)
  if(labels.assets) text(x=dat[, risk.col], y=dat[, return.col], labels=dat_names, pos=4, cex=cex.assets, col=colorset)
  
  # plot the risk-reward scatter of the assets
  if(chart.assets){
    points(x=asset_risk, y=asset_ret)
    text(x=asset_risk, y=asset_ret, labels=colnames(R), pos=4, cex=0.8)
  }
  
  # add the axis
  axis(2, cex.axis=cex.axis, col=element.color)
  axis(1, cex.axis=cex.axis, col=element.color)
  box(col=element.color)
}

chart.Weights.pso <- function(object, ..., neighbors = NULL, main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8, plot.type="line"){
  
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class 'optimize.portfolio.pso'")
  
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
#' @method chart.Weights optimize.portfolio.pso
#' @S3method chart.Weights optimize.portfolio.pso
chart.Weights.optimize.portfolio.pso <- chart.Weights.pso

chart.Scatter.pso <- function(object, ..., neighbors=NULL, return.col="mean", risk.col="ES", chart.assets=FALSE, element.color = "darkgray", cex.axis=0.8, xlim=NULL, ylim=NULL){
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class 'optimize.portfolio.pso'")
  
  R <- object$R
  if(is.null(R)) stop("Returns object not detected, must run optimize.portfolio with trace=TRUE")
  # portfolio <- object$portfolio
  xtract = extractStats(object)
  columnnames = colnames(xtract)
  #return.column = grep(paste("objective_measures",return.col,sep='.'),columnnames)
  return.column = pmatch(return.col,columnnames)
  if(is.na(return.column)) {
    return.col = paste(return.col,return.col,sep='.')
    return.column = pmatch(return.col,columnnames)
  }
  #risk.column = grep(paste("objective_measures",risk.col,sep='.'),columnnames)
  risk.column = pmatch(risk.col,columnnames)
  if(is.na(risk.column)) {
    risk.col = paste(risk.col,risk.col,sep='.')
    risk.column = pmatch(risk.col,columnnames)
  }
  
  # if(is.na(return.column) | is.na(risk.column)) stop(return.col,' or ',risk.col, ' do not match extractStats output')
  
  # If the user has passed in return.col or risk.col that does not match extractStats output
  # This will give the flexibility of passing in return or risk metrics that are not
  # objective measures in the optimization. This may cause issues with the "neighbors"
  # functionality since that is based on the "out" column
  if(is.na(return.column) | is.na(risk.column)){
    return.col <- gsub("\\..*", "", return.col)
    risk.col <- gsub("\\..*", "", risk.col)
    warning(return.col,' or ', risk.col, ' do  not match extractStats output of $objective_measures slot')
    # Get the matrix of weights for applyFUN
    wts_index <- grep("w.", columnnames)
    wts <- xtract[, wts_index]
    if(is.na(return.column)){
      tmpret <- applyFUN(R=R, weights=wts, FUN=return.col)
      xtract <- cbind(tmpret, xtract)
      colnames(xtract)[which(colnames(xtract) == "tmpret")] <- return.col
    }
    if(is.na(risk.column)){
      tmprisk <- applyFUN(R=R, weights=wts, FUN=risk.col)
      xtract <- cbind(tmprisk, xtract)
      colnames(xtract)[which(colnames(xtract) == "tmprisk")] <- risk.col
    }
    columnnames = colnames(xtract)
    return.column = pmatch(return.col,columnnames)
    if(is.na(return.column)) {
      return.col = paste(return.col,return.col,sep='.')
      return.column = pmatch(return.col,columnnames)
    }
    risk.column = pmatch(risk.col,columnnames)
    if(is.na(risk.column)) {
      risk.col = paste(risk.col,risk.col,sep='.')
      risk.column = pmatch(risk.col,columnnames)
    }
  }
  if(chart.assets){
    # Get the arguments from the optimize.portfolio$portfolio object
    # to calculate the risk and return metrics for the scatter plot. 
    # (e.g. arguments=list(p=0.925, clean="boudt")
    arguments <- NULL # maybe an option to let the user pass in an arguments list?
    if(is.null(arguments)){
      tmp.args <- unlist(lapply(object$portfolio$objectives, function(x) x$arguments), recursive=FALSE)
      tmp.args <- tmp.args[!duplicated(names(tmp.args))]
      if(!is.null(tmp.args$portfolio_method)) tmp.args$portfolio_method <- "single"
      arguments <- tmp.args
    }
    # Include risk reward scatter of asset returns
    asset_ret <- scatterFUN(R=R, FUN=return.col, arguments)
    asset_risk <- scatterFUN(R=R, FUN=risk.col, arguments)
    xlim <- range(c(xtract[,risk.column], asset_risk))
    ylim <- range(c(xtract[,return.column], asset_ret))
  } else {
    asset_ret <- NULL
    asset_risk <- NULL
  }
  
  # plot the portfolios from PSOoutput
  plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="darkgray", axes=FALSE, xlim=xlim, ylim=ylim, ...)
  
  ## @TODO: Generalize this to find column containing the "risk" metric
  if(length(names(object)[which(names(object)=='constrained_objective')])) {
    result.slot<-'constrained_objective'
  } else {
    result.slot<-'objective_measures'
  }
  objcols<-unlist(object[[result.slot]])
  names(objcols)<-PortfolioAnalytics:::name.replace(names(objcols))
  return.column = pmatch(return.col,names(objcols))
  if(is.na(return.column)) {
    return.col = paste(return.col,return.col,sep='.')
    return.column = pmatch(return.col,names(objcols))
  }
  risk.column = pmatch(risk.col,names(objcols))
  if(is.na(risk.column)) {
    risk.col = paste(risk.col,risk.col,sep='.')
    risk.column = pmatch(risk.col,names(objcols))
  }
  # risk and return metrics for the optimal weights if the RP object does not
  # contain the metrics specified by return.col or risk.col
  if(is.na(return.column) | is.na(risk.column)){
    return.col <- gsub("\\..*", "", return.col)
    risk.col <- gsub("\\..*", "", risk.col)
    # warning(return.col,' or ', risk.col, ' do  not match extractStats output of $objective_measures slot')
    opt_weights <- object$weights
    ret <- as.numeric(applyFUN(R=R, weights=opt_weights, FUN=return.col))
    risk <- as.numeric(applyFUN(R=R, weights=opt_weights, FUN=risk.col))
    points(risk, ret, col="blue", pch=16) #optimal
    text(x=risk, y=ret, labels="Optimal",col="blue", pos=4, cex=0.8)
  } else {
    points(objcols[risk.column], objcols[return.column], col="blue", pch=16) # optimal
    text(x=objcols[risk.column], y=objcols[return.column], labels="Optimal",col="blue", pos=4, cex=0.8)
  }

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
#' @method chart.RiskReward optimize.portfolio.pso
#' @S3method chart.RiskReward optimize.portfolio.pso
chart.RiskReward.optimize.portfolio.pso <- chart.Scatter.pso


charts.pso <- function(pso, return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="PSO.Portfolios", xlim=NULL, ylim=NULL, ...){
  # Specific to the output of the optimize_method=pso
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),heights=c(2,2),widths=1)
  par(mar=c(4,4,4,2))
  chart.Scatter.pso(object=pso, return.col=return.col, risk.col=risk.col, chart.assets=chart.assets, element.color=element.color, cex.axis=cex.axis, main=main, xlim=xlim, ylim=ylim, ...=...)
  par(mar=c(2,4,0,2))
  chart.Weights.pso(object=pso, neighbors=neighbors, las=3, xlab=NULL, cex.lab=1, element.color=element.color, cex.axis=cex.axis, ...=..., main="")
  par(op)
}


#' @rdname plot
#' @method plot optimize.portfolio.pso
#' @S3method plot optimize.portfolio.pso
plot.optimize.portfolio.pso <- function(x, ..., return.col="mean", risk.col="ES", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="PSO.Portfolios", xlim=NULL, ylim=NULL){
  charts.pso(pso=x, return.col=return.col, risk.col=risk.col, chart.assets=FALSE, cex.axis=cex.axis, element.color=element.color, neighbors=neighbors, main=main, xlim=xlim, ylim=ylim, ...=...)
}

#' Generic method to chart risk contribution
#' 
#' This function is the generic method to chart risk budget objectives for 
#' \code{optimize.portfolio}, \code{optimize.portfolio.rebalancing}, and 
#' \code{opt.list} objects. This function charts the contribution or percent 
#' contribution of the resulting objective measures of a 
#' \code{risk_budget_objective}. The risk contributions for \code{optimize.portfolio.rebalancing}
#' objects are plotted through time with \code{\link[PerformanceAnalytics]{chart.StackedBar}}.
#' 
#' @details
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors. This will extract the 
#' \code{neighbors} closest to the portfolios in terms of the \code{out} 
#' numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that 
#' correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and 
#' should contain properly named contribution and pct_contrib columns. 
#' 
#' @param object optimal portfolio object created by \code{\link{optimize.portfolio}}
#' @param \dots any other passthru parameters to \code{\link{plot}}
#' @param neighbors risk contribution or pct_contrib of neighbor portfolios to be plotted, see Details.
#' @param match.col string of risk column to match. The \code{opt.list} object 
#' may contain risk budgets for ES or StdDev and this will match the proper 
#' column names of the objectives list outp (e.g. ES.contribution).
#' @param risk.type "absolute" or "percentage" to plot risk contribution in absolute terms or percentage contribution.
#' @param main main title for the chart.
#' @param plot.type "line" or "barplot".
#' @param ylab label for the y-axis.
#' @param xlab label for the x-axis.
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param cex.lab the magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis [\emph{default}],}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical.}
#'       }
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @param colorset color palette or vector of colors to use
#' @param legend.loc legend.loc NULL, "topright", "right", or "bottomright". If legend.loc is NULL, the legend will not be plotted
#' @param cex.legend The magnification to be used for the legend relative to the current setting of \code{cex}
#' @seealso \code{\link{optimize.portfolio}} \code{\link{optimize.portfolio.rebalancing}} \code{\link[PerformanceAnalytics]{chart.StackedBar}}
#' @export
chart.RiskBudget <- function(object, ...){
  UseMethod("chart.RiskBudget")
}

#' @rdname chart.RiskBudget
#' @method chart.RiskBudget optimize.portfolio
#' @S3method chart.RiskBudget optimize.portfolio
chart.RiskBudget.optimize.portfolio <- function(object, ..., neighbors=NULL, risk.type="absolute", main="Risk Contribution", ylab="", xlab=NULL, cex.axis=0.8, cex.lab=0.8, element.color="darkgray", las=3, ylim=NULL){
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class optimize.portfolio")
  portfolio <- object$portfolio
  # class of each objective
  obj_class <- sapply(portfolio$objectives, function(x) class(x)[1])
  
  if(!("risk_budget_objective" %in% obj_class)) print("no risk_budget_objective")
  
  # Get the index number of the risk_budget_objectives
  rb_idx <- which(obj_class == "risk_budget_objective")
  
  if(length(rb_idx) > 1) message(paste(length(rb_idx), "risk_budget_objectives, generating multiple plots."))
  
  # list to store $contribution values
  contrib <- list()
  
  # list to store $pct_contrib values
  pct_contrib <- list()
  
  idx <- NULL
  for(i in 1:length(object$objective_measures)){
    if(length(object$objective_measures[[i]]) > 1){
      # we have an objective measure with contribution and pct_contrib
      contrib[[i]] <- object$objective_measures[[i]][2]
      pct_contrib[[i]] <- object$objective_measures[[i]][3]
      idx <- c(idx, i)
    }
  }
  
  columnnames <- names(object$weights)
  numassets <- length(columnnames)
  
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
  
  if(risk.type == "absolute"){
    for(ii in 1:length(idx)){
      if(is.null(ylim)){
        ylim <- range(contrib[[idx[ii]]][[1]])
        ylim[1] <- min(0, ylim[1])
        ylim[2] <- ylim[2] * 1.15
      }
      objname <- portfolio$objectives[[rb_idx[i]]]$name
      # Plot values of contribution
      plot(contrib[[idx[ii]]][[1]], type="n", axes=FALSE, xlab="", ylim=ylim, ylab=paste(objname, "Contribution", sep=" "), main=main, cex.lab=cex.lab, ...)
      
      # neighbors needs to be in the loop if there is more than one risk_budget_objective
      if(!is.null(neighbors)){
        if(is.vector(neighbors)){
          xtract <- extractStats(object)
          riskcols <- grep(paste(objname, "contribution", sep="."), colnames(xtract))
          if(length(riskcols) == 0) stop("Could not extract risk column")
          if(length(neighbors) == 1){
            # overplot nearby portfolios defined by 'out'
            orderx <- order(xtract[,"out"])
            subsetx <- head(xtract[orderx,], n=neighbors)
            for(i in 1:neighbors) points(subsetx[i, riskcols], type="b", col="lightblue")
          } else {
            # assume we have a vector of portfolio numbers
            subsetx <- xtract[neighbors, riskcols]
            for(i in 1:length(neighbors)) points(subsetx[i,], type="b", col="lightblue")
          }
        } # end if neighbors is a vector
        if(is.matrix(neighbors) | is.data.frame(neighbors)){
          # the user has likely passed in a matrix containing calculated values for contrib or pct_contrib
          nbriskcol <- grep(paste(objname, "contribution", sep="."), colnames(neighbors))
          if(length(nbriskcol) == 0) stop(paste("must have '", objname,".contribution' as column name in neighbors",sep=""))
          if(length(nbriskcol) != numassets) stop("number of 'contribution' columns must equal number of assets")
          for(i in 1:nrow(neighbors)) points(as.numeric(neighbors[i, nbriskcol]), type="b", col="lightblue")
          # note that here we need to get risk cols separately from the matrix, not from xtract
          # also note the need for as.numeric.  points() doesn't like matrix inputs
        } # end neighbors plot for matrix or data.frame
      } # end if neighbors is not null
      points(contrib[[idx[ii]]][[1]], type="b", ...)
      axis(2, cex.axis = cex.axis, col = element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
      box(col = element.color)
    } # end for loop of risk_budget_objective
  } # end plot for absolute risk.type
  
  if(risk.type %in% c("percent", "percentage", "pct_contrib")){
    for(ii in 1:length(rb_idx)){
      min_prisk <- portfolio$objectives[[rb_idx[ii]]]$min_prisk
      max_prisk <- portfolio$objectives[[rb_idx[ii]]]$max_prisk
      if(is.null(ylim)){
        #ylim <- range(c(max_prisk, pct_contrib[[i]][[1]]))
        #ylim[1] <- min(0, ylim[1])
        #ylim[2] <- ylim[2] * 1.15
        ylim <- c(0, 1)
      }
      objname <- portfolio$objectives[[rb_idx[i]]]$name
      # plot percentage contribution
      plot(pct_contrib[[idx[ii]]][[1]], type="n", axes=FALSE, xlab='', ylim=ylim, ylab=paste(objname, " % Contribution", sep=" "), main=main, cex.lab=cex.lab, ...)
      # Check for minimum percentage risk (min_prisk) argument
      if(!is.null(min_prisk)){
        points(min_prisk, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
      }
      if(!is.null(max_prisk)){
        points(max_prisk, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
      }
      
      # neighbors needs to be in the loop if there is more than one risk_budget_objective
      if(!is.null(neighbors)){
        if(is.vector(neighbors)){
          xtract <- extractStats(object)
          if(risk.type == "absolute"){
            riskcols <- grep(paste(objname, "contribution", sep="."), colnames(xtract))
          } else if(risk.type %in% c("percent", "percentage", "pct_contrib")){
            riskcols <- grep(paste(objname, "pct_contrib", sep="."), colnames(xtract))
          }
          if(length(riskcols) == 0) stop("Could not extract risk column")
          if(length(neighbors) == 1){
            # overplot nearby portfolios defined by 'out'
            orderx <- order(xtract[,"out"])
            subsetx <- head(xtract[orderx,], n=neighbors)
            for(i in 1:neighbors) points(subsetx[i, riskcols], type="b", col="lightblue")
          } else {
            # assume we have a vector of portfolio numbers
            subsetx <- xtract[neighbors, riskcols]
            for(i in 1:length(neighbors)) points(subsetx[i,], type="b", col="lightblue")
          }
        } # end if neighbors is a vector
        if(is.matrix(neighbors) | is.data.frame(neighbors)){
          # the user has likely passed in a matrix containing calculated values for contrib or pct_contrib
          nbriskcol <- grep(paste(objname, "pct_contrib", sep="."), colnames(neighbors))
          if(length(nbriskcol) == 0) stop(paste("must have '", objname,".pct_contrib' as column name in neighbors",sep=""))
          if(length(nbriskcol) != numassets) stop("number of 'pct_contrib' columns must equal number of assets")
          for(i in 1:nrow(neighbors)) points(as.numeric(neighbors[i, nbriskcol]), type="b", col="lightblue")
          # note that here we need to get risk cols separately from the matrix, not from xtract
          # also note the need for as.numeric.  points() doesn't like matrix inputs
        } # end neighbors plot for matrix or data.frame
      } # end if neighbors is not null
      points(pct_contrib[[idx[ii]]][[1]], type="b", ...)
      axis(2, cex.axis = cex.axis, col = element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
      box(col = element.color)
    } # end for loop of risk_budget_objective
  } # end plot for pct_contrib risk.type
}

#' @rdname chart.RiskBudget
#' @method chart.RiskBudget optimize.portfolio.rebalancing
#' @S3method chart.RiskBudget optimize.portfolio.rebalancing
chart.RiskBudget.optimize.portfolio.rebalancing <- function(object, ..., match.col="ES", risk.type="absolute", main="Risk Contribution"){
  
  # Get the objective measures at each rebalance period
  rebal.obj <- extractObjectiveMeasures(object)
  
  if(risk.type == "absolute"){
    rbcols <- grep(paste(match.col, "pct_contrib", sep="."), colnames(rebal.obj))
    if(length(rbcols) < 1) stop(paste("No ", match.col, ".contribution columns.", sep=""))
    rbdata <- rebal.obj[, rbcols]
    chart.StackedBar(w=rbdata, ylab=paste(match.col, "Contribution", sep=" "), main=main, ...)
  }
  
  if(risk.type %in% c("percent", "percentage", "pct_contrib")){
    rbcols <- grep(paste(match.col, "pct_contrib", sep="."), colnames(rebal.obj))
    if(length(rbcols) < 1) stop(paste("No ", match.col, ".pct_contrib columns.", sep=""))
    rbdata <- rebal.obj[, rbcols]
    chart.StackedBar(w=rbdata, ylab=paste(match.col, "% Contribution", sep=" "), main=main, ...)
  }
}


#' @rdname chart.RiskBudget
#' @method chart.RiskBudget opt.list
#' @S3method chart.RiskBudget opt.list
chart.RiskBudget.opt.list <- function(object, ..., match.col="ES", risk.type="absolute", main="Risk Budget", plot.type="line", cex.axis=0.8, cex.lab=0.8, element.color="darkgray", las=3, ylim=NULL, colorset=NULL, legend.loc=NULL, cex.legend=0.8){
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  
  if(plot.type %in% c("bar", "barplot")){
    barplotRiskBudget(object=object, ...=..., match.col=match.col, risk.type=risk.type, main=main, ylim=ylim, cex.axis=cex.axis, cex.lab=cex.lab, element.color=element.color, las=las, colorset=colorset, legend.loc=legend.loc, cex.legend=cex.legend)
  } else if(plot.type == "line"){
    
    xtract <- extractObjectiveMeasures(object)
    
    if(risk.type == "absolute"){
      # get the index of columns with risk budget
      rbcols <- grep(paste(match.col, "contribution", sep="."), colnames(xtract))
      dat <- na.omit(xtract[, rbcols])
      if(ncol(dat) < 1) stop("No data to plot after na.omit")
      opt_names <- rownames(dat)
      # remove everything up to the last dot (.) to extract the names
      colnames(dat) <- gsub("(.*)\\.", "", colnames(dat))
      
      # set the colors
      if(is.null(colorset)) colorset <- 1:nrow(dat)
      columnnames <- colnames(dat)
      numassets <- length(columnnames)
      
      xlab <- NULL
      if(is.null(xlab))
        minmargin <- 3
      else
        minmargin <- 5
      if(main=="") topmargin=1 else topmargin=4
      if(las > 1) {# set the bottom border to accommodate labels
        bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
        if(bottommargin > 10 ) {
          bottommargin <- 10
          columnnames<-substr(columnnames,1,19)
          # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
        }
      }
      else {
        bottommargin = minmargin
      }
      par(mar = c(bottommargin, 4, topmargin, 2) +.1)
      
      if(is.null(ylim)) ylim <- range(dat)
      
      plot(dat[1,], type="n", ylim=ylim, xlab='', ylab=paste(match.col, "Contribution", sep=" "), main=main, cex.lab=cex.lab, axes=FALSE)
      for(i in 1:nrow(dat)){
        points(dat[i, ], type="b", col=colorset[i], ...) # add dots here
      }
      
      # set the axis
      axis(2, cex.axis=cex.axis, col=element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis=cex.axis, col=element.color)
      box(col=element.color)
      
      # Add a legend
      if(!is.null(legend.loc)) legend(legend.loc, legend=opt_names, col=colorset, lty=1, bty="n", cex=cex.legend)
    }
    
    if(risk.type %in% c("percent", "percentage", "pct_contrib")){
      # get the index of columns with risk budget
      rbcols <- grep(paste(match.col, "pct_contrib", sep="."), colnames(xtract))
      dat <- na.omit(xtract[, rbcols])
      if(ncol(dat) < 1) stop("No data to plot after na.omit")
      opt_names <- rownames(dat)
      # remove everything up to the last dot (.) to extract the names
      colnames(dat) <- gsub("(.*)\\.", "", colnames(dat))
      
      # set the colors
      if(is.null(colorset)) colorset <- 1:nrow(dat)
      
      columnnames <- colnames(dat)
      numassets <- length(columnnames)
      
      xlab <- NULL
      if(is.null(xlab))
        minmargin <- 3
      else
        minmargin <- 5
      if(main=="") topmargin=1 else topmargin=4
      if(las > 1) {# set the bottom border to accommodate labels
        bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
        if(bottommargin > 10 ) {
          bottommargin <- 10
          columnnames<-substr(columnnames,1,19)
          # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
        }
      }
      else {
        bottommargin = minmargin
      }
      par(mar = c(bottommargin, 4, topmargin, 2) +.1)
      
      if(is.null(ylim)) ylim <- range(dat)
      
      plot(dat[1,], type="n", ylim=ylim, xlab='', ylab=paste(match.col, "% Contribution", sep=" "), main=main, cex.lab=cex.lab, axes=FALSE)
      for(i in 1:nrow(dat)){
        points(dat[i, ], type="b", col=colorset[i], ...) # add dots here
      }
      
      axis(2, cex.axis=cex.axis, col=element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis=cex.axis, col=element.color)
      box(col=element.color)
      
      # Add a legend
      if(!is.null(legend.loc)) legend(legend.loc, legend=opt_names, col=colorset, lty=1, bty="n", cex=cex.legend)
    }
  }
}

# This function is called inside chart.RiskBudget.opt.list when plot.type == "bar" or "barplot"
barplotRiskBudget <- function(object, ..., match.col="ES", risk.type="absolute", main="Risk Budget", cex.axis=0.8, cex.lab=0.8, element.color="darkgray", las=3, colorset=NULL, legend.loc=NULL, cex.legend=0.8){
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  
  xtract <- extractObjectiveMeasures(object)
  
  if(risk.type == "absolute"){
    # get the index of columns with risk budget
    rbcols <- grep(paste(match.col, "contribution", sep="."), colnames(xtract))
    dat <- na.omit(xtract[, rbcols])
    if(ncol(dat) < 1) stop("No data to plot after na.omit")
    opt_names <- rownames(dat)
    # remove everything up to the last dot (.) to extract the names
    colnames(dat) <- gsub("(.*)\\.", "", colnames(dat))
    
    columnnames <- colnames(dat)
    numassets <- length(columnnames)
    
    xlab <- NULL
    if(is.null(xlab))
      minmargin <- 3
    else
      minmargin <- 5
    if(main=="") topmargin=1 else topmargin=4
    if(las > 1) {# set the bottom border to accommodate labels
      bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
      if(bottommargin > 10 ) {
        bottommargin <- 10
        columnnames<-substr(columnnames,1,19)
        # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
      }
    }
    else {
      bottommargin = minmargin
    }
    par(mar = c(bottommargin, 4, topmargin, 2) +.1)
    
    # set the colors
    if(is.null(colorset)) colorset <- 1:nrow(dat)
    
    # plot the data
    barplot(dat, names.arg=columnnames, las=las, cex.names=cex.axis, xlab='', col=colorset, main=main, ylab=paste(match.col, "Contribution", sep=" "), cex.lab=cex.lab, cex.axis=cex.axis, beside=TRUE, ...)
    
    # set the axis
    #axis(2, cex.axis=cex.axis, col=element.color)
    #axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis=cex.axis, col=element.color)
    box(col=element.color)
    
    # Add a legend
    if(!is.null(legend.loc)) legend(legend.loc, legend=opt_names, fill=colorset, bty="n", cex=cex.legend)
  }
  
  if(risk.type %in% c("percent", "percentage", "pct_contrib")){
    # get the index of columns with risk budget
    rbcols <- grep(paste(match.col, "pct_contrib", sep="."), colnames(xtract))
    dat <- na.omit(xtract[, rbcols])
    if(ncol(dat) < 1) stop("No data to plot after na.omit")
    opt_names <- rownames(dat)
    # remove everything up to the last dot (.) to extract the names
    colnames(dat) <- gsub("(.*)\\.", "", colnames(dat))
    
    columnnames <- colnames(dat)
    numassets <- length(columnnames)
    
    xlab <- NULL
    if(is.null(xlab))
      minmargin <- 3
    else
      minmargin <- 5
    if(main=="") topmargin=1 else topmargin=4
    if(las > 1) {# set the bottom border to accommodate labels
      bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * cex.lab
      if(bottommargin > 10 ) {
        bottommargin <- 10
        columnnames<-substr(columnnames,1,19)
        # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
      }
    }
    else {
      bottommargin = minmargin
    }
    par(mar = c(bottommargin, 4, topmargin, 2) +.1)
    
    # set the colors
    if(is.null(colorset)) colorset <- 1:nrow(dat)
    
    # plot the data
    barplot(dat, names.arg=columnnames, las=las, cex.names=cex.axis, col=colorset, main=main, ylab=paste(match.col, "% Contribution", sep=" "), cex.lab=cex.lab, cex.axis=cex.axis, beside=TRUE, ...)
    
    #axis(2, cex.axis=cex.axis, col=element.color)
    #axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis=cex.axis, col=element.color)
    box(col=element.color)
    
    # Add a legend
    if(!is.null(legend.loc)) legend(legend.loc, legend=opt_names, fill=colorset, bty="n", cex=cex.legend)
  }
}

chart.Weights.ROI <- function(object, ..., neighbors = NULL, main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8, plot.type="line"){
  
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class 'optimize.portfolio.ROI'")
  
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
}

#' @rdname chart.Weights
#' @method chart.Weights optimize.portfolio.ROI
#' @S3method chart.Weights optimize.portfolio.ROI
chart.Weights.optimize.portfolio.ROI <- chart.Weights.ROI


chart.Scatter.ROI <- function(object, ..., neighbors=NULL, return.col="mean", risk.col="ES", chart.assets=FALSE, element.color = "darkgray", cex.axis=0.8, xlim=NULL, ylim=NULL, rp=FALSE){
  
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class 'optimize.portfolio.ROI'")
  
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
#' @method chart.RiskReward optimize.portfolio.ROI
#' @S3method chart.RiskReward optimize.portfolio.ROI
chart.RiskReward.optimize.portfolio.ROI <- chart.Scatter.ROI


charts.ROI <- function(ROI, rp=FALSE, risk.col="ES", return.col="mean", chart.assets=FALSE, cex.axis=0.8, element.color="darkgray", neighbors=NULL, main="ROI.Portfolios", xlim=NULL, ylim=NULL, ...){
  # Specific to the output of the optimize_method=ROI
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),heights=c(2,1.5),widths=1)
  par(mar=c(4,4,4,2))
  chart.Scatter.ROI(object=ROI, rp=rp, return.col=return.col, risk.col=risk.col, ..., chart.assets=chart.assets, element.color=element.color, cex.axis=cex.axis, main=main, xlim=xlim, ylim=ylim)
  par(mar=c(2,4,0,2))
  chart.Weights.ROI(object=ROI, neighbors=neighbors, ..., main="", las=3, xlab=NULL, cex.lab=1, element.color=element.color, cex.axis=cex.axis)
  par(op)
}

#' @rdname plot
#' @method plot optimize.portfolio.ROI
#' @S3method plot optimize.portfolio.ROI
plot.optimize.portfolio.ROI <- function(x, ..., rp=FALSE, risk.col="ES", return.col="mean", chart.assets=FALSE, element.color="darkgray", neighbors=NULL, main="ROI.Portfolios", xlim=NULL, ylim=NULL){
  charts.ROI(ROI=x, rp=rp, risk.col=risk.col, return.col=return.col, chart.assets=chart.assets, main=main, xlim=xlim, ylim=ylim, ...)
}

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

#' mapping function to transform or penalize weights that violate constraints
#' 
#' The purpose of the mapping function is to transform a weights vector
#' that does not meet all the constraints into a weights vector that
#' does meet the constraints, if one exists, hopefully with a minimum
#' of transformation.
#' 
#' The first step is to test for violation of the constraint. If the constraint
#' is violated, we will apply a transformation such that the weights vector
#' satisfies the constraints. The following constraint types are tested in
#' the mapping function: leverage, box, group, and position limit. The 
#' transformation logic is based on code from the random portfolio sample method.
#' 
#' If relax=TRUE, we will attempt to relax the constraints if a feasible 
#' portfolio could not be formed with an initial call to \code{rp_transform}. 
#' We will attempt to relax the constraints up to 5 times. If we do not have a 
#' feasible portfolio after attempting to relax the constraints, then we will 
#' default to returning the weights vector that violates the constraints.
#' 
#' @param weights vector of weights
#' @param portfolio object of class \code{portfolio}
#' @param relax TRUE/FALSE, default FALSE. Enable constraints to be relaxed.
#' @param \dots any other passthru parameters
#' @return 
#' \itemize{
#' \item{weights: }{vector of transformed weights meeting constraints.}
#' \item{min: }{vector of min box constraints that may have been modified if relax=TRUE.}
#' \item{max: }{vector of max box constraints that may have been modified if relax=TRUE.}
#' \item{cLO: }{vector of lower bound group constraints that may have been modified if relax=TRUE.}
#' \item{cUP: }{vector of upper bound group constraints that may have been modified if relax=TRUE.}
#' }
#' @author Ross Bennett
#' @export
fn_map <- function(weights, portfolio, relax=FALSE, ...){
  if(!is.portfolio(portfolio)) stop("portfolio passed in is not of class 'portfolio'")
  
  nassets <- length(portfolio$assets)
  
  # step 1: Get the constraints out of the portfolio object
  constraints <- get_constraints(portfolio)
  min_sum <- constraints$min_sum
  max_sum <- constraints$max_sum
  # rp_transform will rarely find a feasible portfolio if there is not some 
  # 'wiggle room' between min_sum and max_sum
  if((max_sum - min_sum) < 0.02){
    min_sum <- min_sum - 0.01
    max_sum <- max_sum + 0.01
  }
  min <- constraints$min
  max <- constraints$max
  groups <- constraints$groups
  cLO <- constraints$cLO
  cUP <- constraints$cUP
  group_pos <- constraints$group_pos
  div_target <- constraints$div_target
  turnover_target <- constraints$turnover_target
  max_pos <- constraints$max_pos
  max_pos_long <- constraints$max_pos_long
  max_pos_short <- constraints$max_pos_short
  tolerance <- .Machine$double.eps^0.5
  
  # We will modify the weights vector so create a temporary copy
  # modified for transformation or to relax constraints
  tmp_weights <- weights
  tmp_min <- min
  tmp_max <- max
  tmp_cLO <- cLO
  tmp_cUP <- cUP
  tmp_max_pos <- max_pos
  tmp_max_pos_long <- max_pos_long
  tmp_max_pos_short <- max_pos_short
  
  # step 2: check that the vector of weights satisfies the constraints, 
  # transform weights if constraint is violated
  # TRUE if the weights vector is in compliance with the constraints
  # FALSE if the weights vector violates the constraint
  
  # check leverage constraints
  if(!is.null(min_sum) & !is.null(max_sum)){
    if(!(sum(tmp_weights) >= min_sum & sum(tmp_weights) <= max_sum)){
      # Try to transform only considering leverage and box constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, max_sum=max_sum, 
                                      min=tmp_min, max=tmp_max, 
                                      groups=NULL, cLO=NULL, cUP=NULL, 
                                      max_pos=NULL, group_pos=NULL, 
                                      max_pos_long=NULL, max_pos_short=NULL, 
                                      max_permutations=500), silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
      } # end try-error recovery
    } # end check for leverage constraint violation
  } # end check for NULL arguments
  
  # check box constraints
  if(!is.null(tmp_min) & !is.null(tmp_max)){
    if(!(all(tmp_weights >= tmp_min) & all(tmp_weights <= tmp_max))){
      # Try to transform only considering leverage and box constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, max_sum=max_sum, 
                                      min=tmp_min, max=tmp_max, 
                                      groups=NULL, cLO=NULL, cUP=NULL, 
                                      max_pos=NULL, group_pos=NULL, 
                                      max_pos_long=NULL, max_pos_short=NULL, 
                                      max_permutations=500), silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Try to relax constraints if relax=TRUE
        if(relax){
          i <- 1
          # loop while constraints are violated and relax constraints
          # try to relax constraints up to 5 times
          while((sum(tmp_weights) < min_sum | sum(tmp_weights) > max_sum | any(tmp_weights < tmp_min) | any(tmp_weights > tmp_max)) & i <= 5){
            # check if min is violated
            if(any(tmp_weights < tmp_min)){
              # Find which elements of min are violated and decrease by a random amount
              tmp_min[which(tmp_weights < tmp_min)] <- tmp_min[which(tmp_weights < tmp_min)] - runif(1, 0.01, 0.05)
            }
            # check if max is violated
            if(any(tmp_weights > tmp_max)){
              # Find which elements of min are violated and increase by a random amount
              tmp_max[which(tmp_weights < tmp_max)] <- tmp_max[which(tmp_weights < tmp_max)] + runif(1, 0.01, 0.05)
            }
            
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, max_sum=max_sum, 
                                            min=tmp_min, max=tmp_max, 
                                            groups=NULL, cLO=NULL, cUP=NULL, 
                                            max_pos=NULL, group_pos=NULL, 
                                            max_pos_long=NULL, max_pos_short=NULL, 
                                            max_permutations=500), silent=TRUE) # FALSE for testing
            # Default to original weights if this fails again
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
          # We have a feasible portfolio in terms of min_sum and max_sum, 
          # but were unable to produce a portfolio that satisfies box constraints
          if(isTRUE(all.equal(tmp_weights, weights))){
            # reset min and max to their original values and penalize later
            tmp_min <- min
            tmp_max <- max
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for box constraint violation
  } # end check for NULL arguments
  
  # check group constraints
  if(!is.null(groups) & !is.null(tmp_cLO) & !is.null(tmp_cUP)){
    if(any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))){
      # Try to transform only considering leverage, box, and group constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, max_sum=max_sum, 
                                      min=tmp_min, max=tmp_max, 
                                      groups=groups, cLO=tmp_cLO, cUP=tmp_cUP, 
                                      max_pos=NULL, group_pos=group_pos, 
                                      max_pos_long=NULL, max_pos_short=NULL, 
                                      max_permutations=500), silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        # Try to relax constraints if relax=TRUE
        if(relax){
          i <- 1
          # loop while constraints are violated and relax constraints
          # Try to relax constraints up to 5 times
          while(((sum(tmp_weights) < min_sum | sum(tmp_weights) > max_sum) | (any(tmp_weights < tmp_min) | any(tmp_weights > tmp_max)) | any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))) & i <= 5){
            if(any(group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP, group_pos))){
              # I know which group failed, but not if it was cUP or cLO that was violated
              # Maybe I can modify group_fail to report back what was violated and only relax cLO or cUP, not both
              # This relaxes both cLO and cUP
              tmp_cLO[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] <- tmp_cLO[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] - runif(1, 0.01, 0.05)
              tmp_cUP[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] <- tmp_cUP[group_fail(tmp_weights, groups, tmp_cLO, tmp_cUP)] + runif(1, 0.01, 0.05)
            }
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, max_sum=max_sum, 
                                            min=tmp_min, max=tmp_max, 
                                            groups=groups, cLO=tmp_cLO, cUP=tmp_cUP, 
                                            max_pos=NULL, group_pos=group_pos, 
                                            max_pos_long=NULL, max_pos_short=NULL, 
                                            max_permutations=500), silent=TRUE) # FALSE for testing
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
          # We have a feasible portfolio in terms of min_sum and max_sum, 
          # but were unable to produce a portfolio that satisfies group constraints
          if(isTRUE(all.equal(tmp_weights, weights))){
            # reset min and max to their original values and penalize later
            tmp_cLO <- cLO
            tmp_cUP <- cUP
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for group constraint violation
  } # end check for NULL arguments
  
  # check position_limit constraints
  if(!is.null(tmp_max_pos) | !is.null(tmp_max_pos_long) | !is.null(tmp_max_pos_short)){
    if(pos_limit_fail(tmp_weights, tmp_max_pos, tmp_max_pos_long, tmp_max_pos_short)){
      # Try to transform only considering leverage, box, group, and position_limit constraints
      tmp_weights <- try(rp_transform(w=tmp_weights, 
                                      min_sum=min_sum, max_sum=max_sum, 
                                      min=tmp_min, max=tmp_max, 
                                      groups=groups, cLO=tmp_cLO, cUP=tmp_cUP, 
                                      max_pos=tmp_max_pos, group_pos=group_pos, 
                                      max_pos_long=tmp_max_pos_long, max_pos_short=tmp_max_pos_short, 
                                      max_permutations=500), silent=TRUE) # FALSE for testing
      if(inherits(tmp_weights, "try-error")){
        # Default to initial weights
        tmp_weights <- weights
        if(relax){
          i <- 1
          while(pos_limit_fail(tmp_weights, tmp_max_pos, tmp_max_pos_long, tmp_max_pos_short) & (i <= 5)){
            # increment tmp_max_pos by 1
            if(!is.null(tmp_max_pos)) tmp_max_pos <- min(nassets, tmp_max_pos + 1)
            if(!is.null(tmp_max_pos_long)) tmp_max_pos_long <- min(nassets, tmp_max_pos_long + 1)
            if(!is.null(tmp_max_pos_short)) tmp_max_pos_short <- min(nassets, tmp_max_pos_short + 1)
            # Now try the transformation again
            tmp_weights <- try(rp_transform(w=tmp_weights, 
                                            min_sum=min_sum, max_sum=max_sum, 
                                            min=tmp_min, max=tmp_max, 
                                            groups=groups, cLO=tmp_cLO, cUP=tmp_cUP, 
                                            max_pos=tmp_max_pos, group_pos=group_pos, 
                                            max_pos_long=tmp_max_pos_long, max_pos_short=tmp_max_pos_short, 
                                            max_permutations=500), silent=TRUE) # FALSE for testing
            if(inherits(tmp_weights, "try-error")) tmp_weights <- weights
            i <- i + 1
          }
        } # end if(relax) statement
      } # end try-error recovery
    } # end check for position limit constraint violation
  } # end check for NULL arguments

  names(tmp_weights) <- names(weights)
  return(list(weights=tmp_weights, 
              min=tmp_min, 
              max=tmp_max, 
              cLO=tmp_cLO, 
              cUP=tmp_cUP, 
              max_pos=tmp_max_pos,
              max_pos_long=tmp_max_pos_long,
              max_pos_short=tmp_max_pos_short))
}



#' Transform a weights vector to satisfy leverage, box, group, and position_limit constraints using logic from \code{randomize_portfolio}
#' 
#' This function uses a block of code from \code{\link{randomize_portfolio}} 
#' to transform the weight vector if either the weight_sum (leverage) 
#' constraints, box constraints, group constraints, or position_limit constraints are violated.
#' The resulting weights vector might be quite different from the original weights vector.
#' 
#' @param w weights vector to be transformed
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying maximum weight box constraints
#' @param groups vector specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @param max_pos maximum assets with non-zero weights
#' @param group_pos vector specifying maximum number assets with non-zero weights per group
#' @param max_pos_long maximum number of assets with long (i.e. buy) positions
#' @param max_pos_short maximum number of assets with short (i.e. sell) positions
#' @param max_permutations integer: maximum number of iterations to try for a valid portfolio, default 200
#' @return named weighting vector
#' @author Peter Carl, Brian G. Peterson, Ross Bennett (based on an idea by Pat Burns)
#' @export
rp_transform <- function(w, min_sum=0.99, max_sum=1.01, min, max, groups, cLO, cUP, max_pos=NULL, group_pos=NULL, max_pos_long=NULL, max_pos_short=NULL, max_permutations=200){
  # Uses logic from randomize_portfolio to "normalize" a weights vector to 
  # satisfy min_sum and max_sum while accounting for box and group constraints
  # Modified from randomize_portfolio to trigger the while loops if any weights 
  # violate min or max box constraints. A weights vector would not be transformed
  # in randomize_portfolio if min_sum and max_sum were satisfied, but the
  # min/max constraints were violated.
  
  # Set the tolerance to determine non-zero weights
  tolerance=.Machine$double.eps^0.5
  
  # Set value for max_pos if it is not specified
  if(is.null(max_pos)) max_pos <- length(w)
  
  # Determine maximum number of non-zero weights
  if(!is.null(group_pos)) {
    max_group_pos <- sum(group_pos)
  } else {
    max_group_pos <- length(w)
  }
  
  # Set maximum number of assets based on max_pos and group_pos
  max_assets <- min(max_pos, max_group_pos)
  
  # Create a temporary min vector that will be modified, because a feasible
  # portfolio is rarely created if all(min > 0). This is due to the while
  # loop that checks any(tmp_w < min).
  tmp_min <- min
  
  # If weight_i = 0 and min_i > 0, then this will violate box constraints
  # even though weight_i = 0 to satisfy position_limit constraints. Modify
  # the tmp_min vector and set tmp_min_i equal to zero where weights_i = 0.
  # If w is less than or equal to tolerance then it is essentially 0
  if(any(abs(w) <= tolerance)){
    if(any(tmp_min[which(abs(w) <= tolerance)] > 0)){
      tmp_min[which(abs(w) <= tolerance)] <- -tolerance
    }
  }
  
  # return w if all constraints are satisfied
  if((sum(w) >= min_sum & sum(w) <= max_sum) & 
       (all(w >= tmp_min) & all(w <= max)) & 
       (all(!group_fail(w, groups, cLO, cUP, group_pos))) &
       !pos_limit_fail(w, max_pos, max_pos_long, max_pos_short)){
    return(w)
  }
  
  # generate a sequence of weights based on min/max box constraints
  weight_seq <- generatesequence(min=min(min), max=max(max), by=0.002)
  # make sure there is a 0 in weight_seq
  if((!is.null(max_pos) | !is.null(group_pos) | !is.null(max_pos_long) | !is.null(max_pos_short)) & !is.element(0, weight_seq)) weight_seq <- c(0, weight_seq)
  
  # start the permutations counter
  permutations <- 1
  
  # create a temporary weights vector that will be modified in the while loops
  tmp_w <- w
  
  # while portfolio is outside min_sum/max_sum or tmp_min/max or group or postion_limit constraints and we have not reached max_permutations
  while ((sum(tmp_w) < min_sum | sum(tmp_w) > max_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | (pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short))) & permutations <= max_permutations) {
    permutations = permutations + 1
    # check our box constraints on total portfolio weight
    # reduce(increase) total portfolio size till you get a match
    # 1> check to see which bound you've failed on, probably set this as a pair of while loops
    # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
    # 3> check and repeat
    
    # reset tmp_w and tmp_min to their original values
    tmp_w <- w
    tmp_min <- min
    
    random_index <- sample(1:length(tmp_w), max_assets)
    
    # Get the index values that are not in random_index and set them equal to 0
    full_index <- 1:length(tmp_w)
    not_index <- setdiff(full_index, random_index)
    tmp_w[not_index] <- 0
    
    # set some tmp_min values equal to zero so the while loops do not see a
    # violation of any(tmp_w < tmp_min). This tends to force weights to 0 and
    # works well for long only, but we may want to allow negative weights.
    # tmp_min[not_index] <- 0
    # Only set values of tmp_min that are greater than 0 to 0
    tmp_min[not_index[which(tmp_min[not_index] > 0)]] <- 0
    
    # Transform weights to satisfy max_pos_long and max_pos_short before being
    # passed into the main loops
    # Both max_pos_long and max_pos_short should be specified
    if(!is.null(max_pos_long)){
      pos_idx <- which(tmp_w > 0)
      neg_idx <- which(tmp_w < 0)
      
      # Check if number of positive weights exceeds max_pos_long
      if(length(pos_idx) > max_pos_long){
        # Randomly sample positive weights that cause violation of max_pos_long
        # and replace with randomly sampled negative weights from weight_seq
        make_neg_idx <- sample(pos_idx, length(pos_idx) - max_pos_long)
        for(i in make_neg_idx){
          tmp_idx <- weight_seq[weight_seq < 0 & weight_seq >= min[i]]
          if(length(tmp_idx) > 0){
            tmp_w[i] <- sample(tmp_idx, 1)
          } else {
            # This should never happen if the correct weight_seq and min is specified
            tmp_w[i] <- -tmp_w[i]
          }
        }
      }
    }
    if(!is.null(max_pos_short)){
      # Check if number of negative weights exceeds max_pos_short
      if(length(neg_idx) > max_pos_short){
        # Randomly sample negative weights that cause violation of max_pos_short
        # and replace with randomly sampled positive weights from weight_seq
        make_pos_idx <- sample(neg_idx, length(neg_idx) - max_pos_short)
        for(i in make_pos_idx){
          tmp_seq <- weight_seq[weight_seq > 0 & weight_seq <= max[i]]
          if(length(tmp_seq) > 0){
            tmp_w[i] <- sample(tmp_seq, 1)
          } else {
            # This should never happen if the correct weight_seq and max is specified
            tmp_w[i] <- -tmp_w[i]
          }
        }
      }
    }
    
    i = 1
    # while sum of weights is less than min_sum or tmp_min/max box or group constraint is violated
    while ((sum(tmp_w) < min_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | (pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short))) & i <= length(tmp_w)) {
      # randomly permute and increase a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      tmp_seq <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
      n_tmp_seq <- length(tmp_seq)
      if (n_tmp_seq > 1) {
        # randomly sample an element from weight_seq that is greater than cur_val and less than max
        # tmp_w[cur_index] <- sample(weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])], 1)
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
        # print(paste("new val:",tmp_w[cur_index]))
      } else {
        if (n_tmp_seq == 1) {
          # tmp_w[cur_index] <- weight_seq[(weight_seq >= cur_val) & (weight_seq <= max[cur_index])]
          tmp_w[cur_index] <- tmp_seq
        }
      }
      i=i+1 # increment our counter
    } # end increase loop
    # need to reset i here otherwise the decreasing loop will be ignored
    # group_fail does not test for direction of violation, just that group constraints were violated
    i = 1 
    # while sum of weights is greater than max_sum or tmp_min/max box or group constraint is violated
    while ((sum(tmp_w) > max_sum | any(tmp_w < tmp_min) | any(tmp_w > max) | any(group_fail(tmp_w, groups, cLO, cUP, group_pos)) | (pos_limit_fail(tmp_w, max_pos, max_pos_long, max_pos_short))) & i <= length(tmp_w)) {
      # randomly permute and decrease a random portfolio element
      cur_index <- random_index[i]
      cur_val <- tmp_w[cur_index]
      tmp_seq <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])]
      n_tmp_seq <- length(tmp_seq)
      if (n_tmp_seq > 1) {
        # randomly sample an element from weight_seq that is less than cur_val and greater than tmp_min
        # tmp_w[cur_index] <- sample(weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])] , 1)
        tmp_w[cur_index] <- tmp_seq[sample.int(n=n_tmp_seq, size=1L, replace=FALSE, prob=NULL)]
      } else {
        if (n_tmp_seq == 1) {
          # tmp_w[cur_index] <- weight_seq[(weight_seq <= cur_val) & (weight_seq >= tmp_min[cur_index])]
          tmp_w[cur_index] <- tmp_seq
        }
      }
      i=i+1 # increment our counter
    } # end decrease loop
  } # end final walk towards the edges
  
  portfolio <- tmp_w
  
  colnames(portfolio)<-colnames(w)
  
  # checks for infeasible portfolio
  # Stop execution and return an error if an infeasible portfolio is created
  # This will be useful in fn_map so that we can catch the error and take
  # action (try again with more permutations, relax constraints, different
  # method to normalize, etc.)
  if (sum(portfolio) < min_sum | sum(portfolio) > max_sum){
    portfolio <- w
    stop("Infeasible portfolio created, perhaps increase max_permutations and/or adjust your parameters.")
  }
#   if(isTRUE(all.equal(w,portfolio))) {
#     if (sum(w)>=min_sum & sum(w)<=max_sum) {
#       warning("Unable to generate a feasible portfolio different from w, perhaps adjust your parameters.")
#       return(w)
#     } else {
#       warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
#       return(NULL)
#     }
#   }
  return(portfolio)
}

#' Test if group constraints have been violated
#' 
#' The function loops through each group and tests if cLO or cUP have been violated
#' for the given group. This is a helper function for \code{\link{rp_transform}}.
#' 
#' @param weights weights vector to test
#' @param groups list of vectors specifying the groups of the assets
#' @param cLO numeric or vector specifying minimum weight group constraints
#' @param cUP numeric or vector specifying minimum weight group constraints
#' @param group_pos vector specifying the number of non-zero weights per group
#' @return logical vector: TRUE if group constraints are violated for a given group
#' @author Ross Bennett
group_fail <- function(weights, groups, cLO, cUP, group_pos=NULL){
  # return FALSE if groups, cLO, or cUP is NULL
  if(is.null(groups) | is.null(cLO) | is.null(cUP)) return(FALSE)
  group_count <- sapply(groups, length)
  # group_pos sets a limit on the number of non-zero weights by group
  # Set equal to groups if NULL
  if(is.null(group_pos)) group_pos <- group_count
  tolerance <- .Machine$double.eps^0.5
  
  n.groups <- length(groups)
  group_fail <- vector(mode="logical", length=n.groups)
  
  for(i in 1:n.groups){
    # sum of the weights for a given group
    tmp.w <- weights[groups[[i]]]
    group_fail[i] <- ( (sum(tmp.w) < cLO[i]) | (sum(tmp.w) > cUP[i]) | (sum(abs(tmp.w) > tolerance) > group_pos[i]) )
  }
  # returns logical vector of groups. TRUE if either cLO or cUP is violated
  return(group_fail)
}

#' function to check for violation of position limits constraints
#' 
#' This is used as a helper function for \code{\link{rp_transform}} to check
#' for violation of position limit constraints. The position limit constraints
#' checked are max_pos, max_pos_long, and max_pos_short. 
#' 
#' @param weights vector of weights to test
#' @param max_pos maximum number of assets with non-zero weights
#' @param max_pos_long maximum number of assets with long (i.e. buy) positions
#' @param max_pos_short maximum number of assets with short (i.e. sell) positions
#' @return TRUE if any position_limit is violated. FALSE if all position limits are satisfied
#' @export
pos_limit_fail <- function(weights, max_pos, max_pos_long, max_pos_short){
  # tolerance for "non-zero" definition
  tolerance <- .Machine$double.eps^0.5
  
  # Check if max_pos is violated
  if(!is.null(max_pos)){
    if(sum(abs(weights) > tolerance) > max_pos){
      return(TRUE)
    }
  }
  
  # Check if max_pos_long is violated
  if(!is.null(max_pos_long)){
    if(sum(weights > tolerance) > max_pos_long){
      return(TRUE)
    }
  }
  
  # Check if max_pos_short is violated
  if(!is.null(max_pos_short)){
    if(sum(weights < -tolerance) > max_pos_short){
      return(TRUE)
    }
  }
  # Return FALSE if nothing is violated
  return(FALSE)
}

# test
# w <- c(0.1, 0.25, 0.3, 0.15, 0.05, 0.15)
# min <- rep(0.1, length(w))
# max <- rep(0.45, length(w))
# w1 <- rp_normalize(w=w, min_sum=0.99, max_sum=1.01, min=min, max=max)
# w1
# sum(w1)
# any(w1 < min)
# any(w1 > max)

# library(PortfolioAnalytics)
# data(edhec)
# ret <- edhec[, 1:4]
# funds <- colnames(ret)
# 
# pspec <- portfolio.spec(assets=funds)
# pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0.99, max_sum=1.01, enabled=TRUE)
# pspec <- add.constraint(portfolio=pspec, type="box", enabled=TRUE)
# pspec <- add.constraint(portfolio=pspec, type="group", groups=c(2,2), group_min=c(0.1, 0.2), group_max=c(0.3, 0.8), enabled=TRUE)
# 
# weights <- c(0.15, 0.2, 0.15, 0.5)
# sum(weights)
# 
# (w <- constraint_fn_map(weights, pspec))
# sum(w)

#' Function to compute diversification as a constraint
#' 
#' Diversification is defined as 1 minus the sum of the squared weights
#' \deqn{diversification = 1 - sum(w^2)}
#' 
#' @param weights vector of asset weights
#' @author Ross Bennett
#' @export
diversification <- function(weights){
   div <- 1 - sum(weights^2)
   return(div)
}
#' constructor for class constraint_ROI
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param op.problem an object of type "OP" (optimization problem, of \code{ROI}) specifying the complete optimization problem, see ROI help pages for proper construction of OP object.
#' @param solver string argument for what solver package to use, must have ROI plugin installed for that solver.  Currently support is for \code{glpk} and \code{quadprog}.
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
#' @author Hezky Varon
#' @export
constraint_ROI <- function(assets=NULL, op.problem, solver=c("glpk", "quadprog"), weight_seq=NULL) 
{
  #
  # Structure for this constructor function borrowed from "constraints.R"
  #
  if(is.null(op.problem) | !inherits(op.problem, "OP")) 
    stop("Need to pass in optimization problem of ROI:::OP object type.")
  
  if (is.null(assets)) {
    stop("You must specify the assets")
  }
  
  if(is.character(assets)){
    nassets=length(assets)
    assetnames=assets
    message("assuming equal weighted seed portfolio")
    assets<-rep(1/nassets,nassets)
    names(assets)<-assetnames  # set names, so that other code can access it,
    # and doesn't have to know about the character vector
    # print(assets)
  }
  if(!is.null(assets)){
    # TODO FIXME this doesn't work quite right on matrix of assets
    if(is.numeric(assets)){
      if (length(assets) == 1) {
        nassets=assets
        #we passed in a number of assets, so we need to create the vector
        message("assuming equal weighted seed portfolio")
        assets<-rep(1/nassets,nassets)
      } else {
        nassets = length(assets)
      }
      # and now we may need to name them
      if (is.null(names(assets))) {
        for(i in 1:length(assets)){
          names(assets)[i]<-paste("Asset",i,sep=".")
        }
      }
    }
  }
  print(paste("You chose to use the ",solver[1]," solver", sep=""))
  return(structure(
    list(
         assets = assets,
         constrainted_objective = op.problem,
         solver = solver[1],
         weight_seq = weight_seq,
         objectives = list(),
         call = match.call()
        ), 
    class=c("constraint_ROI","constraint")
    ))
}

#' Create an equal weight portfolio
#' 
#' This function calculates objective measures for an equal weight portfolio.
#' 
#' @details
#' This function is simply a wrapper around \code{\link{constrained_objective}}
#' to calculate the objective measures in the given \code{portfolio} object of
#' an equal weight portfolio. The portfolio object should include all objectives
#' to be calculated.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param \dots any other passthru parameters to \code{constrained_objective}
#' @return a list containing the returns, weights, objective measures, call, and portfolio object
#' @author Ross Bennett
#' @export
equal.weight <- function(R, portfolio, ...){
  # Check for portfolio object passed in
  if(!is.portfolio(portfolio)) stop("portfolio object passed in must be of class 'portfolio'")
  
  # get asset information for equal weight portfolio
  assets <- portfolio$assets
  nassets <- length(assets)
  weights <- rep(1 / nassets, nassets)
  names(weights) <- names(assets)
  
  # make sure the number of columns in R matches the number of assets
  if(ncol(R) != nassets){
    if(ncol(R) > nassets){
      R <- R[, 1:nassets]
      warning("number of assets is less than number of columns in returns object, subsetting returns object.")
    } else {
      stop("number of assets is greater than number of columns in returns object")
    }
  }
  
  tmpout <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE, ...)
  return(structure(list(
    R=R,
    weights=weights,
    out=tmpout$out,
    objective_measures=tmpout$objective_measures,
    call=match.call(),
    portfolio=portfolio),
                   class=c("optimize.portfolio.eqwt", "optimize.portfolio"))
  )
}




#' Create an inverse volatility weighted portfolio
#' 
#' This function calculates objective measures for an equal weight portfolio.
#' 
#' @details
#' This function is simply a wrapper around \code{\link{constrained_objective}}
#' to calculate the objective measures in the given \code{portfolio} object of
#' an inverse volatility weight portfolio. The portfolio object should include all objectives
#' to be calculated.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param \dots any other passthru parameters to \code{constrained_objective}
#' @return a list containing the returns, weights, objective measures, call, and portfolio object
#' @author Peter Carl
#' @export
inverse.volatility.weight <- function(R, portfolio, ...){
  # Check for portfolio object passed in
  if(!is.portfolio(portfolio)) stop("portfolio object passed in must be of class 'portfolio'")
  
  # get asset information for equal weight portfolio
  assets <- portfolio$assets
  nassets <- length(assets)
  
  # make sure the number of columns in R matches the number of assets
  if(ncol(R) != nassets){
    if(ncol(R) > nassets){
      R <- R[, 1:nassets]
      warning("number of assets is less than number of columns in returns object, subsetting returns object.")
    } else {
      stop("number of assets is greater than number of columns in returns object")
    }
  }
  
  weights <- as.vector((1/StdDev(R))/sum(1/StdDev(R)))
  names(weights) <- names(assets)
  
  tmpout <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE, ...)
  return(structure(list(
    R=R,
    weights=weights,
    out=tmpout$out,
    objective_measures=tmpout$objective_measures,
    call=match.call(),
    portfolio=portfolio),
                   class=c("optimize.portfolio.invol", "optimize.portfolio"))
  )
}
#' Calculates turnover given two vectors of weights.
#' This is used as an objective function and is called when the user adds an objective of type turnover with \code{\link{add.objective}}
#' @param weights vector of weights from optimization
#' @param wts.init vector of initial weights used to calculate turnover from
#' @author Ross Bennett
#' @export
turnover <- function(weights, wts.init=NULL) {
  # turnover function from https://r-forge.r-project.org/scm/viewvc.php/pkg/PortfolioAnalytics/sandbox/script.workshop2012.R?view=markup&root=returnanalytics
  
  N <- length(weights)
  
  # If wts.init is not given, then assume a vector of equal weights
  if(is.null(wts.init)) {
    wts.init <- rep(1/N, N)
  }
  
  # Check that weights and wts.init are the same length
  if(length(weights) != length(wts.init)) stop("weights and wts.init are not the same length")
  
  return(sum(abs(wts.init - weights)) / N)
}

#' Calculate portfolio variance
#' 
#' This function is used to calculate the portfolio variance via a call to 
#' constrained_objective when var is an object for mean variance or quadratic 
#' utility optimization.
#' 
#' @param R xts object of asset returns
#' @param weights vector of asset weights
#' @return numeric value of the portfolio variance
#' @author Ross Bennett
#' @export
var.portfolio <- function(R, weights){
  weights <- matrix(weights, ncol=1)
  return(as.numeric(t(weights) %*% var(R) %*% weights))
}

#' Concentration of weights
#' 
#' This function computes the concentration of weights using the Herfindahl Hirschman Index
#' 
#' @param weights set of portfolio weights
#' @param groups list of vectors of grouping
#' @author Ross Bennett
#' @export
HHI <- function(weights, groups=NULL){
  
  # calculate overall HHI
  hhi <- sum(weights^2)
  
  # calculate group HHI
  if(!is.null(groups)){
    ngroups <- length(groups)
    group_hhi <- rep(0, ngroups)
    if(!is.null((names(groups)))) names(group_hhi) <- names(groups)
    for(i in 1:ngroups){
      group_hhi[i] <- sum(weights[groups[[i]]]^2)
    }
    return(list(HHI=hhi, Groups_HHI=group_hhi))
  } else {
    return(hhi)
  }
}

# portfolio mean return
port.mean <- function(weights, mu){
  # t(weights) %*% moments$mu
  as.numeric(crossprod(weights, mu))
}

##### GMV and QU QP Function #####
#' GMV/QU QP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param lambda_hhi concentration aversion parameter
#' @param conc_groups list of vectors specifying the groups of the assets.
#' @param solver solver to use
#' @author Ross Bennett
gmv_opt <- function(R, constraints, moments, lambda, target, lambda_hhi, conc_groups, solver="quadprog"){
  stopifnot("package:ROI" %in% search() || require("ROI", quietly = TRUE))
  stopifnot("package:ROI.plugin.quadprog" %in% search() || require("ROI.plugin.quadprog", quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of assets
  N <- ncol(R)
  
  # Check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- tmp_means
  dir.vec <- "=="
  rhs.vec <- target
  meq <- 1
  
  # Set up initial A matrix for leverage constraints
  Amat <- rbind(Amat, rep(1, N), rep(-1, N))
  dir.vec <- c(dir.vec, ">=",">=")
  rhs.vec <- c(rhs.vec, constraints$min_sum, -constraints$max_sum)
  
  # Add min box constraints
  Amat <- rbind(Amat, diag(N))
  dir.vec <- c(dir.vec, rep(">=", N))
  rhs.vec <- c(rhs.vec, constraints$min)
  
  # Add max box constraints
  Amat <- rbind(Amat, -1*diag(N))
  dir.vec <- c(dir.vec, rep(">=", N))
  rhs.vec <- c(rhs.vec, -constraints$max)
  
  # Include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir.vec, and rhs.vec
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, t.B, -t.B)
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }

  # quadprog cannot handle infinite values so replace Inf with .Machine$double.xmax
  # This is the strategy used in ROI
  # Amat[ is.infinite(Amat) & (Amat <= 0) ] <- -.Machine$double.xmax
  # Amat[ is.infinite(Amat) & (Amat >= 0) ] <-  .Machine$double.xmax
  # rhs.vec[is.infinite(rhs.vec) & (rhs.vec <= 0)] <- -.Machine$double.xmax
  # rhs.vec[is.infinite(rhs.vec) & (rhs.vec >= 0)] <- .Machine$double.xmax
  
  # Remove the rows of Amat and elements of rhs.vec where rhs.vec is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs.vec), ]
  dir.vec <- dir.vec[!is.infinite(rhs.vec)]
  rhs.vec <- rhs.vec[!is.infinite(rhs.vec)]
  
  # Set up the quadratic objective
  if(!is.null(lambda_hhi)){
    if(length(lambda_hhi) == 1 & is.null(conc_groups)){
      ROI_objective <- Q_objective(Q=2*lambda*(moments$var + lambda_hhi * diag(N)), L=-moments$mean) # ROI
      #Dmat <- 2*lambda*(moments$var + lambda_hhi * diag(N)) # solve.QP
      #dvec <- moments$mean # solve.QP
    } else if(!is.null(conc_groups)){
      # construct the matrix with concentration aversion values by group
      hhi_mat <- matrix(0, nrow=N, ncol=N)
      vec <- 1:N
      for(i in 1:length(conc_groups)){
        tmpI <- diag(N)
        tmpvec <- conc_groups[[i]]
        zerovec <- setdiff(vec, tmpvec)
        for(j in 1:length(zerovec)){
          tmpI[zerovec[j], ] <- rep(0, N)
        }
        hhi_mat <- hhi_mat + lambda_hhi[i] * tmpI
      }
      ROI_objective <- Q_objective(Q=2*lambda*(moments$var + hhi_mat), L=-moments$mean) # ROI
      #Dmat <- 2 * lambda * (moments$var + hhi_mat) # solve.QP
      #dvec <- moments$mean # solve.QP
    }
  } else {
    ROI_objective <- Q_objective(Q=2*lambda*moments$var, L=-moments$mean) # ROI
    #Dmat <- 2 * lambda * moments$var # solve.QP
    #dvec <- moments$mean # solve.QP
  }
  # set up the optimization problem and solve
  opt.prob <- OP(objective=ROI_objective, 
                       constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec))
  result <- ROI_solve(x=opt.prob, solver=solver)
  
  # result <- try(solve.QP(Dmat=Dmat, dvec=dvec, Amat=t(Amat), bvec=rhs.vec, meq=meq), silent=TRUE)
  if(inherits(x=result, "try-error")) stop(paste("No solution found:", result))
  
  weights <- result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  # out$out <- result$objval # ROI
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return LP Function #####
#' Maximum Return LP Optimization
#' 
#' This function is called by optimize.portfolio to solve maximum return
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param solver solver to use
#' @author Ross Bennett
maxret_opt <- function(R, moments, constraints, target, solver="glpk"){
  stopifnot("package:ROI" %in% search() || require("ROI",quietly = TRUE))
  stopifnot("package:ROI.plugin.glpk" %in% search() || require("ROI.plugin.glpk",quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  N <- ncol(R)
  # Applying box constraints
  # maxret_opt needs non infinite values for upper and lower bounds
  lb <- constraints$min
  ub <- constraints$max
  if(any(is.infinite(lb)) | any(is.infinite(ub))){
    warning("Inf or -Inf values detected in box constraints, maximum return 
            objectives must have finite box constraint values.")
    ub[is.infinite(ub)] <- max(abs(c(constraints$min_sum, constraints$max_sum)))
    lb[is.infinite(lb)] <- 0
  }
  bnds <- V_bound(li=seq.int(1L, N), lb=as.numeric(lb),
                  ui=seq.int(1L, N), ub=as.numeric(ub))
  
  # set up initial A matrix for leverage constraints
  Amat <- rbind(rep(1, N), rep(1, N))
  dir.vec <- c(">=","<=")
  rhs.vec <- c(constraints$min_sum, constraints$max_sum)
  
  # check for a target return
  if(!is.na(target)) {
    Amat <- rbind(Amat, moments$mean)
    dir.vec <- c(dir.vec, "==")
    rhs.vec <- c(rhs.vec, target)
  }
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, Amat.group, -Amat.group)
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir.vec, and rhs.vec
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, t.B, -t.B)
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }
  
  # set up the linear objective
  ROI_objective <- L_objective(L=-moments$mean)
  # objL <- -moments$mean
  
  # set up the optimization problem and solve
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                 bounds=bnds)
  roi.result <- ROI_solve(x=opt.prob, solver=solver)
  
  # roi.result <- Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=rhs.vec, bounds=bnds)
  
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(roi.result$status$code != 0) {
    message(roi.result$status$msg$message)
    stop("No solution")
    return(NULL)
  }
  
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  port.mean <- -roi.result$objval
  names(port.mean) <- "mean"
  obj_vals[["mean"]] <- port.mean
  out$obj_vals <- obj_vals
  # out$call <- call # need to get the call outside of the function
  return(out)
}

##### Maximize Return MILP Function #####
#' Maximum Return MILP Optimization
#' 
#' This function is called by optimize.portfolio to solve maximum return 
#' problems via mixed integer linear programming.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param solver solver to use
#' @author Ross Bennett
maxret_milp_opt <- function(R, constraints, moments, target, solver="glpk"){
  stopifnot("package:ROI" %in% search() || require("ROI",quietly = TRUE))
  stopifnot("package:ROI.plugin.glpk" %in% search() || require("ROI.plugin.glpk",quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of assets
  N <- ncol(R)
  
  # Maximum number of positions (non-zero weights)
  max_pos <- constraints$max_pos
  min_pos <- 1
  
  # Upper and lower bounds on weights
  LB <- as.numeric(constraints$min)
  UB <- as.numeric(constraints$max)
  
  # Check for target return
  if(!is.na(target)){
    # We have a target
    targetcon <- rbind(c(moments$mean, rep(0, N)),
                       c(-moments$mean, rep(0, N)))
    targetdir <- c("<=", "==")
    targetrhs <- c(Inf, -target)
  } else {
    # No target specified, just maximize
    targetcon <- NULL
    targetdir <- NULL
    targetrhs <- NULL
  }
  
  # weight_sum constraint
  Amat <- rbind(c(rep(1, N), rep(0, N)),
                c(rep(1, N), rep(0, N)))
  
  # Target return constraint
  Amat <- rbind(Amat, targetcon)
  
  # Bounds and position limit constraints
  Amat <- rbind(Amat, cbind(-diag(N), diag(LB)))
  Amat <- rbind(Amat, cbind(diag(N), -diag(UB)))
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N)))
  Amat <- rbind(Amat, c(rep(0, N), rep(1, N)))
  
  dir <- c("<=", ">=", targetdir, rep("<=", 2*N), "<=", "<=")
  rhs <- c(1, 1, targetrhs, rep(0, 2*N), -min_pos, max_pos)
  
  # Include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- constraints$groups[i] 
      Amat.group[i, k:(l+j)] <- 1
      k <- l + j + 1
      l <- k - 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(data=0, nrow=nrow(Amat.group), ncol=ncol(Amat.group))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Only seems to work if I do not specify bounds
  # bnds <- V_bound(li=seq.int(1L, 2*m), lb=c(as.numeric(constraints$min), rep(0, m)),
  #                 ui=seq.int(1L, 2*m), ub=c(as.numeric(constraints$max), rep(1, m)))
  bnds <- NULL
  
  # Set up the types vector with continuous and binary variables
  types <- c(rep("C", N), rep("B", N))
  
  # Set up the linear objective to maximize mean return
  ROI_objective <- L_objective(L=c(-moments$mean, rep(0, N)))
  
  # Set up the optimization problem and solve
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=Amat, dir=dir, rhs=rhs),
                 bounds=bnds, types=types)
  roi.result <- try(ROI_solve(x=opt.prob, solver=solver), silent=TRUE)
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  # Weights
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  
  # The out object is returned
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  
  obj_vals <- list()
  port.mean <- -roi.result$objval
  names(port.mean) <- "mean"
  obj_vals[["mean"]] <- port.mean
  out$obj_vals <- obj_vals
  return(out)
}

##### Minimize ETL LP Function #####
#' Minimum ETL LP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum ETL problems.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @param solver solver to use
#' @author Ross Bennett
etl_opt <- function(R, constraints, moments, target, alpha, solver="glpk"){
  stopifnot("package:ROI" %in% search() || require("ROI",quietly = TRUE))
  stopifnot("package:ROI.plugin.glpk" %in% search() || require("ROI.plugin.glpk",quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  N <- ncol(R)
  T <- nrow(R)
  # Applying box constraints
  LB <- c(as.numeric(constraints$min), rep(0, T), -1)
  UB <- c(as.numeric(constraints$max), rep(Inf, T), 1)
  bnds <- V_bound(li=seq.int(1L, N+T+1), lb=LB,
                  ui=seq.int(1L, N+T+1), ub=UB)
  
  # Add this check if mean is not an objective and return is a constraints
  if(!is.na(target)){
    if(all(moments$mean == 0)){
      moments$mean <- colMeans(R)
    }
  } else {
    moments$mean <- rep(0, N)
    target <- 0
  }
  
  Amat <- cbind(rbind(1, 1, moments$mean, coredata(R)), rbind(0, 0, 0, cbind(diag(T), 1))) 
  dir.vec <- c(">=","<=",">=",rep(">=",T))
  rhs.vec <- c(constraints$min_sum, constraints$max_sum, target ,rep(0, T))
  
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(0, nrow=n.groups, ncol=(T+1))
    Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
    rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
  }
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=(T+1))
    Amat <- rbind(Amat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir.vec <- c(dir.vec, rep(">=", 2 * nrow(t.B)))
    rhs.vec <- c(rhs.vec, constraints$lower, -constraints$upper)
  }
  
  ROI_objective <- L_objective(c(rep(0,N), rep(1/(alpha*T),T), 1))
  opt.prob <- OP(objective=ROI_objective, 
                       constraints=L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                       bounds=bnds)
  roi.result <- try(ROI_solve(x=opt.prob, solver=solver), silent=TRUE)
  if(inherits(x=roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  weights <- roi.result$solution[1:N]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  es_names <- c("ES", "ETL", "CVaR")
  es_idx <- which(es_names %in% names(moments))
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user. This will avoid
  # the extra call to constrained_objective
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  } else {
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### Minimize ETL MILP Function #####
#' Minimum ETL MILP Optimization
#' 
#' This function is called by optimize.portfolio to solve minimum ETL problems 
#' via mixed integer linear programming.
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param target target return value
#' @param alpha alpha value for ETL/ES/CVaR
#' @param solver solver to use
#' @author Ross Bennett
etl_milp_opt <- function(R, constraints, moments, target, alpha, solver="glpk"){
  stopifnot("package:ROI" %in% search() || require("ROI",quietly = TRUE))
  stopifnot("package:ROI.plugin.glpk" %in% search() || require("ROI.plugin.glpk",quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Number of rows
  n <- nrow(R)
  
  # Number of columns
  m <- ncol(R)
  
  max_sum <- constraints$max_sum
  min_sum <- constraints$min_sum
  LB <- constraints$min
  UB <- constraints$max
  max_pos <- constraints$max_pos
  min_pos <- 1
  moments_mean <- as.numeric(moments$mean)
  
  # A benchmark can be specified in the parma package. 
  # Leave this in and set to 0 for now
  benchmark <- 0
  
  # Check for target return
  if(!is.na(target)){
    # We have a target
    targetcon <- c(moments_mean, rep(0, n+2))
    targetdir <- "=="
    targetrhs <- target
  } else {
    # No target specified, just maximize
    targetcon <- NULL
    targetdir <- NULL
    targetrhs <- NULL
  }
  
  # Set up initial A matrix
  tmpAmat <- cbind(-coredata(R),
                   matrix(-1, nrow=n, ncol=1), 
                   -diag(n),
                   matrix(benchmark, nrow=n, ncol=1))
  
  # Add leverage constraints to matrix
  tmpAmat <- rbind(tmpAmat, rbind(c(rep(1, m), rep(0, n+2)),
                                  c(rep(1, m), rep(0, n+2))))
  
  # Add target return to matrix
  tmpAmat <- rbind(tmpAmat, as.numeric(targetcon))
  
  # This step just adds m rows to the matrix to accept box constraints in the next step
  tmpAmat <- cbind(tmpAmat, matrix(0, ncol=m, nrow=dim(tmpAmat)[1]))
  
  # Add lower bound box constraints
  tmpAmat <- rbind(tmpAmat, cbind(-diag(m), matrix(0, ncol=n+2, nrow=m), diag(LB)))
  
  # Add upper bound box constraints
  tmpAmat <- rbind(tmpAmat, cbind(diag(m), matrix(0, ncol=n+2, nrow=m), diag(-UB)))
  
  # Add rows cardinality constraints
  tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(-1, ncol=m, nrow=1))) 
  tmpAmat <- rbind(tmpAmat, cbind(matrix(0, ncol=m + n + 2, nrow=1), matrix(1, ncol=m, nrow=1)))
  
  # Set up the rhs vector
  rhs <- c( rep(0, n), min_sum, max_sum, targetrhs, rep(0, 2*m), -min_pos, max_pos)
  
  # Set up the dir vector
  dir <- c( rep("<=", n), ">=", "<=", targetdir, rep("<=", 2*m), "<=", "<=")
  
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=m)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    zeros <- matrix(0, nrow=n.groups, ncol=(m + n + 2))
    tmpAmat <- rbind(tmpAmat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(data=0, nrow=nrow(t.B), ncol=(m + n + 2))
    tmpAmat <- rbind(tmpAmat, cbind(t.B, zeros), cbind(-t.B, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Linear objective vector
  ROI_objective <- L_objective(c( rep(0, m), 1, rep(1/n, n) / alpha, 0, rep(0, m)))
  
  # Set up the types vector with continuous and binary variables
  types <- c( rep("C", m), "C", rep("C", n), "C", rep("B", m))
  
  bnds <- V_bound( li = 1L:(m + n + 2 + m), lb = c(LB, -1, rep(0, n), 1, rep(0, m)),
                   ui = 1L:(m + n + 2 + m), ub = c(UB, 1, rep(Inf, n), 1, rep(1, m)))
  
  # Set up the optimization problem and solve  
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=tmpAmat, dir=dir, rhs=rhs),
                 bounds=bnds, types=types)
  roi.result <- ROI_solve(x=opt.prob, solver=solver)
  
  # The Rglpk solvers status returns an an integer with status information
  # about the solution returned: 0 if the optimal solution was found, a 
  #non-zero value otherwise.
  if(roi.result$status$code != 0) {
    message("Undefined Solution")
    return(NULL)
  }
  
  weights <- roi.result$solution[1:m]
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  es_names <- c("ES", "ETL", "CVaR")
  es_idx <- which(es_names %in% names(moments))
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  } else {
    port.es <- roi.result$objval
    names(port.es) <- es_names[es_idx]
    obj_vals[[es_names[es_idx]]] <- port.es
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### minimize variance or maximize quadratic utility with turnover constraints #####
#' GMV/QU QP Optimization with Turnover Constraint
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems with turnover constraint
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param init_weights initial weights to compute turnover
#' @param solver solver to use
#' @author Ross Bennett
gmv_opt_toc <- function(R, constraints, moments, lambda, target, init_weights, solver="quadprog"){
  # function for minimum variance or max quadratic utility problems
  stopifnot("package:corpcor" %in% search() || require("corpcor",quietly = TRUE))
  stopifnot("package:ROI" %in% search() || require("ROI", quietly = TRUE))
  stopifnot("package:ROI.plugin.quadprog" %in% search() || require("ROI.plugin.quadprog", quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  R0 <- matrix(0, ncol=ncol(R), nrow=nrow(R))
  returns <- cbind(R, R0, R0)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
  } else {
    tmp_means <- rep(0, N)
    target <- 0
  }
  Amat <- c(tmp_means, rep(0, 2*N))
  dir <- "=="
  rhs <- target
  meq <- N + 1
  
  # Amat for initial weights
  # Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
  Amat <- rbind(Amat, cbind(diag(N), -1*diag(N), diag(N)))
  rhs <- c(rhs, init_weights)
  dir <- c(dir, rep("==", N))
  
  # Amat for turnover constraints
  Amat <- rbind(Amat, c(rep(0, N), rep(-1, N), rep(-1, N)))
  rhs <- c(rhs, -constraints$turnover_target)
  dir <- c(dir, ">=")
  
  # Amat for positive weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=N), diag(N), matrix(0, nrow=N, ncol=N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for negative weights
  Amat <- rbind(Amat, cbind(matrix(0, nrow=N, ncol=2*N), diag(N)))
  rhs <- c(rhs, rep(0, N))
  dir <- c(dir, rep(">=", N))
  
  # Amat for full investment constraint
  Amat <- rbind(Amat, rbind(c(rep(1, N), rep(0,2*N)), c(rep(-1, N), rep(0,2*N))))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), diag(0, N), diag(0, N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    zeros <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, zeros, zeros))
    Amat <- rbind(Amat, cbind(-Amat.group, zeros, zeros))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    zeros <- matrix(0, nrow=nrow(t.B), ncol=ncol(t.B))
    Amat <- rbind(Amat, cbind(t.B, zeros, zeros))
    Amat <- rbind(Amat, cbind(-t.B, zeros, zeros))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  # Remove the rows of Amat and elements of rhs.vec where rhs is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs), ]
  rhs <- rhs[!is.infinite(rhs)]
  dir <- dir[!is.infinite(rhs)]
  
  ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), 
                               L=rep(-tmp_means, 3))
  
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))
  
  roi.result <- try(ROI_solve(x=opt.prob, solver=solver), silent=TRUE)
  
  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  wts <- roi.result$solution
  wts.final <- wts[1:N]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  return(out)
}

##### minimize variance or maximize quadratic utility with proportional transactioncosts constraints #####
#' GMV/QU QP Optimization with Proportional Transaction Cost Constraint
#' 
#' This function is called by optimize.portfolio to solve minimum variance or 
#' maximum quadratic utility problems with proportional transaction cost constraint
#' 
#' @param R xts object of asset returns
#' @param constraints object of constraints in the portfolio object extracted with \code{get_constraints}
#' @param moments object of moments computed based on objective functions
#' @param lambda risk_aversion parameter
#' @param target target return value
#' @param init_weights initial weights to compute turnover
#' @param solver solver to use
#' @author Ross Bennett
gmv_opt_ptc <- function(R, constraints, moments, lambda, target, init_weights, solver="quadprog"){
  # function for minimum variance or max quadratic utility problems
  # modifying ProportionalCostOpt function from MPO package
  stopifnot("package:corpcor" %in% search() || require("corpcor", quietly = TRUE))
  stopifnot("package:ROI" %in% search() || require("ROI", quietly = TRUE))
  stopifnot("package:ROI.plugin.quadprog" %in% search() || require("ROI.plugin.quadprog", quietly = TRUE))
  
  # Check for cleaned returns in moments
  if(!is.null(moments$cleanR)) R <- moments$cleanR
  
  # Modify the returns matrix. This is done because there are 3 sets of
  # variables 1) w.initial, 2) w.buy, and 3) w.sell
  returns <- cbind(R, R, R)
  V <- cov(returns)
  
  # number of assets
  N <- ncol(R)
  
  # initial weights for solver
  if(is.null(init_weights)) init_weights <- rep(1/ N, N)
  
  # Amat for initial weights
  Amat <- cbind(diag(N), matrix(0, nrow=N, ncol=N*2))
  rhs <- init_weights
  dir <- rep("==", N)
  meq <- N
  
  # check for a target return constraint
  if(!is.na(target)) {
    # If var is the only objective specified, then moments$mean won't be calculated
    if(all(moments$mean==0)){
      tmp_means <- colMeans(R)
    } else {
      tmp_means <- moments$mean
    }
    Amat <- rbind(Amat, rep((1+tmp_means), 3))
    dir <- c(dir, "==")
    rhs <- c(rhs, (1+target))
    meq <- N + 1
  }
  
  # Amat for positive weights for w.buy and w.sell
  weights.positive <- rbind(matrix(0,ncol=2*N,nrow=N),diag(2*N))
  temp.index <- (N*3-N+1):(N*3)
  weights.positive[temp.index,] <- -1*weights.positive[temp.index,]
  Amat <- rbind(Amat, t(weights.positive))
  rhs <- c(rhs, rep(0, 2*N))
  
  # Amat for full investment constraint
  ptc <- constraints$ptc
  Amat <- rbind(Amat, rbind(c(rep(1, N), (1+ptc), (1-ptc)), -c(rep(1, N), (1+ptc), (1-ptc))))
  rhs <- c(rhs, constraints$min_sum, -constraints$max_sum)
  dir <- c(dir, ">=", ">=")
  
  # Amat for lower box constraints
  Amat <- rbind(Amat, cbind(diag(N), diag(N), diag(N)))
  rhs <- c(rhs, constraints$min)
  dir <- c(dir, rep(">=", N))
  
  # Amat for upper box constraints
  Amat <- rbind(Amat, cbind(-diag(N), -diag(N), -diag(N)))
  rhs <- c(rhs, -constraints$max)
  dir <- c(dir, rep(">=", N))
  
  # include group constraints
  if(try(!is.null(constraints$groups), silent=TRUE)){
    n.groups <- length(constraints$groups)
    Amat.group <- matrix(0, nrow=n.groups, ncol=N)
    for(i in 1:n.groups){
      Amat.group[i, constraints$groups[[i]]] <- 1
    }
    if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
    if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
    Amat <- rbind(Amat, cbind(Amat.group, Amat.group, Amat.group))
    Amat <- rbind(Amat, cbind(-Amat.group, -Amat.group, -Amat.group))
    dir <- c(dir, rep(">=", (n.groups + n.groups)))
    rhs <- c(rhs, constraints$cLO, -constraints$cUP)
  }
  
  # Add the factor exposures to Amat, dir, and rhs
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    Amat <- rbind(Amat, cbind(t.B, t.B, t.B))
    Amat <- rbind(Amat, cbind(-t.B, -t.B, -t.B))
    dir <- c(dir, rep(">=", 2 * nrow(t.B)))
    rhs <- c(rhs, constraints$lower, -constraints$upper)
  }
  
  d <- rep(-moments$mean, 3)
  
  # Remove the rows of Amat and elements of rhs.vec where rhs is Inf or -Inf
  Amat <- Amat[!is.infinite(rhs), ]
  rhs <- rhs[!is.infinite(rhs)]
  dir <- dir[!is.infinite(rhs)]
  
  ROI_objective <- Q_objective(Q=make.positive.definite(2*lambda*V), 
                               L=rep(-moments$mean, 3))
  
  opt.prob <- OP(objective=ROI_objective, 
                 constraints=L_constraint(L=Amat, dir=dir, rhs=rhs))
  roi.result <- try(ROI_solve(x=opt.prob, solver=solver), silent=TRUE)

  if(inherits(roi.result, "try-error")) stop(paste("No solution found:", roi.result))
  
  wts <- roi.result$solution
  w.buy <- roi.result$solution[(N+1):(2*N)]
  w.sell <- roi.result$solution[(2*N+1):(3*N)]
  w.total <- init_weights + w.buy + w.sell
  wts.final <- wts[(1:N)] + wts[(1+N):(2*N)] + wts[(2*N+1):(3*N)]
  
  weights <- wts.final
  names(weights) <- colnames(R)
  out <- list()
  out$weights <- weights
  out$out <- roi.result$objval
  obj_vals <- list()
  # Calculate the objective values here so that we can use the moments$mean
  # and moments$var that might be passed in by the user.
  if(!all(moments$mean == 0)){
    port.mean <- as.numeric(sum(weights * moments$mean))
    names(port.mean) <- "mean"
    obj_vals[["mean"]] <- port.mean
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  } else {
    port.sd <- as.numeric(sqrt(t(weights) %*% moments$var %*% weights))
    names(port.sd) <- "StdDev"
    obj_vals[["StdDev"]] <- port.sd
  }
  out$obj_vals <- obj_vals
  return(out)
}


mean_etl_opt <- function(R, constraints, moments, target, alpha, solver="glpk", tol=.Machine$double.eps^0.5, maxit=50){
  # This function returns the target mean return that maximizes mean / etl (i.e. starr)
  
  # if all(moments$mean == 0) then the user did not specify mean as an objective,
  # and we just want to return the target mean return value
  if(all(moments$mean == 0)) return(target)
  
  fmean <- matrix(moments$mean, ncol=1)
  
  # can't use optimize.portfolio here, this function is called inside 
  # optimize.portfolio and will throw an error message about nesting too deeply
  
  # Find the maximum return
  if(!is.null(constraints$max_pos)){
    max_ret <- maxret_milp_opt(R=R, constraints=constraints, moments=moments, target=NA, solver=solver)
  } else {
    max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, target=NA, solver=solver)
  }
  max_mean <- as.numeric(-max_ret$out)
  
  # Find the starr at the maximum etl portfolio
  if(!is.null(constraints$max_pos)){
    ub_etl <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=max_mean, alpha=alpha, solver=solver)
  } else {
    ub_etl <- etl_opt(R=R, constraints=constraints, moments=moments, target=max_mean, alpha=alpha, solver=solver)
  }
  ub_weights <- matrix(ub_etl$weights, ncol=1)
  ub_mean <- as.numeric(t(ub_weights) %*% fmean)
  ub_etl <- as.numeric(ub_etl$out)
  # starr at the upper bound
  ub_starr <- ub_mean / ub_etl
  if(is.infinite(ub_starr)) stop("Inf value for STARR, objective value is 0")
  
  # cat("ub_mean", ub_mean, "\n")
  # cat("ub_etl", ub_etl, "\n")
  # cat("ub_starr", ub_starr, "\n")
  
  # Find the starr at the minimum etl portfolio
  if(!is.null(constraints$max_pos)){
    lb_etl <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=NA, alpha=alpha, solver=solver)
  } else {
    lb_etl <- etl_opt(R=R, constraints=constraints, moments=moments, target=NA, alpha=alpha, solver=solver)
  }
  lb_weights <- matrix(lb_etl$weights)  
  lb_mean <- as.numeric(t(lb_weights) %*% fmean)  
  lb_etl <- as.numeric(lb_etl$out)
  
  # starr at the lower bound
  lb_starr <- lb_mean / lb_etl
  # if(is.infinite(lb_starr)) stop("Inf value for STARR, objective value is 0")
  
  # set lb_starr equal to 0, should this be a negative number like -1e6?
  # the lb_* values will be 0 for a dollar-neutral strategy so we need to reset the values
  if(is.na(lb_starr) | is.infinite(lb_starr)) lb_starr <- 0
  
  # cat("lb_mean", lb_mean, "\n")
  # cat("lb_etl", lb_etl, "\n")
  # cat("lb_starr", lb_starr, "\n")
  
  # want to find the return that maximizes mean / etl
  i <- 1
  while((abs(ub_starr - lb_starr) > tol) & (i < maxit)){
    # bisection method to find the maximum mean / etl
    
    # print(i)
    # cat("ub_starr", ub_starr, "\n")
    # cat("lb_starr", lb_starr, "\n")
    # print("**********")
    # Find the starr at the mean return midpoint
    new_ret <- (lb_mean + ub_mean) / 2
    if(!is.null(constraints$max_pos)){
      mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
    } else {
      mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
    }
    # print(mid)
    mid_weights <- matrix(mid$weights, ncol=1)
    mid_mean <- as.numeric(t(mid_weights) %*% fmean)
    mid_etl <- as.numeric(mid$out)
    mid_starr <- mid_mean / mid_etl
    # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
    # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
    # tmp_starr <- mid_starr
    
    # cat("mid_mean", mid_mean, "\n")
    # cat("mid_etl", mid_etl, "\n")
    # cat("mid_starr", mid_starr, "\n")
    
    if(mid_starr > ub_starr){
      # if mid_starr > ub_starr then mid_starr becomes the new upper bound
      ub_mean <- mid_mean
      ub_starr <- mid_starr
      new_ret <- (lb_mean + ub_mean) / 2
      if(!is.null(constraints$max_pos)){
        mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
      } else {
        mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
      }
      mid_weights <- matrix(mid$weights, ncol=1)
      mid_mean <- as.numeric(t(mid_weights) %*% fmean)
      mid_etl <- as.numeric(mid$out)
      mid_starr <- mid_mean / mid_etl
      # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
      # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
    } else if(mid_starr > lb_starr){
      # if mid_starr > lb_starr then mid_starr becomes the new lower bound
      lb_mean <- mid_mean
      lb_starr <- mid_starr
      new_ret <- (lb_mean + ub_mean) / 2
      if(!is.null(constraints$max_pos)){
        mid <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
      } else {
        mid <- etl_opt(R=R, constraints=constraints, moments=moments, target=new_ret, alpha=alpha, solver=solver)
      }
      mid_weights <- matrix(mid$weights, ncol=1)
      mid_mean <- as.numeric(t(mid_weights) %*% fmean)
      mid_etl <- as.numeric(mid$out)
      mid_starr <- mid_mean / mid_etl
      # the mid_* values MIGHT be 0 for a dollar-neutral strategy so we need to reset the values
      # if(is.na(mid_starr) | is.infinite(mid_starr)) mid_starr <- 0
    }
    i <- i + 1
  }
  return(new_ret)
}

max_sr_opt <- function(R, constraints, moments, lambda, target, lambda_hhi, conc_groups, solver="quadprog", tol=.Machine$double.eps^0.5, maxit=50){
  # This function returns the target mean return that maximizes mean / sd (i.e. sharpe ratio)
  
  # get the forecast mean from moments
  fmean <- matrix(moments$mean, ncol=1)
  
  # Find the maximum return
  max_ret <- maxret_opt(R=R, moments=moments, constraints=constraints, target=NA)
  max_mean <- as.numeric(-max_ret$out)
  
  # Calculate the sr at the maximum mean return portfolio
  ub_weights <- matrix(max_ret$weights, ncol=1)
  ub_mean <- max_mean
  ub_sd <- as.numeric(sqrt(t(ub_weights) %*% moments$var %*% ub_weights))
  # sr at the upper bound
  ub_sr <- ub_mean / ub_sd
  
  # Calculate the sr at the miminum var portfolio
  tmpmoments <- moments
  tmpmoments$mean <- rep(0, length(moments$mean))
  lb_sr <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=NA, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
  lb_weights <- matrix(lb_sr$weights)
  lb_mean <- as.numeric(t(lb_weights) %*% fmean)
  lb_sd <- as.numeric(sqrt(t(lb_weights) %*% moments$var %*% lb_weights))
  # sr at the lower bound
  lb_sr <- lb_mean / lb_sd
  
  # cat("lb_mean:", lb_mean, "\n")
  # cat("ub_mean:", ub_mean, "\n")
  # print("**********")
  
  # want to find the return that maximizes mean / sd
  i <- 1
  while((abs(ub_sr - lb_sr) > tol) & (i < maxit)){
    # bisection method to find the maximum mean / sd
    
    # Find the starr at the mean return midpoint
    new_ret <- (lb_mean + ub_mean) / 2
    mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
    mid_weights <- matrix(mid$weights, ncol=1)
    mid_mean <- as.numeric(t(mid_weights) %*% fmean)
    mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
    mid_sr <- mid_mean / mid_sd
    # tmp_sr <- mid_sr
    
    # print(i)
    # cat("new_ret:", new_ret, "\n")
    # cat("mid_sr:", mid_sr, "\n")
    # print("**********")
    
    if(mid_sr > ub_sr){
      # if mid_sr > ub_sr then mid_sr becomes the new upper bound
      ub_mean <- mid_mean
      ub_sr <- mid_sr
      new_ret <- (lb_mean + ub_mean) / 2
      mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
      mid_weights <- matrix(mid$weights, ncol=1)
      mid_mean <- as.numeric(t(mid_weights) %*% fmean)
      mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
      mid_sr <- mid_mean / mid_sd
    } else if(mid_sr > lb_sr){
      # if mid_sr > lb_sr then mid_sr becomes the new lower bound
      lb_mean <- mid_mean
      lb_sr <- mid_sr
      new_ret <- (lb_mean + ub_mean) / 2
      mid <- gmv_opt(R=R, constraints=constraints, moments=tmpmoments, lambda=1, target=new_ret, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver)
      mid_weights <- matrix(mid$weights, ncol=1)
      mid_mean <- as.numeric(t(mid_weights) %*% fmean)
      mid_sd <- as.numeric(sqrt(t(mid_weights) %*% moments$var %*% mid_weights))
      mid_sr <- mid_mean / mid_sd
    }
    i <- i + 1
  }
  return(new_ret)
}


#' Combine objects created by optimize.portfolio
#' 
#' This function takes a list of objects created by \code{\link{optimize.portfolio}}
#' and sets the class name attribute to 'opt.list' for use in generic functions
#' 
#' @param x a list of objects created by \code{\link{optimize.portfolio}}
#' @return an \code{opt.list} object
#' @export
combine.optimizations <- function(x){
  if(!is.list(x)) stop("x must be passed in as a list")
  for(i in 1:length(x)){
    if(!inherits(x[[i]], "optimize.portfolio")) stop("All objects in x must be of class 'optimize.portfolio'")
  }
  class(x) <- "opt.list"
  return(x)
}

#' Combine objects created by portfolio
#' 
#' This function takes a list of objects created by \code{\link{portfolio.spec}}
#' and sets the class name attribute to 'portfolio.list' for use in generic functions
#' 
#' @param x a list of objects created by \code{\link{portfolio.spec}}
#' @return a \code{portfolio.list} object
#' @export
portfolios.combine <- function(x){
  if(!is.list(x)) stop("x must be passed in as a list")
  for(i in 1:length(x)){
    if(!inherits(x[[i]], "portfolio")) stop("All objects in x must be of class 'portfolio'")
  }
  class(x) <- "portfolio.list"
  return(x)
}

modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # modify.args function from quantstrat
  
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals
  if(!length(arglist))
    return(formals)
  
  argnames <- names(arglist)
  if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
    stop("'arglist' must be a *named* list, with no names == \"\"")
  
  .formals  <- formals
  onames <- names(.formals)
  
  pm <- pmatch(argnames, onames, nomatch = 0L)
  #if(any(pm == 0L))
  #    message(paste("some arguments stored for", fun, "do not match"))
  names(arglist[pm > 0L]) <- onames[pm]
  .formals[pm] <- arglist[pm > 0L]
  
  # include all elements from arglist if function formals contain '...'
  if(dots && !is.null(.formals$...)) {
    dotnames <- names(arglist[pm == 0L])
    .formals[dotnames] <- arglist[dotnames]
    #.formals$... <- NULL  # should we assume we matched them all?
  }
  .formals
}

# This is how it is used in quantstrat in applyIndicators()
# # replace default function arguments with indicator$arguments
# .formals <- formals(indicator$name)
# .formals <- modify.args(.formals, indicator$arguments, dots=TRUE)
# # now add arguments from parameters
# .formals <- modify.args(.formals, parameters, dots=TRUE)
# # now add dots
# .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
# # remove ... to avoid matching multiple args
# .formals$`...` <- NULL
# 
# tmp_val <- do.call(indicator$name, .formals)


###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
