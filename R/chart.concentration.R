
# conc.type = weight or pct_contrib for risk budget optimization

#' Classic risk reward scatter and concentration
#' 
#' This function charts the \code{optimize.portfolio} object in risk-return space
#' and the degree of concentration based on the weights or percentage component
#' contribution to risk.
#' 
#' @param object optimal portfolio created by \code{\link{optimize.portfolio}}.
#' @param \dots any other passthru parameters.
#' @param return.col string matching the objective of a 'return' objective, on vertical axis.
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis.
#' @param chart.assets TRUE/FALSE. Includes a risk reward scatter of the assets in the chart.
#' @param conc.type concentration type can be based on the concentration of weights
#' or concentration of percentage component contribution to risk (only works with risk
#' budget objective for the optimization).
#' @param col color palette or vector of colors to use.
#' @param element.color color for the border and axes.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}.
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}.
#' @seealso \code{\link{optimize.portfolio}}
#' @author Peter Carl and Ross Bennett
#' @export
chart.Concentration <- function(object,
                                ...,
                                return.col='mean', 
                                risk.col='ES', 
                                chart.assets=FALSE, 
                                conc.type=c("weights", "pct_contrib"),
                                col=heat.colors(20),
                                element.color = "darkgray", 
                                cex.axis=0.8, 
                                xlim=NULL, ylim=NULL){
  # check the object
  if(!inherits(object, "optimize.portfolio")){
    stop("object must be of class 'optimize.portfolio'")
  }
  
  # extract the stats
  xtract <- try(extractStats(object), silent=TRUE)
  if(inherits(xtract, "try-error")) {
    message(xtract)
    return(NULL)
  }
  
  # get the concentration type
  # We can either chart the concentration of the weights or the concentration
  # of the percentage contribution to risk for risk budget optimizations
  conc.type <- match.arg(conc.type)
  
  columnnames <- colnames(xtract)
  R <- object$R
  
  # Get the return and risk columns from xtract
  return.column <- pmatch(return.col, columnnames)
  if(is.na(return.column)) {
    return.col <- paste(return.col, return.col, sep='.')
    return.column <- pmatch(return.col, columnnames)
  }
  risk.column <- pmatch(risk.col, columnnames)
  if(is.na(risk.column)) {
    risk.col <- paste(risk.col, risk.col, sep='.')
    risk.column <- pmatch(risk.col, columnnames)
  }
  
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
  
  if(conc.type == "weights"){
    idx <- grep("w.", colnames(xtract))
    if(length(idx) == 0) stop("weights not detected in output of extractStats")
    tmp.x <- xtract[, idx]
  } else if(conc.type == "pct_contrib"){
    idx <- grep("pct_contrib", colnames(xtract))
    if(length(idx) == 0) stop("pct_contrib not detected in output of extractStats")
    tmp.x <- xtract[, idx]
  }
  # need a check to make sure that tmp.x is valid
  
  # # Use HHI to compute the concentration of the pct_contrib_MES or concentration of weights
  x.hhi <- apply(tmp.x, MARGIN=1, FUN="HHI")
  # normalized HHI between 0 and 1
  y <- (x.hhi - min(x.hhi)) / (max(x.hhi) - min(x.hhi))
  
  op <- par(no.readonly=TRUE)
  layout(matrix(c(1,2)),heights=c(4,1.25),widths=1)
  par(mar=c(5,4,1,2)+.1, cex=1) # c(bottom, left, top, right)
  
  # plot the asset in risk-return space ordered based on degree of concentration
  plot(xtract[order(y, decreasing=TRUE), risk.column], xtract[order(y, decreasing=TRUE), return.column], xlab=risk.col, ylab=return.col, col=col, axes=FALSE, xlim=xlim, ylim=ylim, ...)
  
  # plot the risk-reward scatter of the assets
  if(chart.assets){
    points(x=asset_risk, y=asset_ret)
    text(x=asset_risk, y=asset_ret, labels=colnames(R), pos=4, cex=0.8)
  }
  
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
  
  # Now plot the portfolio concentration part
  # Add legend to bottom panel
  par(mar=c(5,5.5,1,3)+.1, cex=0.7)
  x <- x.hhi
  scale01 <- function(x, low = min(x), high = max(x)) {
    return((x - low) / (high - low))
  }
  
  breaks <- seq(min(x.hhi, na.rm=TRUE), max(x.hhi, na.rm=TRUE), length=(length(col)+1))
  min.raw <- min(x, na.rm = TRUE)
  max.raw <- max(x, na.rm = TRUE)
  z <- seq(min.raw, max.raw, length=length(col))
  image(z = matrix(z, ncol=1), col=col, breaks=breaks, xaxt="n", yaxt="n")
  par(usr=c(0, 1, 0, 1)) # needed to draw the histogram correctly
  lv <- pretty(breaks)
  xv <- scale01(as.numeric(lv), min.raw, max.raw)
  axis(1, at=xv, labels=sprintf("%s%%", pretty(lv)))
  h <- hist(x, plot=FALSE, breaks=breaks)
  hx <- scale01(breaks, min(x), max(x))
  hy <- c(h$counts, h$counts[length(h$counts)])
  lines(hx, hy / max(hy) * 0.95, lwd=2, type="s", col="blue")
  axis(2, at=pretty(hy) / max(hy) * 0.95, pretty(hy))
  title(ylab="Count")
  title(xlab="Degree of Concentration")
  par(op)
  invisible(NULL)
}
