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
