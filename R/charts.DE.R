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


chart.Weights.DE <- function(object, ..., neighbors = NULL, main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8, colorset=NULL, legend.loc="topright", cex.legend=0.8, plot.type="line"){
  # Specific to the output of optimize.portfolio with optimize_method="DEoptim"
  if(!inherits(object, "optimize.portfolio.DEoptim")) stop("object must be of class 'optimize.portfolio.DEoptim'")
  
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
#' @method chart.Weights optimize.portfolio.DEoptim
#' @S3method chart.Weights optimize.portfolio.DEoptim
chart.Weights.optimize.portfolio.DEoptim <- chart.Weights.DE


chart.Scatter.DE <- function(object, ..., neighbors = NULL, return.col='mean', risk.col='ES', chart.assets=FALSE, element.color = "darkgray", cex.axis=0.8, xlim=NULL, ylim=NULL){
  # more or less specific to the output of the DEoptim portfolio code with constraints
  # will work to a point with other functions, such as optimize.porfolio.parallel
  # there's still a lot to do to improve this.
  
  if(!inherits(object, "optimize.portfolio.DEoptim")) stop("object must be of class 'optimize.portfolio.DEoptim'")
  
  R <- object$R
  if(is.null(R)) stop("Returns object not detected, must run optimize.portfolio with trace=TRUE")
  portfolio <- object$portfolio
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
  # print(colnames(head(xtract)))
  
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
  
  # plot the portfolios from DEoptim_objective_results
  plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="darkgray", axes=FALSE, xlim=xlim, ylim=ylim, ...)
  
  # plot the risk-reward scatter of the assets
  if(chart.assets){
    points(x=asset_risk, y=asset_ret)
    text(x=asset_risk, y=asset_ret, labels=colnames(R), pos=4, cex=0.8)
  }
  
  if(!is.null(neighbors)){ 
    if(is.vector(neighbors)){
      if(length(neighbors)==1){
        # overplot nearby portfolios defined by 'out'
        orderx = order(xtract[,"out"]) #TODO this won't work if the objective is anything other than mean
        subsetx = head(xtract[orderx,], n=neighbors)
      } else{
        # assume we have a vector of portfolio numbers
        subsetx = xtract[neighbors,]
      }
      points(subsetx[,risk.column], subsetx[,return.column], col="lightblue", pch=1)
    }
    if(is.matrix(neighbors) | is.data.frame(neighbors)){
      # the user has likely passed in a matrix containing calculated values for risk.col and return.col      
      rtc = pmatch(return.col,columnnames)
      if(is.na(rtc)) {
        rtc = pmatch(paste(return.col,return.col,sep='.'),columnnames)
      }
      rsc = pmatch(risk.col,columnnames)
      if(is.na(rsc)) {
        risk.column = pmatch(paste(risk.col,risk.col,sep='.'),columnnames)
      }
      for(i in 1:nrow(neighbors)) points(neighbors[i,rsc], neighbors[i,rtc], col="lightblue", pch=1)
    }
  }
  
  #     points(xtract[1,risk.column],xtract[1,return.column], col="orange", pch=16) # overplot the equal weighted (or seed)
  #check to see if portfolio 1 is EW  object$random_portoflios[1,] all weights should be the same
  #     if(!isTRUE(all.equal(object$random_portfolios[1,][1],1/length(object$random_portfolios[1,]),check.attributes=FALSE))){
  #show both the seed and EW if they are different 
  #NOTE the all.equal comparison could fail above if the first element of the first portfolio is the same as the EW weight, 
  #but the rest is not, shouldn't happen often with real portfolios, only toy examples
  #         points(xtract[2,risk.column],xtract[2,return.column], col="green", pch=16) # overplot the equal weighted (or seed)
  #     }
  
  ## Draw solution trajectory
  if(!is.null(R) & !is.null(portfolio)){
    w.traj = unique(object$DEoutput$member$bestmemit)
    rows = nrow(w.traj)
    # Only attempt to draw trajectory if rows is greater than or equal to 1
    # There may be some corner cases where nrow(w.traj) is equal to 0, 
    # resulting in a 'subscript out of bounds' error.
    if(rows >= 2){
      rr = matrix(nrow=rows, ncol=2)
      ## maybe rewrite as an apply statement by row on w.traj
      rtc = NULL
      rsc = NULL
      trajnames = NULL
      for(i in 1:rows){
        
        w = w.traj[i,]
        x = unlist(constrained_objective(w=w, R=R, portfolio=portfolio, trace=TRUE))
        names(x)<-name.replace(names(x))
        if(is.null(trajnames)) trajnames<-names(x)
        if(is.null(rsc)){
          rtc = pmatch(return.col,trajnames)
          if(is.na(rtc)) {
            rtc = pmatch(paste(return.col,return.col,sep='.'),trajnames)
          }
          rsc = pmatch(risk.col,trajnames)
          if(is.na(rsc)) {
            rsc = pmatch(paste(risk.col,risk.col,sep='.'),trajnames)
          }
        }
        rr[i,1] = x[rsc] #'FIXME
        rr[i,2] = x[rtc]  #'FIXME      
      }
      colors2 = colorRamp(c("blue","lightblue"))
      colortrail = rgb(colors2((0:rows)/rows),maxColorValue=255)
      for(i in 1:rows){
        points(rr[i,1], rr[i,2], pch=1, col = colortrail[rows-i+1])
      }
      
      for(i in 2:rows){
        segments(rr[i,1], rr[i,2], rr[i-1,1], rr[i-1,2],col = colortrail[rows-i+1], lty = 1, lwd = 2)
      }
    }
  } else{
    message("Trajectory cannot be drawn because return object or constraints were not passed.")
  }
  
  
  ## @TODO: Generalize this to find column containing the "risk" metric
  if(length(names(object)[which(names(object)=='constrained_objective')])) {
    result.slot<-'constrained_objective'
  } else {
    result.slot<-'objective_measures'
  }
  objcols<-unlist(object[[result.slot]])
  names(objcols)<-name.replace(names(objcols))
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
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.RiskReward
#' @method chart.RiskReward optimize.portfolio.DEoptim
#' @S3method chart.RiskReward optimize.portfolio.DEoptim
chart.RiskReward.optimize.portfolio.DEoptim <- chart.Scatter.DE


charts.DE <- function(DE, risk.col, return.col, chart.assets, neighbors=NULL, main="DEoptim.Portfolios", xlim=NULL, ylim=NULL, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that DE is of the correct class
    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2)),heights=c(2,1.5),widths=1)
    par(mar=c(4,4,4,2))
    chart.Scatter.DE(object=DE, risk.col=risk.col, return.col=return.col, chart.assets=chart.assets, neighbors=neighbors, main=main, xlim=xlim, ylim=ylim, ...)
    par(mar=c(2,4,0,2))
    chart.Weights.DE(object=DE, main="", neighbors=neighbors, ...)
    par(op)
}


#' plot method for objects of class \code{optimize.portfolio}
#' 
#' Scatter and weights chart for portfolio optimizations run with trace=TRUE
#' 
#' @details
#' \code{return.col} must be the name of a function used to compute the return metric on the random portfolio weights
#' \code{risk.col} must be the name of a function used to compute the risk metric on the random portfolio weights
#' 
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors.  This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' \code{risk.col},\code{return.col}, and weights columns all properly named.
#' 
#' The ROI and GenSA solvers do not store the portfolio weights like DEoptim or random
#' portfolios, random portfolios can be generated for the scatter plot with the
#' \code{rp} argument. 
#' 
#' @param x set of portfolios created by \code{\link{optimize.portfolio}}
#' @param \dots any other passthru parameters
#' @param rp TRUE/FALSE to plot feasible portfolios generated by \code{\link{random_portfolios}}
#' @param return.col string name of column to use for returns (vertical axis)
#' @param risk.col string name of column to use for risk (horizontal axis)
#' @param chart.assets TRUE/FALSE to include risk-return scatter of assets
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @param xlim set the limit on coordinates for the x-axis
#' @param ylim set the limit on coordinates for the y-axis
#' @param element.color provides the color for drawing less-important chart elements, such as the box lines, axis lines, etc.
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @rdname plot
#' @method plot optimize.portfolio.DEoptim
#' @S3method plot optimize.portfolio.DEoptim
plot.optimize.portfolio.DEoptim <- function(x, ..., return.col='mean', risk.col='ES',  chart.assets=FALSE, neighbors=NULL, main='optimized portfolio plot', xlim=NULL, ylim=NULL) {
    charts.DE(DE=x, risk.col=risk.col, return.col=return.col, chart.assets=chart.assets, neighbors=neighbors, main=main, xlim=xlim, ylim=ylim, ...)
}
