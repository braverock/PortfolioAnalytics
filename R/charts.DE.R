###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2012 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' @rdname chart.Weights
#' @export
chart.Weights.DE <- function(object, neighbors = NULL, ..., main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8){
  # Specific to the output of optimize.portfolio with optimize_method="DEoptim"
  if(!inherits(object, "optimize.portfolio.DEoptim")) stop("object must be of class 'optimize.portfolio.DEoptim'")
  
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
chart.Weights.optimize.portfolio.DEoptim <- chart.Weights.DE

#' @rdname chart.RiskReward
#' @export
chart.Scatter.DE <- function(object, neighbors = NULL, ..., return.col='mean', risk.col='ES', element.color = "darkgray", cex.axis=0.8){
  # more or less specific to the output of the DEoptim portfolio code with constraints
  # will work to a point with other functions, such as optimize.porfolio.parallel
  # there's still a lot to do to improve this.
  
  if(!inherits(object, "optimize.portfolio.DEoptim")) stop("object must be of class 'optimize.portfolio.DEoptim'")
  
  R <- object$R
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
  
  plot(xtract[,risk.column],xtract[,return.column], xlab=risk.col, ylab=return.col, col="darkgray", axes=FALSE, ...)
  
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
    colortrail = rgb(colors2((0:rows)/rows),max=255)
    for(i in 1:rows){
      points(rr[i,1], rr[i,2], pch=1, col = colortrail[rows-i+1])
    }
    
    for(i in 2:rows){
      segments(rr[i,1], rr[i,2], rr[i-1,1], rr[i-1,2],col = colortrail[rows-i+1], lty = 1, lwd = 2)
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
  } else {
    points(objcols[risk.column], objcols[return.column], col="blue", pch=16) # optimal
  }
  axis(1, cex.axis = cex.axis, col = element.color)
  axis(2, cex.axis = cex.axis, col = element.color)
  box(col = element.color)
}

#' @rdname chart.RiskReward
#' @export
chart.RiskReward.optimize.portfolio.DEoptim <- chart.Scatter.DE

#' scatter and weights chart  for random portfolios
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
#' @param DE set of random portfolios created by \code{\link{optimize.portfolio}}
#' @param ... any other passthru parameters 
#' @param risk.col string name of column to use for risk (horizontal axis)
#' @param return.col string name of column to use for returns (vertical axis)
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @seealso 
#' \code{\link{optimize.portfolio}}
#' \code{\link{extractStats}}
#' @export
charts.DE <- function(DE, risk.col, return.col, neighbors=NULL, main="DEoptim.Portfolios", ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that DE is of the correct class
    op <- par(no.readonly=TRUE)
    layout(matrix(c(1,2)),height=c(2,1.5),width=1)
    par(mar=c(4,4,4,2))
    chart.Scatter.DE(object=DE, risk.col=risk.col, return.col=return.col, neighbors=neighbors, main=main, ...)
    par(mar=c(2,4,0,2))
    chart.Weights.DE(object=DE, main="", neighbors=neighbors, ...)
    par(op)
}

#TODO make chart.DE into a plot() method or methods

#' plot method for optimize.portfolio.DEoptim output
#' 
#' scatter and weights chart  for DEoptim portfolio optimizations run with trace=TRUE
#' 
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors.  This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' \code{risk.col},\code{return.col}, and weights columns all properly named.  
#' @param x set of portfolios created by \code{\link{optimize.portfolio}}
#' @param ... any other passthru parameters 
#' @param risk.col string name of column to use for risk (horizontal axis)
#' @param return.col string name of column to use for returns (vertical axis)
#' @param neighbors set of 'neighbor portfolios to overplot
#' @param main an overall title for the plot: see \code{\link{title}}
#' @export
plot.optimize.portfolio.DEoptim <- function(x, ..., return.col='mean', risk.col='ES',  neighbors=NULL, main='optimized portfolio plot') {
    charts.DE(DE=x, risk.col=risk.col, return.col=return.col, neighbors=neighbors, main=main, ...)
}
