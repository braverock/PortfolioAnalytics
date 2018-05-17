
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
#' or \code{\link{optimize.portfolio.rebalancing}}
#' @param \dots any other passthru parameters to \code{\link{plot}}
#' @param neighbors risk contribution or pct_contrib of neighbor portfolios to be plotted, see Details.
#' @param match.col string of risk column to match. The \code{opt.list} object 
#' may contain risk budgets for ES or StdDev and this will match the proper 
#' column names of the objectives list outp (e.g. ES.contribution).
#' @param risk.type "absolute" or "percentage" to plot risk contribution in absolute terms or percentage contribution.
#' @param regime integer of the regime number. For use with 
#' \code{\link{optimize.portfolio.rebalancing}} run with regime switching portfolios.
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
chart.RiskBudget.optimize.portfolio.rebalancing <- function(object, ..., match.col="ES", risk.type="absolute", regime=NULL, main="Risk Contribution"){
  
  # Get the objective measures at each rebalance period
  rebal.obj <- extractObjectiveMeasures(object)
  
  if(inherits(object$portfolio, "regime.portfolios")){
    # If the optimize.portfolio.rebalancing object is run with regime switching,
    # the output of extractObjectiveMeasures is a list of length N where each
    # element is the objective measures of the corresponding regime. (i.e.
    # rebal.obj[[1]] is the objective measures for portfolio 1 with regime 1)
    if(is.null(regime)) regime=1
    rebal.obj <- rebal.obj[[regime]]
  }
  
  if(risk.type == "absolute"){
    rbcols <- grep(paste(match.col, "contribution", sep="."), colnames(rebal.obj))
    if(length(rbcols) < 1) stop(paste("No ", match.col, ".contribution columns.", sep=""))
    rbdata <- rebal.obj[, rbcols]
    colnames(rbdata) <- gsub("^.*\\.", "", colnames(rbdata))
    chart.StackedBar(w=rbdata, ylab=paste(match.col, "Contribution", sep=" "), main=main, ...)
  }
  
  if(risk.type %in% c("percent", "percentage", "pct_contrib")){
    rbcols <- grep(paste(match.col, "pct_contrib", sep="."), colnames(rebal.obj))
    if(length(rbcols) < 1) stop(paste("No ", match.col, ".pct_contrib columns.", sep=""))
    rbdata <- rebal.obj[, rbcols]
    colnames(rbdata) <- gsub("^.*\\.", "", colnames(rbdata))
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
