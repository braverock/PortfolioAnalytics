
#' Chart risk contribution or percent contribution
#' 
#' This function charts the contribution or percent contribution of the resulting
#' objective measures in \code{risk_budget_objectives}.
#' 
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors. This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' properly named contribution and pct_contrib columns. 
#' 
#' @param object optimal portfolio object created by \code{\link{optimize.portfolio}}
#' @param neighbors risk contribution or pct_contrib of neighbor portfolios to be plotted
#' @param ... passthrough parameters to \code{\link{plot}}
#' @param risk.type plot risk contribution in absolute terms or percentage contribution
#' @param main main title for the chart
#' @param ylab label for the y-axis
#' @param xlab a title for the x axis: see \code{\link{title}}
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of \code{cex}
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param element.color color for the default plot lines
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis [\emph{default}],}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical.}
#'       }
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @author Ross Bennett
#' @export
chart.RiskBudget <- function(object, neighbors=NULL, ..., risk.type="absolute", main="Risk Contribution", ylab="", xlab=NULL, cex.axis=0.8, cex.lab=0.8, element.color="darkgray", las=3, ylim=NULL){
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
  
  for(i in 1:length(object$objective_measures)){
    if(length(object$objective_measures[[i]]) > 1){
      # we have an objective measure with contribution and pct_contrib
      contrib[[i]] <- object$objective_measures[[i]][2]
      pct_contrib[[i]] <- object$objective_measures[[i]][3]
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
    for(ii in 1:length(rb_idx)){
      if(is.null(ylim)){
        ylim <- range(contrib[[ii]][[1]])
        ylim[1] <- min(0, ylim[1])
        ylim[2] <- ylim[2] * 1.15
      }
      objname <- portfolio$objectives[[rb_idx[i]]]$name
      # Plot values of contribution
      plot(contrib[[ii]][[1]], type="n", axes=FALSE, xlab="", ylim=ylim, ylab=paste(objname, "Contribution", sep=" "), main=main, cex.lab=cex.lab, ...)
      
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
      points(contrib[[ii]][[1]], type="b", ...)
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
      plot(pct_contrib[[ii]][[1]], type="n", axes=FALSE, xlab='', ylim=ylim, ylab=paste(objname, " % Contribution", sep=" "), main=main, cex.lab=cex.lab, ...)
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
      points(pct_contrib[[ii]][[1]], type="b", ...)
      axis(2, cex.axis = cex.axis, col = element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
      box(col = element.color)
    } # end for loop of risk_budget_objective
  } # end plot for pct_contrib risk.type
  
  
}
