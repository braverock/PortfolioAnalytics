
#' Chart risk contribution or percent contribution
#' 
#' This function charts the contribution or percent contribution of the resulting
#' objective measures in \code{risk_budget_objectives}.
#' 
#' @param object optimal portfolio object created by \code{\link{optimize.portfolio}}
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
chart.RiskBudget <- function(object, ..., risk.type="absolute", main="Risk Contribution", ylab="", xlab=NULL, cex.axis=0.8, cex.lab=0.8, element.color="darkgray", las=3, ylim=NULL){
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
    for(i in 1:length(rb_idx)){
      if(is.null(ylim)){
        ylim <- range(contrib[[i]][[1]])
        ylim[1] <- min(0, ylim[1])
        ylim[2] <- ylim[2] * 1.15
      }
      
      # Plot values of contribution
      plot(contrib[[i]][[1]], type="b", axes=FALSE, xlab='', ylim=ylim, ylab=ylab, main=main, cex.lab=cex.lab, ...)
      axis(2, cex.axis = cex.axis, col = element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
      box(col = element.color)
    }
  }
  
  if(risk.type %in% c("percent", "percentage", "pct_contrib")){
    for(i in 1:length(rb_idx)){
      min_prisk <- portfolio$objectives[[rb_idx[i]]]$min_prisk
      max_prisk <- portfolio$objectives[[rb_idx[i]]]$max_prisk
      if(is.null(ylim)){
        ylim <- range(c(max_prisk, pct_contrib[[i]][[1]]))
        ylim[1] <- min(0, ylim[1])
        ylim[2] <- ylim[2] * 1.15
      }
      
      # plot percentage contribution
      plot(pct_contrib[[i]][[1]], type="b", axes=FALSE, xlab='', ylim=ylim, ylab=ylab, main=main, cex.lab=cex.lab, ...)
      # Check for minimum percentage risk (min_prisk) argument
      if(!is.null(min_prisk)){
        points(min_prisk, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
      }
      if(!is.null(max_prisk)){
        points(max_prisk, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
      }
      axis(2, cex.axis = cex.axis, col = element.color)
      axis(1, labels=columnnames, at=1:numassets, las=las, cex.axis = cex.axis, col = element.color)
      box(col = element.color)
    }
  }
}
