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

#' Extract the efficient frontier of portfolios that meet your objectives over a range of risks
#' 
#' The efficient frontier is extracted from the set of portfolios created by 
#' \code{optimize.portfolio} with \code{trace=TRUE}.
#' 
#' If you do not have an optimal portfolio object created by 
#' \code{\link{optimize.portfolio}}, you can pass in a portfolio object and an
#' optimization will be run via \code{\link{optimize.portfolio}}
#' 
#' @note
#' Note that this function will be extremely sensitive to the objectives in your
#' \code{\link{portfolio}} object.  It will be especially obvious if you 
#' are looking at a risk budget objective and your return preference is not set high enough.
#' 
#'  
#' @param object optimial portfolio object as created by \code{\link{optimize.portfolio}}
#' @param from minimum value of the sequence
#' @param to maximum value of the sequence
#' @param by number to increment the sequence by
#' @param match.col string name of column to use for risk (horizontal axis)
#' @param \dots any other passthru parameters to \code{optimize.portfolio}
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization, see \code{\link{portfolio.spec}}
#' @param optimize_method one of "DEoptim", "random", "ROI", "pso", or "GenSA"
#' @export
extract.efficient.frontier <- function (object=NULL, match.col='ES', from=0, to=1, by=0.005, ..., R=NULL, portfolio=NULL, optimize_method='random')
{
    #TODO add a threshold argument for how close it has to be to count
    # do we need to recalc the constrained_objective too?  I don't think so.
    if(!inherits(object, "optimize.portfolio")) stop("object passed in must of of class 'portfolio'")
    
    set<-seq(from=from,to=to,by=by)
    set<-cbind(quantmod::Lag(set,1),as.matrix(set))[-1,]
    if(is.null(object)){
        if(!is.null(R) & !is.null(portfolio)){
            portfolios<-optimize.portfolio(portfolio=portfolio, R=R, optimize_method=optimize_method[1], trace=TRUE, ...)
        } else {
            stop('you must specify a portfolio object and a return series or an objective of class optimize.portfolio')
        }
    }
    
    xtract<-extractStats(object)
    columnnames=colnames(xtract)
    #if("package:multicore" %in% search() || require("multicore",quietly = TRUE)){
    #    mclapply
    #}
    stopifnot("package:foreach" %in% search() || require("foreach",quietly = TRUE))
#    rtc = pmatch(return.col,columnnames)
#    if(is.na(rtc)) {
#        rtc = pmatch(paste(return.col,return.col,sep='.'),columnnames)
#    }
    mtc = pmatch(match.col,columnnames)
    if(is.na(mtc)) {
        mtc = pmatch(paste(match.col,match.col,sep='.'),columnnames)
    }
    
    result <- foreach(i=1:nrow(set),.inorder=TRUE, .combine=rbind, .errorhandling='remove') %do% {
        tmp<-xtract[which(xtract[,mtc]>=set[i,1] & xtract[,mtc]<set[i,2]),]
        #tmp<-tmp[which.min(tmp[,'out']),]
        tmp<-tmp[which.max(tmp[,'mean']),]
        #tmp
    }
    return(result)
}

#' Generate the efficient frontier for a mean-variance portfolio
#' 
#' This function generates the mean-variance efficient frontier of a portfolio
#' specifying constraints and objectives. To generate the mean-var efficient 
#' frontier, the portfolio must have two objectives 1) "mean" and 2) "var".
#' 
#' @param portfolio a portfolio object with constraints and objectives created via \code{\link{portfolio.spec}}
#' @param R an xts or matrix of asset returns
#' @param n.portfolios number of portfolios to plot along the efficient frontier
#' @return a matrix of objective measure values and weights along the efficient frontier
#' @author Ross Bennett
#' @export
meanvar.efficient.frontier <- function(portfolio, R, n.portfolios=25){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  # step 1: find the minimum return given the constraints
  # step 2: find the maximum return given the constraints
  # step 3: 'step' along the returns and run the optimization to calculate
  # the weights and objective measures along the efficient frontier
  
  # for a mean-var efficient frontier, there must be two objectives 1) "mean" and 2) "var"
  # get the names of the objectives
  objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  if(!((length(objnames) == 2) & ("var" %in% objnames) & ("mean" %in% objnames))){
    stop("The portfolio object must have both 'mean' and 'var' specified as objectives")
  }
  # get the index number of the var objective 
  var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "var")
  # get the index number of the mean objective
  mean_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "mean")
  
  # set the risk_aversion to a very small number for equivalent to max return portfolio
  portfolio$objectives[[var_idx]]$risk_aversion <- 1e-6
  
  # run the optimization to get the maximum return
  tmp <- optimize.portfolio(R=ret, portfolio=portfolio, optimize_method="ROI")
  maxret <- extractObjectiveMeasures(tmp)$mean
  
  # set the risk_aversion to a very large number equivalent to a minvar portfolio
  portfolio$objectives[[var_idx]]$risk_aversion <- 1e6
  tmp <- optimize.portfolio(R=ret, portfolio=portfolio, optimize_method="ROI")
  stats <- extractStats(tmp)
  minret <- stats["mean"]
  
  # length.out is the number of portfolios to create
  ret_seq <- seq(from=minret, to=maxret, length.out=n.portfolios)
  
  out <- matrix(0, nrow=length(ret_seq), ncol=length(extractStats(tmp)))
  
  for(i in 1:length(ret_seq)){
    portfolio$objectives[[mean_idx]]$target <- ret_seq[i]
    out[i, ] <- extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI"))
  }
  colnames(out) <- names(stats)
  return(out)
}
