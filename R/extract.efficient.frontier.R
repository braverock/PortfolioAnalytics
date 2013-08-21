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
