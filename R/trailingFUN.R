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

#' apply a function over a configurable trailing period
#' 
#' this function is primarily designed for use with portfolio functions passing 
#' 'x' or 'R' and weights, but may be usable for other things as well, see Example for a vector example.
#' 
#' called with e.g.
#' 
#' trailingFUN(seq(1:100), weights=NULL, n=12, FUN='mean',FUNargs=list())
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param weights a vector of weights to test
#' @param \dots any other passthru parameters 
#' @param n numeric number of trailing periods
#' @param FUN string describing the function to be called
#' @param FUNargs list describing any additional arguments
#' @export
trailingFUN <- function(R, weights, n=0, FUN, FUNargs=NULL, ...) {
    
    if (is.null(FUN)) {stop("you must supply a function to apply to")}
    
    funname<-FUN
    FUN<-match.fun(FUN)
    
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        # rm('...')
        nargs=NULL
    }
    
    if(!is.null(nrow(R))) R<-R[((nrow(R)-n)):nrow(R),]
    else R<-R[(length(R)-n):length(R)]
    
    
    if(is.function(FUN)){
        .formals  <- formals(FUN)
        onames <- names(.formals)
        if(is.list(FUNargs)){
            #TODO FIXME only do this if R and weights are in the argument list of the fn
            if(is.null(nargs$R) | !length(nargs$R)==length(R)) nargs$R <- R
            if(!is.null(nargs$x) | !length(nargs$x)==length(R)) nargs$x <- R
            
            if(is.null(nargs$weights)) nargs$weights <- weights
            
            pm <- pmatch(names(FUNargs), onames, nomatch = 0L)
            if (any(pm == 0L))
                warning(paste("some FUNargs stored for",funname,"do not match"))
            # this line overwrites the names of things stored in $FUNargs with names from formals.
            # I'm not sure it's a good idea, so commenting for now, until we prove we need it
            #names(FUNargs[pm > 0L]) <- onames[pm]
            .formals[pm] <- FUNargs[pm > 0L]
            #now add dots
            if (length(nargs)) {
                dargs<-nargs
                pm <- pmatch(names(dargs), onames, nomatch = 0L)
                names(dargs[pm > 0L]) <- onames[pm]
                .formals[pm] <- dargs[pm > 0L]
            }
            .formals$... <- NULL
        } else {
            warning('no FUNargs passed for function')
        }
    } else {
        stop('FUN must specify an R function')
    }
    
    tmp_measure = try((do.call(FUN,.formals)) ,silent=TRUE)
      
    if(inherits(tmp_measure,"try-error") | is.na(tmp_measure)) { 
        message(paste("trailing function generated an error or warning:",tmp_measure))
    } else {
        return(tmp_measure)
    }
}