#' Apply a risk or return function to a set of weights
#' 
#' This function is used to calculate risk or return metrics given a matrix of
#' weights and is primarily used as a convenience function used in chart.Scatter functions
#' 
#' @param R xts object of asset returns
#' @param weights a matrix of weights generated from random_portfolios or \code{optimize.portfolio}
#' @param FUN name of a function
#' @param arguments named list of arguments to FUN
#' @author Ross Bennett
#' @export
applyFUN <- function(R, weights, FUN="mean", arguments){
  nargs <- arguments
  
  moments <- function(R){
    momentargs <- list()
    momentargs$mu <- matrix(as.vector(apply(R, 2, "mean")), ncol = 1)
    momentargs$sigma <- cov(R)
    momentargs$m3 <- PerformanceAnalytics::M3.MM(R)
    momentargs$m4 <- PerformanceAnalytics::M4.MM(R)
    return(momentargs)
  }
  
  nargs <- c(nargs, moments(R))
  nargs$R <- R
  #nargs$invert=FALSE
  
  # match the FUN arg to a risk or return function
  switch(FUN,
         mean = {
           fun = match.fun(mean)
         },
         sd =,
         StdDev = { 
           fun = match.fun(StdDev)
         },
         mVaR =,
         VaR = {
           fun = match.fun(VaR) 
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
         es =,
         mES =,
         CVaR =,
         cVaR =,
         ETL=,
         mETL=,
         ES = {
           fun = match.fun(ES)
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
{   # see 'S Programming p. 67 for this matching
  fun <- try(match.fun(FUN))
}
  ) # end switch block
  
  if(!is.null(nrow(weights))){
    # case for matrix of weights
    out <- rep(0, nrow(weights))
    .formals  <- formals(fun)
    onames <- names(.formals)
    for(i in 1:nrow(weights)){
      nargs$weights <- as.numeric(weights[i,])
      nargs$x <- R %*% as.numeric(weights[i,])
      dargs <- nargs
      pm <- pmatch(names(dargs), onames, nomatch = 0L)
      names(dargs[pm > 0L]) <- onames[pm]
      .formals[pm] <- dargs[pm > 0L]
      out[i] <- try(do.call(fun, .formals))
    }
  } else {
    # case for single vector of weights
    .formals  <- formals(fun)
    onames <- names(.formals)
    nargs$weights <- as.numeric(weights)
    nargs$x <- R %*% as.numeric(weights)
    dargs <- nargs
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
    out <- try(do.call(fun, .formals))
  }
     return(out)
}

#' Apply a risk or return function to asset returns
#' 
#' This function is used to calculate risk or return metrics given a matrix of
#' asset returns and will be used for a risk-reward scatter plot of the assets
#' 
#' @param R xts object of asset returns
#' @param FUN name of function
#' @param arguments named list of arguments to FUN
#' @author Ross Bennett
#' @export
scatterFUN <- function(R, FUN, arguments=NULL){
  if(is.null(arguments)){
    nargs <- list()
  } else{
    nargs <- arguments
  }
  
  # match the FUN arg to a risk or return function
  switch(FUN,
         mean = {
           return(as.numeric(apply(R, 2, mean)))
           #fun = match.fun(mean)
           #nargs$x = R
         },
         var = {
           return(as.numeric(apply(R, 2, var)))
           #fun = match.fun(mean)
           #nargs$x = R
         },
         sd =,
         StdDev = { 
           fun = match.fun(StdDev)
         },
         mVaR =,
         VaR = {
           fun = match.fun(VaR) 
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
         es =,
         mES =,
         CVaR =,
         cVaR =,
         ETL =,
         mETL =,
         ES = {
           fun = match.fun(ES)
           if(is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
           if(is.null(nargs$invert)) nargs$invert = FALSE
         },
{   # see 'S Programming p. 67 for this matching
  fun <- try(match.fun(FUN))
}
  ) # end switch block
  
  # calculate FUN on R
  out <- rep(0, ncol(R))
  .formals  <- formals(fun)
  onames <- names(.formals)
  for(i in 1:ncol(R)){
    nargs$R <- R[, i]
    dargs <- nargs
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
    out[i] <- try(do.call(fun, .formals))
  }
  return(out)
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

