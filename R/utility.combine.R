

#' Combine objects created by optimize.portfolio
#' 
#' This function takes a list of objects created by \code{\link{optimize.portfolio}}
#' and sets the class name attribute to 'opt.list' for use in generic functions
#' 
#' @param x a list of objects created by \code{\link{optimize.portfolio}}
#' @return an \code{opt.list} object
#' @export
combine.optimizations <- function(x){
  if(!is.list(x)) stop("x must be passed in as a list")
  for(i in 1:length(x)){
    if(!inherits(x[[i]], "optimize.portfolio")) stop("All objects in x must be of class 'optimize.portfolio'")
  }
  class(x) <- "opt.list"
  return(x)
}

#' Combine objects created by portfolio
#' 
#' This function takes a list of objects created by \code{\link{portfolio.spec}}
#' and sets the class name attribute to 'portfolio.list' for use in generic functions
#' 
#' @param x a list of objects created by \code{\link{portfolio.spec}}
#' @return a \code{portfolio.list} object
#' @export
portfolios.combine <- function(x){
  if(!is.list(x)) stop("x must be passed in as a list")
  for(i in 1:length(x)){
    if(!inherits(x[[i]], "portfolio")) stop("All objects in x must be of class 'portfolio'")
  }
  class(x) <- "portfolio.list"
  return(x)
}


###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
