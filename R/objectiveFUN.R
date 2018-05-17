
#' Calculates turnover given two vectors of weights.
#' This is used as an objective function and is called when the user adds an objective of type turnover with \code{\link{add.objective}}
#' @param weights vector of weights from optimization
#' @param wts.init vector of initial weights used to calculate turnover from
#' @author Ross Bennett
#' @export
turnover <- function(weights, wts.init=NULL) {
  # turnover function from https://r-forge.r-project.org/scm/viewvc.php/pkg/PortfolioAnalytics/sandbox/script.workshop2012.R?view=markup&root=returnanalytics
  
  N <- length(weights)
  
  # If wts.init is not given, then assume a vector of equal weights
  if(is.null(wts.init)) {
    wts.init <- rep(1/N, N)
  }
  
  # Check that weights and wts.init are the same length
  if(length(weights) != length(wts.init)) stop("weights and wts.init are not the same length")
  
  return(sum(abs(wts.init - weights)) / N)
}

#' Calculate portfolio variance
#' 
#' This function is used to calculate the portfolio variance via a call to 
#' constrained_objective when var is an object for mean variance or quadratic 
#' utility optimization.
#' 
#' @param R xts object of asset returns
#' @param weights vector of asset weights
#' @return numeric value of the portfolio variance
#' @author Ross Bennett
#' @export
var.portfolio <- function(R, weights){
  weights <- matrix(weights, ncol=1)
  return(as.numeric(t(weights) %*% var(R) %*% weights))
}

#' Concentration of weights
#' 
#' This function computes the concentration of weights using the Herfindahl Hirschman Index
#' 
#' @param weights set of portfolio weights
#' @param groups list of vectors of grouping
#' @author Ross Bennett
#' @export
HHI <- function(weights, groups=NULL){
  
  # calculate overall HHI
  hhi <- sum(weights^2)
  
  # calculate group HHI
  if(!is.null(groups)){
    ngroups <- length(groups)
    group_hhi <- rep(0, ngroups)
    if(!is.null((names(groups)))) names(group_hhi) <- names(groups)
    for(i in 1:ngroups){
      group_hhi[i] <- sum(weights[groups[[i]]]^2)
    }
    return(list(HHI=hhi, Groups_HHI=group_hhi))
  } else {
    return(hhi)
  }
}

# portfolio mean return
port.mean <- function(weights, mu){
  # t(weights) %*% moments$mu
  as.numeric(crossprod(weights, mu))
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
