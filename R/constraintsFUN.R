
#' Function to compute diversification as a constraint
#' 
#' Diversification is defined as 1 minus the sum of the squared weights
#' \deqn{diversification = 1 - sum(w^2)}
#' 
#' @param weights vector of asset weights
#' @author Ross Bennett
#' @export
diversification <- function(weights){
   div <- 1 - sum(weights^2)
   return(div)
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
