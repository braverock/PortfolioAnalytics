

#' Create an equal weight portfolio
#' 
#' This function calculates objective measures for an equal weight portfolio.
#' 
#' @details
#' This function is simply a wrapper around \code{\link{constrained_objective}}
#' to calculate the objective measures in the given \code{portfolio} object of
#' an equal weight portfolio. The portfolio object should include all objectives
#' to be calculated.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param \dots any other passthru parameters to \code{constrained_objective}
#' @return a list containing the returns, weights, objective measures, call, and portfolio object
#' @author Ross Bennett
#' @export
equal.weight <- function(R, portfolio, ...){
  # Check for portfolio object passed in
  if(!is.portfolio(portfolio)) stop("portfolio object passed in must be of class 'portfolio'")

  max_sum <- get_constraints(portfolio)$max_sum
  
  # get asset information for equal weight portfolio
  assets <- portfolio$assets
  nassets <- length(assets)
  weights <- rep(max_sum / nassets, nassets)
  names(weights) <- names(assets)
  
  # make sure the number of columns in R matches the number of assets
  if(ncol(R) != nassets){
    if(ncol(R) > nassets){
      R <- R[, 1:nassets]
      warning("number of assets is less than number of columns in returns object, subsetting returns object.")
    } else {
      stop("number of assets is greater than number of columns in returns object")
    }
  }
  
  tmpout <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE, ...)
  return(structure(list(
    R=R,
    weights=weights,
    out=tmpout$out,
    objective_measures=tmpout$objective_measures,
    call=match.call(),
    portfolio=portfolio),
                   class=c("optimize.portfolio.eqwt", "optimize.portfolio"))
  )
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
