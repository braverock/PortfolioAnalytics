###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2010 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' print method for optimize.portfolio.rebalancing
#' @param x object of type optimize.portfolio.rebalancing
#' @param \dots any other passthru parameters
#' @export
print.optimize.portfolio.rebalancing <- function(x, ...) {
    if(!inherits(x,"optimize.portfolio.rebalancing")) 
        stop ("passed object is not of class optimize.portfolio.rebalancing")
    
    
    # loop through and show the results and weights
    cat('Weights:\n')
    for(i in 1:length(x)){
        cat(names(x[i]))
        cat('\n')
        print(round(x[[i]]$weights,4))
    }
    cat('Objective Measures\n')
    for(i in 1:length(x)){
        cat(names(x[i]))
        cat('\n')
        print(x[[i]]$objective_measures)
    }
    
}