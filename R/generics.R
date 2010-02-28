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

#' summary method for optimize.portfolio.rebalancing
#' @param object object of type optimize.portfolio.rebalancing
#' @param \dots any other passthru parameters
#' @export
summary.optimize.portfolio.rebalancing <- function(object, ...) {
    if(!inherits(object,"optimize.portfolio.rebalancing")) 
        stop ("passed object is not of class optimize.portfolio.rebalancing")
    
    
    # loop through and show the results and weights
    cat('Weights:\n')
    for(i in 1:length(object)){
        cat(names(object[i]))
        cat('\n')
        if(!inherits(object[i],'try-error')){
            print(round(object[[i]]$weights,4))
        } else {
            print(object[i])
        }
    }
    cat('Objective Measures\n')
    for(i in 1:length(object)){
        if(!inherits(object[i],'try-error')){
            cat(names(object[i]))
            cat('\n')
            print(object[[i]]$constrained_objective)
        }
    }    
}