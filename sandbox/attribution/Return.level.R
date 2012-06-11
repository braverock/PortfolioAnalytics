#' aggregate portfolio to the given level
#' 
#' @aliases aggregate
#' 
#' Aggregates the portfoio up to the chosen level using returns, weights and
#' portfolio hierarchy (from the buildHierarchy function)
#'
#' @aliases aggregate
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights.
#' @param h  portfolio hierarchy returned by the buildHierarchy function
#' @param level aggregation level from the hierarchy
#' @author Andrii Babii
#' @seealso  \code{\link{buildHierarchy}}
#' @references
#' @export
#' @examples
#' 

Weight.transform <- 
function(wp, Rp)
{
    # Transform weights to the xts object used by aggregation and attribution functions
    if (is.vector(wp)){
        wp = as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), index(Rp))
        colnames(wp) = colnames(Rp)
    } else{
        if(as.Date(last(index(Rp))) < (as.Date(index(wp[1,]))+1)){
            stop(paste('last date in series',as.Date(last(index(Rp))),'occurs before beginning of first rebalancing period',as.Date(first(index(wp)))+1))
        }
        wp = checkData(wp, method = "xts")
        wp = merge(wp, xts(, index(Rp)))
        wp = na.locf(wp)
        if(as.Date(first(index(Rp))) > (as.Date(index(wp[1,]))+1)) {
            warning(paste('data series starts on',as.Date(first(index(Rp))),', which is after the first rebalancing period',as.Date(first(index(wp)))+1)) 
            wp = wp
        } else{
            wp = wp[2:nrow(wp)]
        }
    }
    return(wp)
}

Return.level <-
function(Rp, wp, h, level = "Sector")
{
    Rp = checkData(Rp, method = "xts")
    wp = Weight.transform(wp, Rp)
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
        Rp = Rp[2:nrow(Rp)]
    }

    # Aggregate returns to the chosen level from the hierarchy
    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(i in 1:length(h)){
        returns[, i] = rowSums(Rp[, h[[i]]] * wp[, h[[i]]])
    }
    colnames(returns) = names(h)
    return(returns)
}

Weight.level <-
function(wp, h, level = "Sector")
{   
    wp = Weight.transform(wp, Rp)

    h = split(h$primary_id, h[level])
    weights = wp[, 1:length(h)]
    for(i in 1:length(h)){
        weights[, i] = rowSums(wp[, h[[i]]])
    }
    colnames(weights) = names(h)
    return(weights)
}

# Example
# data(attrib) 
Weight.transform(wp, Rp)
Return.level(Rp, wp, h, level = "Sector")
Return.level(Rp, wp, h, level = "type")
Weight.level(wp, h, level = "Sector")

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R 1905 2012-04-21 19:23:13Z braverock $
#
###############################################################################