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
#' TODO Replace example using portfolio dataset. Make rebalancing working 
#' correctly, starting from the next day as in the Return.rebalacing.
#' Fix bugs. Look at na.locf
#' @references
#' @export
#' @examples
#' 
Return.level <-
function(Rp, wp, h, level = "Sector")
{
    Rp = checkData(Rp, method = "xts")
    wp = Weight.transform(wp, Rp)

    # Aggregate returns to the chosen level from the hierarchy
    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(j in 1:length(h)){
        rp = as.xts(matrix(0, ncol = 1, nrow = nrow(Rp)), index(Rp))
        for(i in 1:length(h[[j]])){
            asset = h[[j]][i]
            r = as.data.frame(Rp)[asset] * as.data.frame(wp)[asset]
            r = as.xts(r)
            rp = rp + r
        }
        returns[, j] = rp
        colnames(returns) = names(h)
        }
    return(returns)
}


Weight.transform <- 
function(wp, Rp)
{
    # Transform weights to the xts object used by aggregation and attribution functions
    if (is.vector(wp)){
        wp = as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), index(Rp))
        colnames(wp) = colnames(Rp)
    } else{
        wp = merge(wp, xts(, index(Rp)))
        wp = na.locf(wp)
    }
    return(wp)
}

Weight.level <-
function(wp, h, level = "Sector")
{
    #aggregate weights to the level chosen from the hierarchy
    h = split(h$primary_id, h[level])
    weights = wp[, 1:length(h)]
    
    for(j in 1:length(h)){
        W = as.xts(matrix(0, ncol = 1, nrow = nrow(wp)), index(wp))
        for(i in 1:length(h[[j]])){
            asset = h[[j]][i]
            w = as.data.frame(wp)[asset]
            w = as.xts(w)
            W = W + w
        }
        weights[, j] = W
        colnames(weights) = names(h)
    }
    return(weights)
}

# Example
wp <- c(0.3, 0.2, 0.2, 0.1, 0.2)
wp <- Weight.transform(wp, Rp)
Return.level(Rp, wp, hierarchy, level = "Sector")
# with xts weights
wp <- Rp[1:2, ]
wp[1, ] <- c(0.3, 0.2, 0.2, 0.1, 0.2)
wp[2, ] <- c(0.3, 0.2, 0.2, 0.1, 0.2)
wp <- Weight.transform(wp, Rp)
Return.level(Rp, wp, hierarchy, level = "type")
Weight.level(wp, hierarchy, level = "Sector")


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