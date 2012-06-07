#' performs arithmetic attribution
#' 
#' @aliases attribution.crithmetic
#' 
#' Performs arithmetic attribution analysis of returns. Used to uncover the sources 
#' of portfolio return 
#'
#' @aliases attribution.crithmetic
#' @param Rp portfolio returns
#' @param wp portfolio weights
#' @param Rb benchmark returns
#' @param wb benchmark weights
#' @author Andrii Babii
#' @seealso
#' @references Jon A. Christopherson, David R., Wayne E. Ferson 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009.
#' @examples
#' 
#' 
#'
attribution <- 
function (Rp, wp, Rb, wb, method = c("top.down", "bottom.up", "simple"), 
linking = c("carino", "menchero", "grap", "frongello", "geometric"))
{ # @author Andrii Babii

    # DESCRIPTION:
    # Attribution analysis.
    # TODO: fix bugs in linking methods, multi-level attribution. 
    # Fix bugs in examples

    # Inputs:
    # Rp: portfolio returns
    # wp: portfolio weights
    # Rb: benchmark returns
    # wb: benchmark weights
    # method: 
    # linking: 
  
    # Outputs: 
    # This function returns the attribution effects
    # FUNCTION:
    
    # Transform data to the xts
    Rb = checkData(Rb)
    Rp = checkData(Rp)
    wp = Weight.transform(Rp, wp)
    wb = Weight.transform(Rb, wb)

    # Compute attribution effects
    allocation = (wp - wb) * Rb
    selection = wb * (Rp - Rb)
    interaction = (wp - wb) * (Rp - Rb)

    # Get total attribution effects
    allocation = cbind(allocation, rowSums(allocation))
    names(allocation)[ncol(allocation)] = "Total"
    selection = cbind(selection, rowSums(selection))
    names(selection)[ncol(selection)] = "Total"
    interaction = cbind(interaction, rowSums(interaction))
    names(interaction)[ncol(interaction)] = "Total"
    total = allocation + selection + interaction
      
    # Get total portfolio returns
    rp = reclass(rowSums(Rp * wp), Rp) 
    rb = reclass(rowSums(Rb * wb), Rb)
    names(rp) = "Total"
    names(rb) = "Total"
    rp.a = prod(rp + 1) - 1 
    rb.a = prod(rb + 1) - 1
    Rp = cbind(Rp, rp)
    Rb = cbind(Rb, rb)
    
    # Adjust attribution effects using one of linking methods
    if (linking == "carino"){
        # Carino linking
        kt = rp
        for (t in 1:nrow(kt)){
            if (rp[t] == rb[t]){
                kt[t] = 1 / (1 + rp[t]) # Carino factors if portfolio and benchmark returns are equal
            } else{
                kt[t] = (log(1 + rp[t]) - log(1 + rb[t])) / (rp[t] - rb[t]) # if different
            }
        }
        k = (log(1 + rp.a) - log(1 + rb.a)) / (rp.a - rb.a)
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * kt / k
            selection[, i] = selection[, i] * kt / k
            interaction[, i] = interaction[, i] * kt / k
        }
    }

    if (linking == "menchero"){
        # Menchero linking
        T = nrow(Rp)
        if (rp.a == rb.a){
            M = (1 + rb.a)^((T - 1) / T)
            at = 0
        } else{
            M = ((rp.a - rb.a) / T) / ((1 + rp.a)^(1 / T) - (1 + rb.a)^(1 / T))
            at = (rp.a - rb.a - M * sum(rp - rb)) * (rp - rb) / sum((rp - rb)^2)
        }
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * (M + at)
            selection[, i] = selection[, i] * (M + at)
            interaction[, i] = interaction[, i] * (M + at)
        }
    }    

    if (linking == "grap"){
        # GRAP linking
        G = rp
        T = nrow(Rp)
        G[1] = prod(1 + rb[2:T])           #GRAP factor for the first period
        if (T == 2){
            G[2] = (1 + rp[1])
        }
        if (T > 2){
            G[T] = prod(1 + rp[1:(T - 1)]) #GRAP factor for the last period
        }
        if (T > 3){
            for(i in 2:(T - 1)){
                r = 1 + rp[1:(i-1)]
                b = 1 + rb[(i+1):T]
                G[i] = apply(r, 2, prod) * apply(b, 2, prod)
            }
        }
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * G
            selection[, i] = selection[, i] * G
            interaction[, i] = interaction[, i] * G
        }
    }

    if (linking == "frongello"){
        # Frongello linking
        allocation[1, ] = allocation[1, ] + Rb[1, ]
        selection[1, ] = selection[1, ] + Rb[1, ]
        interaction[1, ] = interaction[1, ] + Rb[1, ]
        for(i in 2:nrow(Rp)){
            allocation[i, ] = allocation[i, ] * apply((1 + Rp[1:(i-1), ]), 2, prod) + Rb[i, ] * apply(allocation[1:(i-1)], 2, sum)
            selection[i, ] = selection[i, ] * apply((1 + Rp[1:(i-1), ]), 2, prod) + Rb[i, ] * apply(selection[1:(i-1)], 2, sum)
            interaction[i, ] = interaction[i, ] * apply((1 + Rp[1:(i-1), ]), 2, prod) + Rb[i, ] * apply(interaction[1:(i-1)], 2, sum)
        }
    }
    
    if (linking == "geometric"){
        bs = reclass(rowSums((wp * Rb[, 1:ncol(wp)])), rp) # Seminotional fund returns
        allocation = ((1 + Rb) / (1 + rb.a) - 1) * cbind((wp - wb), rep(1, nrow(wp)))
        for (i in 1:ncol(wp)){
            selection[, i] = ((Rp - Rb) * cbind(wp, rep(1, nrow(wp))))[, i] / (1 + bs)
        }
    }
    
    if (linking != "geometric"){
        totals <- function(x){
            x = as.data.frame(x)
            x = rbind(x, colSums(x))
            rownames(x)[nrow(x)] = "Total"
            return(x)
        }
        allocation = totals(allocation)
        selection = totals(selection)
        interaction = totals(interaction)
        total = totals(total)
    } else{
        allocation = rbind(as.data.frame(allocation), (apply(1 + allocation, 2, prod) - 1))
        selection = rbind(as.data.frame(selection), (apply(1 + selection, 2, prod) - 1))
        rownames(allocation)[nrow(allocation)] = "Total"
        rownames(selection)[nrow(selection)] = "Total"
    }
    # Select the appropriate result corresponding to the chosen method
    result = list()
    result[[1]] = allocation
    result[[2]] = selection
    result[[3]] = total
    if (linking != "geometric"){
        if (method == "top.down"){     # Top-down attribution
            result[[2]] = result[[2]] + interaction
        }
        if (method == "bottom.up"){    # Bottom-up attribution
            result[[1]] = result[[1]] + interaction
        }
        if (method == "simple"){
            result[[4]] = result[[3]]
            result[[3]] = interaction
        }
    }
    # Label the output
    if (method == "simple" & linking != "geometric"){
        names(result) = c("Allocation", "Selection", "Interaction", "Total")
    } else{
        names(result) = c("Allocation", "Selection", "Total")
    }
    return(result)
}

#EXAMPLE:
data(attrib) # !!! Load attrib.RData workspace
require(FinancialInstrument)
require(PerformanceAnalytics)
attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
attribution(Rp, wp, Rb, wb, method = "bottom.up", linking = "menchero")
attribution(Rp, wp, Rb, wb, method = "simple", linking = "grap")
attribution(Rp, wp, Rb, wb, method = "top.down", linking = "frongello")
attribution(Rp, wp, Rb, wb, method = "bottom.up", linking = "geometric")

#' @export 
#' @rdname attribution

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
