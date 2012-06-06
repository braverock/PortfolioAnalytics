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
    # TODO: add GRAP and Frongello linking, Geometric attribution

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
    allocation = cbind(allocation, rowSums(allocation))    # Total allocation effect for each period
    names(allocation)[ncol(allocation)] = "Total"
    selection = wb * (Rp - Rb)
    selection = cbind(selection, rowSums(selection))       # Total selection effect for each period
    names(selection)[ncol(selection)] = "Total"
    interaction = (wp - wb) * (Rp - Rb)
    interaction = cbind(interaction, rowSums(interaction)) # Total interaction effect for each period
    names(interaction)[ncol(interaction)] = "Total"
    
    # Get total portfolio returns
    rp = reclass(rowSums(Rp * wp), Rp) 
    rb = reclass(rowSums(Rb * wb), Rb)
    names(rp) = "Total"
    names(rb) = "Total"
    Rp = cbind(Rp, rp)
    Rb = cbind(Rb, rb)
    rp.c = Return.cumulative(rp)
    rb.c = Return.cumulative(rb)
   
    if(linking == "carino"){
        # Carino linking #!!! Correct for equal portfolio and benchmark returns
        kt = (log(1 + rp) - log(1 + rb)) / (rp - rb) # Carino factors
        k = (log(1 + rp.c) - log(1 + rb.c)) / (rp.c - rb.c)
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * kt / k
            selection[, i] = selection[, i] * kt / k
            interaction[, i] = interaction[, i] * kt / k
        }
    }

    if(linking == "menchero"){
        # Menchero linking #!!! Correct for equal portfolio and benchmark returns
        M = ((rp.c - rb.c) / nrow(Rp)) / ((1 + rp.c)^(1 / nrow(Rp)) - (1 + rb.c)^(1 / nrow(Rp)))
        at = (rp.c - rb.c - M * sum(rp - rb)) * (rp - rb) / sum((rp - rb)^2)
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * (M + at)
            selection[, i] = selection[, i] * (M + at)
            interaction[, i] = interaction[, i] * (M + at)
        }
    }    

    if(linking == "grap"){
        # GRAP linking
        G = rp
        G[1] = prod(1 + rb[2:length(rp)])
        G[nrow(rp)] = prod(1 + rp[1:(length(rp) - 1)])
        for(i in 2:(nrow(Rp) - 1)){   # !!! Fix. Works only if t>3
            r = 1 + rp[1:(i-1)]
            b = 1 + rb[(i+1):(nrow(Rp))]
            G[i] = apply(r, 2, prod) * apply(b, 2, prod)
        }
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * G
            selection[, i] = selection[, i] * G
            interaction[, i] = interaction[, i] * G
        }
    }

    if(linking == "frongello"){
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
    
    if(linking == "geometric"){
        k = (log(1 + Rp) - log(1 + Rb)) / (Rp - Rb)
        allocation = exp(allocation * k) - 1
        selection = exp(selection * k) - 1
        interaction = exp(interaction * k) - 1
    }

    # Get attribution effects for the whole period
    total = allocation + selection + interaction
    
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

    # Select the appropriate result corresponding to the chosen method
    result = list()
    result[[1]] = allocation
    result[[2]] = selection
    result[[3]] = total
    if(method == "top.down"){     # Top-down attribution
        result[[2]] = result[[2]] + interaction
    }
    if(method == "bottom.up"){    # Bottom-up attribution
        result[[1]] = result[[1]] + interaction
    }
    if(method == "simple"){
        result[[4]] = result[[3]]
        result[[3]] = interaction
    }

    # Label the output
    if(method == "simple"){
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
Rp <- Return.level(Rp, wp, h, level = "Sector") # Sector-level attribution
Rb <- Return.level(Rb, wb, h, level = "Sector")
wp <- Weight.level(wp, h, level = "Sector")
wb <- Weight.level(wb, h, level = "Sector")
attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
attribution(Rp, wp, Rb, wb, method = "bottom.up", linking = "menchero")
attribution(Rp, wp, Rb, wb, method = "simple", linking = "grap")

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
