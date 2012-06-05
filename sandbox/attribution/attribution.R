#' performs arithmetic attribution
#' 
#' @aliases attribution.arithmetic
#' 
#' Performs arithmetic attribution analysis of returns. Used to uncover the sources 
#' of portfolio return 
#'
#' @aliases attribution.arithmetic
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
attribution.arithmetic <- 
function (Rp, wp, Rb, wb, method = c("top.down", "bottom.up", "simple"), 
linking = c("carino", "menchero", "grap", "frongello", "geometric"))
{ # @author Andrii Babii

    # DESCRIPTION:
    # This is a wrapper for attribution analysis.
    # TODO: add GRAP and Frongello linking, Geometric attribution

    # Inputs:
    # Rp: portfolio returns
    # wp: portfolio weights
    # Rb: benchmark returns
    # wb: benchmark weights
    # method: 
  
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
    
    
    # Get aggregated portfolio returns
    rp = reclass(rowSums(Rp * wp), Rp) 
    rb = reclass(rowSums(Rb * wb), Rb)
    rp.a = prod(rp + 1) - 1 
    rb.a = prod(rb + 1) - 1
   
    if(linking == "carino"){
        # Carino linking #!!! Correct for equal portfolio and benchmark returns
        kt = (log(1 + rp) - log(1 + rb)) / (rp - rb) # Carino factors
        k = (log(1 + rp.a) - log(1 + rb.a)) / (rp.a - rb.a)
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * kt / k
            selection[, i] = selection[, i] * kt / k
            interaction[, i] = interaction[, i] * kt / k
        }
    }

    if(linking == "menchero"){
        # Menchero linking #!!! Correct for equal portfolio and benchmark returns
        M = ((rp.a - rb.a) / nrow(Rp)) / ((1 + rp.a)^(1 / nrow(Rp)) - (1 + rb.a)^(1 / nrow(Rp)))
        at = (rp.a - rb.a - M * sum(rp - rb)) * (rp - rb) / sum((rp - rb)^2)
        for(i in 1:ncol(allocation)){
            allocation[, i] = allocation[, i] * (M + at)
            selection[, i] = selection[, i] * (M + at)
            interaction[, i] = interaction[, i] * (M + at)
        }
    }    

    if(linking == "grap"){
        # GRAP linking
        
    }

    if(linking == "frongello"){
        # Frongello linking
    
    }
    
    if(linking == "geometric"){
        k = (log(1 + Rp) - log(1 + Rb)) / (Rp - Rb)
        allocation = exp(allocation * k) - 1
        selection = exp(selection * k) - 1
        interaction = exp(interaction * k) - 1
    }

    # Get attribution effects for the whole period
    allocation = as.data.frame(allocation)
    allocation = rbind(allocation, colSums(allocation))
    rownames(allocation)[nrow(allocation)] = "Total"
    selection = as.data.frame(selection)
    selection = rbind(selection, colSums(selection))
    rownames(selection)[nrow(selection)] = "Total"
    interaction = as.data.frame(interaction)
    interaction = rbind(interaction, colSums(interaction))
    rownames(interaction)[nrow(interaction)] = "Total"
    total = as.data.frame(total)
    total = rbind(total, colSums(total))
    rownames(total)[nrow(total)] = "Total"
    total = allocation + selection + interaction

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
Rp <- Return.level(Rp, wp, h, level = "Sector") # Sector-level attribution
Rb <- Return.level(Rb, wb, h, level = "Sector")
wp <- Weight.level(wp, h, level = "Sector")
wb <- Weight.level(wb, h, level = "Sector")
attribution.arithmetic(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
attribution.arithmetic(Rp, wp, Rb, wb, method = "bottom.up", linking = "menchero")
attribution.arithmetic(Rp, wp, Rb, wb, method = "simple", linking = "carino")

#' @export 
#' @rdname attribution.arithmetic

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
