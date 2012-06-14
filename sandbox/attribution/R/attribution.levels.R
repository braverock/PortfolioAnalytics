#' provides multi-level geometric peformance attribution
#' 
#' Provides multi-level geometric peformance attribution. The Brinson model
#' attribute excess returns at one level of the decision process. This 
#' function works with more complex decision processes. For instance, the 
#' 3-level decision process may have the following levels: type of asset - 
#' country - sector. The levels should be specified in the vector with 
#' elements in the particular order: from the highest level to the lowest.
#' The contribution to the allocation in the ith category for the dth level  
#' is: \deqn{\left(^{d}wp_{i}-^{d}wb_{i}\right)\times\left(\frac{1+^{d}b_{i}}{1+^{d-1}b_{i}}-1\right)\times\frac{1+^{d-1}b_{i}}{1+bs^{d-1}}}
#' The total attribution for each asset allocation step in the decision process
#' is: \deqn{\frac{1+^{d}bs}{1+^{d-1}bs}-1}
#' The final step, stock selection, is measured by:
#' \deqn{^{d}w_{i}\times\left(\frac{1+r_{i}}{1+^{d}b_{i}}-1\right)\times\frac{1+^{d}b_{i}}{1+^{d}bs}}
#' 
#' @aliases Attribution
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param h data.frame with the hierarchy obtained from the buildHierarchy 
#' function or defined manually in the the same style as buildHierarchy's
#' output
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 215-220
#' @keywords multi-level attribution, geometric attribution
#' @examples
#' 
#' data(attrib)
#' Attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))
#' Attribution.levels(Rp, wp, Rb, wb, h, c("type", "Sector"))
#' 
#' @export
#' @TODO label the output for arbitrary number of levels (more than 4) 
#' compute total effects for multiple periods
Attribution.levels <-
function(Rp, wp, Rb, wb, h, ...)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
  
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # h        data.frame with the hierarchy
  
    # Outputs: 
    # This function returns the list with total attribution effects 
    # (allocation, selection and total) including total multi-period 
    # attribution effects, attribution effects at each level and secruity
    # selection
  
  # FUNCTION:
    Rp = checkData(Rp)
    Rb = checkData(Rb)
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
        Rp = Rp[2:nrow(Rp)]
        Rb = Rb[2:nrow(Rb)]
    }

    levels <- unlist(list(...))
    if (!is.null(levels)) stopifnot(is.character(levels))
    if (length(levels) == 1){
      stop("Use Attribution function for the single level. This function is for the multi-level attribution")
    }

    # Get portfolio and benchmark returns
    r = Return.rebalancing(Rp, wp)
    b = Return.rebalancing(Rb, wb)

    # Get returns and weights at all levels
    returns.p = list()
    weights.p = list()
    returns.b = list()
    weights.b = list()
    bs = list()
    for(i in 1:length(levels)){
        returns.p[[i]] = Return.level(Rp, wp, h, level = levels[i])
        weights.p[[i]] = Weight.level(wp, h, level = levels[i])
        returns.b[[i]] = Return.level(Rb, wb, h, level = levels[i])
        weights.b[[i]] = Weight.level(wb, h, level = levels[i])
        bs[[i]] = reclass(rowSums(returns.b[[i]] * weights.p[[i]]), r)  # semi-notional funds returns
    } 
    names(returns.p) = levels
    names(weights.p) = levels
    names(returns.b) = levels
    names(weights.b) = levels

    # Total attribution effects
    allocation = list()
    allocation[[1]] = (1 + bs[[1]]) / (1 + b) - 1 # Allocation 1
    for (i in 2:length(levels)){
        allocation[[i]] = (1 + bs[[i]]) / (1 + bs[[i-1]]) - 1
    }
    allocation = as.xts(as.data.frame(allocation))
    selection = (1 + r) / (1 + last(bs)[[1]]) - 1
    total = (1 + r) / (1 + b) - 1 # Total excess return
    
    # Transform portfolio, benchmark returns and semi-notional funds returns to conformable matrices for multi-level attribution
    b = as.xts(matrix(rep(b, ncol(returns.b[[1]])), nrow(b), ncol(returns.b[[1]])), index(b))
    r = as.xts(matrix(rep(r, ncol(last(returns.b)[[1]])), nrow(r), ncol(last(returns.b)[[1]])), index(r))
    
    returns.b2 = list()
    for (j in 1:(length(levels) - 1)){ # make benchmark returns conformable at different levels
        r_l = Return.level(Rb, wb, h, level = levels[j])
        r_h = Return.level(Rb, wb, h, level = levels[j+1])
        hierarchy = split(h[levels[j]], h[levels[j+1]])
        for (i in 1:ncol(r_h)){
            r_h[, i] = r_l[, hierarchy[[i]][1, 1]]
        }
        returns.b2[[j]] = r_h
    }

    for (i in 1:(length(bs) - 1)){
        bs[[i]] = as.xts(matrix(rep(bs[[i]], ncol(returns.b2[[i]])), nrow(r), ncol(returns.b2[[i]])), index(r))
    }
    bs[length(bs)] = bs[length(bs) - 1]

    # Attribution at each level
    level = list()
    level[[1]] = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) / (1 + b) - 1)
    for (i in 2:length(levels)){ 
        level[[i]] = (weights.p[[i]] - weights.b[[i]]) * ((1 + returns.b[[i]]) / (1 + returns.b2[[i-1]]) - 1) * ((1 + returns.b2[[i-1]]) / (1 + bs[[i-1]]))
    }

    # Security/Asset selection
    select = as.xts(as.data.frame(last(weights.p))) * ((1 + r) / (1 + as.xts(as.data.frame(last(returns.b)))) - 1) * ((1 + as.xts(as.data.frame(last(returns.b)))) / (1 + as.xts(as.data.frame(last(bs)))))

    # Get the multi-period summary
    general = cbind(allocation, selection, total)
    general = rbind(as.data.frame(general), (apply(1 + general, 2, prod) - 1))
    for (i in 1:length(level)){
        level[[i]] = rbind(as.data.frame(level[[i]]), (apply(1 + level[[i]], 2, prod) - 1))
        rownames(level[[i]])[nrow(level[[i]])] = "Total"
    }
    select = rbind(as.data.frame(select), (apply(1 + select, 2, prod) - 1))
    rownames(general)[nrow(general)] = "Total"
    rownames(select)[nrow(select)] = "Total"
        
    # Label output
    result = list()
    if (length(levels) == 2){
        colnames(general) = c("L1 allocation", "L2 allocation", "Selection", "Total")
        names(level) = c("Level 1", "Level 2")
    }
    if (length(levels) == 3){
        colnames(general) = c("L1 allocation", "L2 allocation", "L3 allocation", "Selection", "Total")
        names(level) = c("Level 1", "Level 2", "Level 3")
    }
    if (length(levels) == 4){
        colnames(general) = c("L1 allocation", "L2 allocation", "L3 allocation", "L4 allocation", "Selection", "Total")
        names(level) = c("Level 1", "Level 2", "Level 3", "Level 4")
    }
    
    result[[1]] = general
    result[[2]] = level
    result[[3]] = select
    names(result) = c("Multi-level attribution", "Attribution at each level", "Security selection")
    return(result)
}