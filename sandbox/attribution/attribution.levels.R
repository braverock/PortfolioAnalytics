# 5-steps attribution (3-levels)
attribution.levels <-
function(Rp, Rb, wp, wb, h, ...)
{ # @author Andrii Babii

    Rb = checkData(Rb)
    Rp = checkData(Rp)

    levels <- unlist(list(...))
    if (!is.null(levels)) stopifnot(is.character(levels))
    
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
        bs[[i]] = Return.rebalancing(weights.p[[i]], returns.b[[i]])   # semi-notional funds returns
    } 
    names(returns.p) = levels
    names(weights.p) = levels
    names(returns.b) = levels
    names(weights.b) = levels

    # Get portfolio and benchmark returns
    r = Return.rebalancing(Rp, wp)
    b = Return.rebalancing(Rb, wb)
    
    # Total attribution effects
    allocation = list()
    allocation[[1]] = (1 + bs[[1]]) / (1 + b) - 1 # Allocation 1
    for (i in 2:length(levels)){
        allocation[[i]] = (1 + bs[[i]]) / (1 + bs[[i-1]]) - 1
    }
    allocation = as.xts(as.data.frame(allocation))
    selection = (1 + r) / (1 + last(bs)[[1]]) - 1
    total = (1 + r) / (1 + b) - 1 # Total excess return
    
    #level = list()
    #level[[1]] = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) / (1 + b) - 1)
    #for (i in 2:length(levels)){
    #    level[[i]] = (weights.p[[i]] - weights.b[[i]]) * ((1 + returns.b[[i]]) / (1 + returns.b[[i-1]]) - 1) * ((1 + returns.b[[i - 1]]) / (1 + bs[[i-1]]))
    #}

    # Level 1 attribution
    l1 = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) / (1 + b) - 1)
    
    # Level 2 attribution
    l2 = (weights.p[[2]] - weights.b[[2]]) * ((1 + returns.b[[2]]) / (1 + returns.b[[1]]) - 1) * ((1 + returns.b[[1]]) / (1 + bs[[1]]))
    
    # Level 3 attribution
    w = (weights.p[[3]] - weights.b[[3]])
    a1 = 1 + returns.b[[2]]
    b1 = ((1 + returns.b[[3]]) / (cbind(a1, a1, a1)) - 1)
    b2 = ((1 + returns.b[[2]]) / (1 + bs[[2]]))
    b2 = cbind(b2, b2, b2)
    l3 = w * b1 * b2

    # Security/Asset selection
    w = weights.p[[3]]
    a1 = cbind((1 + r), (1 + r), (1 + r)) 
    b1 = a1 / (1 + returns.b[[3]]) - 1
    a2 = cbind((1 + bs[[3]]), (1 + bs[[3]]), (1 + bs[[3]]))
    b2 = (1 + returns.b[[3]]) / a2
    select = w * b1 * b2

    result = list()
    general = cbind(allocation, selection, total)
    colnames(general) = c("L1 allocation", "L2 allocation", "L3 allocation", "Selection", "Total")
    result[[1]] = general
    result[[2]] = l1
    result[[3]] = l2
    result[[4]] = l3
    result[[5]] = select
    names(result) = c("Multi-level attribution", "Level 1 attribution", "Level 2 attribution", "Level 3 attribution", "Security selection")
    return(result)

}

# Example:
data(attrib) # !!! Load attrib.RData workspace
attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))


