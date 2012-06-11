# Multi-level attribution
# TODO: find a way to make conformable returns at different levels
attribution.levels <-
function(Rp, Rb, wp, wb, h, ...)
{ # @author Andrii Babii

    Rp = checkData(Rp)
    Rb = checkData(Rb)
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    
    levels <- unlist(list(...))
    if (!is.null(levels)) stopifnot(is.character(levels))

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
   
    for (i in 1:length(bs)){
        bs[[i]] = as.xts(matrix(rep(bs[[i]], ncol(returns.p[[i]])), nrow(r), ncol(returns.p[[i]])), index(r))
    }

    # Attribution at each level
    level = list()
    level[[1]] = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) / (1 + b) - 1)
    for (i in 2:length(levels)){ # This does not work. Need to finish
        level[[i]] = (weights.p[[i]] - weights.b[[i]]) * ((1 + returns.b[[i]]) / (1 + returns.b[[i-1]]) - 1) * ((1 + returns.b[[i-1]]) / (1 + bs[[i-1]]))
    }

    # Security/Asset selection
    select = as.xts(as.data.frame(last(weights.p))) * ((1 + r) / (1 + as.xts(as.data.frame(last(returns.b)))) - 1) * ((1 + as.xts(as.data.frame(last(returns.b)))) / (1 + as.xts(as.data.frame(last(bs)))))

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
data(attrib)
attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))


