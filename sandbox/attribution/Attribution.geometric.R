# Geometric attribution
Attribution.geometric <-
function(Rp, wp, Rb, wb)
{
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)  
    names(rp) = "Total"                    
    names(rb) = "Total"                 
    rp.a = prod(1 + rp) - 1              
    rb.a = prod(1 + rb) - 1
    Rp = cbind(Rp, rp)
    Rb = cbind(Rb, rb)
    
    bs = reclass(rowSums((wp * Rb[, 1:ncol(wp)])), rp) # Seminotional funds returns
    allocation = ((1 + Rb) / (1 + rb.a) - 1) * cbind((wp - wb), rep(1, nrow(wp)))
    selection = allocation
    for (i in 1:ncol(wp)){
      selection[, i] = ((Rp - Rb) * cbind(wp, rep(1, nrow(wp))))[, i] / (1 + bs)
    }
    allocation = rbind(as.data.frame(allocation), (apply(1 + allocation, 2, prod) - 1))
    selection = rbind(as.data.frame(selection), (apply(1 + selection, 2, prod) - 1))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    result = list()
    result[[1]] = allocation
    result[[2]] = selection
    result[[3]] = allocation + selection
    names(result) = c("Allocation", "Selection", "Total")
    return(result)
}