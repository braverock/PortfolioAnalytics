# Frongello linking
Frongello <-
function(Rp, wp, Rb, wb, attributions)
{   # @author Andrii Babii
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)
    Rp = cbind(Rp, rp)
    Rb = cbind(Rb, rb)
    attr = attributions
    attr[1, ] = attributions[1, ] + Rb[1, ]
    
    for(i in 2:nrow(rp)){
        attr[i, ] = attr[i, ] * apply((1 + Rp[1:(i-1), ]), 2, prod) + Rb[i, ] * apply(attr[1:(i-1)], 2, sum)
    }
    total = colSums(attr)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}
