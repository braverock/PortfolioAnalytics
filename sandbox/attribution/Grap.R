# GRAP linking
Grap <-
function(rp, rb, attributions)
{   # @author Andrii Babii
    G = rp
    T = nrow(rp)
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
    g = matrix(rep(G, ncol(attributions)), nrow(attributions), ncol(attributions), byrow = FALSE)
    total = colSums(attributions * g)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}