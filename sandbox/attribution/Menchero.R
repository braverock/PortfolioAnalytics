# Menchero linking
Menchero <-
function(rp, rb, attributions)
{   # @author Andrii Babii
    rp.a = prod(1 + rp) - 1              
    rb.a = prod(1 + rb) - 1
    T = nrow(rp)
    if (rp.a == rb.a){
        M = (1 + rb.a)^((T - 1) / T)
        at = 0
    } else{
        M = ((rp.a - rb.a) / T) / ((1 + rp.a)^(1 / T) - (1 + rb.a)^(1 / T))
        at = (rp.a - rb.a - M * sum(rp - rb)) * (rp - rb) / sum((rp - rb)^2)
    }
    m = matrix(rep(M + at, ncol(attributions)), nrow(attributions), ncol(attributions), byrow = FALSE)
    total = colSums(attributions * m)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}