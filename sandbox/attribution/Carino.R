# Carino linking
Carino <- 
function(rp, rb, attributions)
{   # @author Andrii Babii
    rp.a = prod(1 + rp) - 1              
    rb.a = prod(1 + rb) - 1
    k = (log(1 + rp.a) - log(1 + rb.a)) / (rp.a - rb.a)
    kt = rp
    for (t in 1:nrow(kt)){
      if (rp[t] == rb[t]){
        kt[t] = 1 / (1 + rp[t]) # Carino factors if portfolio and benchmark returns are equal
      } else{
        kt[t] = (log(1 + rp[t]) - log(1 + rb[t])) / (rp[t] - rb[t]) # if different
      }
    }
    kt = matrix(rep(kt, ncol(attributions)), nrow(attributions), ncol(attributions), byrow = FALSE)
    total = colSums(attributions * kt / k)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}