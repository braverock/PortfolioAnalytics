#' calculates total attribution effects using Davies and Laker smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Davies and Laker linking method. Used internally by the 
#' \code{\link{Attribution}} function. Arithmetic attribution effects do not 
#' naturally link over time. This function uses Davies and Laker linking method
#' to compute total attribution effects. 
#' Arithmetic excess returns are decomposed as follows:
#' \deqn{r - b = Allocation + Selection + Interaction}
#' \deqn{Allocation = \overset{T}{\underset{t=1}{\prod}}(1+bs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
#' \deqn{Selection = \overset{T}{\underset{t=1}{\prod}}(1+rs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
#' \deqn{Interaction = \overset{T}{\underset{t=1}{\prod}}(1+rp_{t})-\overset{T}{\underset{t=1}{\prod}}(1+rs_{t})-\overset{T}{\underset{t=1}{\prod}}(1+bs_{t})+\overset{T}{\underset{t=1}{\prod}}(1+rb_{t})}
#' \deqn{rp_{i}} - portfolio returns at period \deqn{i}
#' \deqn{rb_{i}} - benchmark returns at period \deqn{i}
#' \deqn{rs_{i}} - selection notional fund returns at period \deqn{i}
#' \deqn{bs_{i}} - allocation notional fund returns at period \deqn{i}
#' \deqn{T} - number of periods
#' 
#' @aliases DaviesLaker
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @return This function returns the data.frame with original attribution effects
#' and total attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Carino}} \cr
#' \code{\link{Attribution.geometric}} \cr \code{\link{Frongello}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 201-204
#' 
#' Davies, O. and Laker, D. (2001) Multiple-period performance attribution 
#' using the brinson model.Journal of Performance MeasurementFall, 12–22.
#' @keywords arithmetic attribution, Davies and Laker linking
#' @examples
#' 
#' data(attrib)
#' DaviesLaker(Rp, wp, Rb, wb)
#' 
#' @export
DaviesLaker <-
function(Rp, wp, Rb, wb)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Davies and Laker linking. Used internally by the Attribution function
  
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
    
    # FUNCTION:
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)  
    colnames(rp) = "Total"                    
    colnames(rb) = "Total"                 
    bs = reclass(rowSums((wp * Rb[, 1:ncol(wp)])), Rp) # Allocation notional fund returns
    rs = reclass(rowSums((wb * Rp[, 1:ncol(wb)])), Rp) # Selection notional fund returns
    a = apply(1 + bs, 2, prod) - apply(1 + rb, 2, prod)
    s = apply(1 + rs, 2, prod) - apply(1 + rb, 2, prod)
    i = apply(1 + rp, 2, prod) - apply(1 + rs, 2, prod) - apply(1 + bs, 2, prod) + apply(1 + rb, 2, prod)
    
    # Compute attribution effects (Brinson, Hood and Beebower model)
    allocation = (wp - wb) * Rb
    selection = wb * (Rp - Rb)
    interaction = (wp - wb) * (Rp - Rb)
    n = ncol(allocation)               # number of segments
    allocation = cbind(allocation, rowSums(allocation))
    names(allocation)[n + 1] = "Total"  
    selection = cbind(selection, rowSums(selection))
    names(selection)[n + 1] = "Total"   
    interaction = cbind(interaction, rowSums(interaction))
    names(interaction)[n + 1] = "Total"
    
    allocation = rbind(as.data.frame(allocation), c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), c(rep(NA, ncol(selection) - 1), s))
    interaction = rbind(as.data.frame(interaction), c(rep(NA, ncol(interaction) - 1), i))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    rownames(interaction)[nrow(allocation)] = "Total"

    excess.returns = rp - rb
    rp.a = prod(1 + rp) - 1              
    rb.a = prod(1 + rb) - 1
    aer.a = as.matrix(rp.a - rb.a)                  # Arithmetic (annualized) excess returns
    rownames(aer.a) = "Total arithmetic"
    excess.returns = rbind(as.matrix(excess.returns), aer.a)
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    result[[4]] = interaction
    names(result) = c("Excess returns", "Allocation", "Selection", "Interaction")
    return(result)
}
