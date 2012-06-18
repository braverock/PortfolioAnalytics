#' performs geometric attribution 
#' 
#' Performance attribution of geometric excess returns. Calculates total 
#' geometric attribution effects over multiple periods. Used internally by the 
#' \code{\link{Attribution}} function. Geometric attribution effects in the
#' conrast with arithmetic do naturally link over time multiplicatively:
#' \deqn{\frac{(1+r)}{1+b}-1=\overset{n}{\underset{t=1}{\prod}}(1+A_{t}^{G})\times\overset{n}{\underset{t=1}{\prod}}(1+S{}_{t}^{G})-1}
#' , where
#' \deqn{A_{t}^{G}} - total allocation effect at time t
#' \deqn{S_{t}^{G}} - total selection effect at time t
#' \deqn{A_{t}^{G}=\frac{1+b_{s}}{1+b_{t}}-1}
#' \deqn{S_{t}^{G}=\frac{1+r_{t}}{1+b_{s}}-1}
#' \deqn{b_{s}=\overset{n}{\underset{i=1}{\sum}}wp_{i}\times rb_{i}}
#' \deqn{b_{s}} - semi-notional fund
#' \deqn{wp_{t}} - portfolio weights at time t
#' \deqn{wb_{t}} - benchmark weights at time t
#' \deqn{r_{t}} - portfolio returns at time t
#' \deqn{b_{t}} - benchmark returns at time t
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{n} - number of periods
#'
#' @aliases Attribution.geometric
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009. 
#' Chapter 18-19
#' 
#' Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 5, 8
#' @keywords attribution, geometric attribution, geometric linking
#' @examples
#' 
#' data(attrib)
#' Attribution.geometric(Rp, wp, Rb, wb)
#' 
#' @export
Attribution.geometric <-
function(Rp, wp, Rb, wb)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
  
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
  
    # Outputs: 
    # This function returns the list with attribution effects (allocation,
    # selection and total effects) including total multi-period 
    # attribution effects
  
    # FUNCTION:
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)  
    names(rp) = "Total"                    
    names(rb) = "Total"                 
    bs = reclass(rowSums((wp * Rb[, 1:ncol(wp)])), rp) # Seminotional funds returns
    allocation = ((1 + Rb) / (1 + rep(rb, ncol(Rp))) - 1) * (wp - wb) # Geometric attribution effects for individual categories
    selection = wp * (Rp - Rb) / (1 + rep(bs, ncol(Rp)))
    allocation = cbind(allocation, rowSums(allocation)) # Total effects
    selection = cbind(selection, rowSums(selection))
    colnames(allocation)[ncol(allocation)] = "Total"
    colnames(selection)[ncol(selection)] = "Total"
    
    # Link single-period attribution effects
    a = (apply(1 + allocation[, ncol(allocation)], 2, prod) - 1)
    s = (apply(1 + selection[, ncol(selection)], 2, prod) - 1)
    (apply(1 + selection[, ncol(selection)], 2, prod) - 1)
    allocation = rbind(as.data.frame(allocation), c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), c(rep(NA, ncol(selection) - 1), s))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    
    # Get annualized geometric excess returns
    excess.returns = (1 + rp) / (1 + rb) - 1
    rp.a = prod(1 + rp) - 1              
    rb.a = prod(1 + rb) - 1
    ger.a = as.matrix((1 + rp.a) / (1 + rb.a) - 1)  # Geometric (annualized) excess returns
    rownames(ger.a) = "Total geometric"
    excess.returns = rbind(as.matrix(excess.returns), ger.a)
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    names(result) = c("Excess returns", "Allocation", "Selection")
    return(result)
}