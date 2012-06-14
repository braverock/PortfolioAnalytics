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