#' calculates total attribution effects using Menchero smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Menchero linking method. Used internally by the \code{\link{Attribution}}
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses Menchero smoothing algorithm to adjust
#' attribution effects so that they can be summed up over multiple periods
#' Attribution effect are multiplied by the adjustment factor 
#' \deqn{A_{t}' = A_{t} \times (M +a_{t})}{At' = At * (M + at)}
#' where \deqn{M=\frac{\frac{1}{n}(R_{p}-R_{b})}{(1+R_{p})^{\frac{1}{n}}-
#' (1+R_{b})^{\frac{1}{n}}}}
#' \deqn{a_{t} = \left(\frac{R_{p}-R_{b}-M\times\sum^{n}_{t=1}(R_{pt}-
#' R_{bt})}{\sum^{n}_{t=1}(R_{pt}-R_{bt})^{2}}\right)\times(R_{pt}-R_{bt})}
#' In case if portfolio and benchmark returns are equal the limit of the
#' above value is used:
#' \deqn{M = (1 + r_{p})^\frac{n-1}{n}}{M = (1 + rp)^((n - 1) / n)}
#' \eqn{A_{t}'}{At'} - adjusted attribution effects at period \eqn{t}, 
#' \eqn{A_{t}}{At} - unadjusted attribution effects at period \eqn{t}, 
#' \eqn{R_{pt}}{Rpt} - portfolio returns at period \eqn{t}, 
#' \eqn{R_{bt}}{Rbt} - benchmark returns at period \eqn{t}, 
#' \eqn{R_{p}}{Rp} - total portfolio returns, 
#' \eqn{R_{b}}{Rb} - total benchmark returns, 
#' \eqn{n} - number of periods
#' 
#' The total arithmetic excess returns can be explained in terms of the sum 
#' of adjusted attribution effects: 
#' \deqn{R_{p} - R_{b} = \sum^{n}_{t=1}\left(Allocation_{t}+Selection_{t}+
#' Interaction_{t}\right)}
#'
#' @aliases Menchero
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
#' @param attributions  xts with attribution effects
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period
#' @return returns a data frame with original attribution effects and total 
#' attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Carino}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Frongello}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 194-196 \cr Menchero, J. (2000) \emph{An 
#' optimized approach to linking attribution effects over time}. Journal of 
#' Performance Measurement. Fall. p. 36-42 \cr
#' @keywords arithmetic attribution, Menchero linking
#' @examples
#' 
#' data(attrib)
#' Menchero(rp = attrib.returns[, 21], rb = attrib.returns[, 22], attributions = attrib.allocation, adjusted = FALSE)
#' 
#' @export
Menchero <-
function(rp, rb, attributions, adjusted)
{   # @author Andrii Babii
    
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Menchero linking. Used internally by the Attribution function
  
    # Inputs:
    # rp            xts of portfolio returns
    # rb            xts of benchmark returns
    # attributions  attribution effects (allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
  
    # FUNCTION:
    rpc = prod(1 + rp) - 1    # Cumulative returns          
    rbc = prod(1 + rb) - 1
    T = nrow(rp)
    if (rpc == rbc){
      M = (1 + rbc)^((T - 1) / T)
      at = 0
    } else{
      M = ((rpc - rbc) / T) / ((1 + rpc)^(1 / T) - (1 + rbc)^(1 / T))
      at = (rpc - rbc - M * sum(rp - rb)) * (rp - coredata(rb)) / sum((rp - 
        coredata(rb))^2)
    }
    m = matrix(rep(M + at, ncol(attributions)), nrow(attributions), 
               ncol(attributions), byrow = FALSE)
    adj = attributions * m
    total = colSums(adj)
    if (adjusted == FALSE){
      attributions = rbind(as.data.frame(attributions), total)
    } else{
      attributions = rbind(as.data.frame(adj), total)
    }
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}