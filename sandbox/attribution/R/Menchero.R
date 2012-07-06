#' calculates total attribution effects using Menchero smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Menchero linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses Menchero smoothing algorithm to adjust
#' attribution effects so that they can be summed up over multiple periods
#' Attribution effect are multiplied by the adjustment factor 
#' \deqn{A_{t}' = A_{t} \times (M +a_{t})},
#' where \deqn{M=\frac{\frac{1}{n}(r-b)}{(1+r)^{\frac{1}{n}}-(1+b)^{\frac{1}{n}}}}
#' \deqn{a_{t} = \left(\frac{r-b-M\times\overset{n}{\underset{t=1}{\sum}}(r_{t}-b_{t})}{\overset{n}{\underset{t=1}{\sum}}(r_{t}-b_{t})^{2}}\right)\times(r_{t}-b_{t})}
#' In case if portfolio and benchmark returns are equal 
#' \deqn{M = (1 + r)^\frac{n-1}{n}}
#' \deqn{A_{t}}' - adjusted attribution effects at period \deqn{t}
#' \deqn{A_{t}} - unadjusted attribution effects at period \deqn{t}
#' \deqn{r_{t}} - portfolio returns at period \deqn{t}
#' \deqn{b_{t}} - benchmark returns at period \deqn{t}
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{n} - number of periods
#' The total arithmetic excess returns can be explained in terms of the sum 
#' of adjusted attribution effects: 
#' \deqn{r - b = \overset{n}{\underset{t=1}{\sum}}\left(Allocation_{t}+Selection_{t}+Interaction_{t}\right)}
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
#' Attribution}. Wiley. 2004. p. 194-196
#' 
#' Menchero, J. (2000) \emph{An optimized approach to linking attribution 
#' effects over time}.Journal of Performance Measurement. Fall, 36–42.
#' @keywords arithmetic attribution, Menchero linking
#' @examples
#' 
#' data(attrib)
#' Menchero(rp, rb, allocation, adjusted = FALSE)
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
    # attributions  attribution effects (e.g. allocation, selection, interaction)
  
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
      at = (rpc - rbc - M * sum(rp - rb)) * (rp - coredata(rb)) / sum((rp - coredata(rb))^2)
    }
    m = matrix(rep(M + at, ncol(attributions)), nrow(attributions), ncol(attributions), byrow = FALSE)
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