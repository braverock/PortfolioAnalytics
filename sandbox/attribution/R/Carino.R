#' calculates total attribution effects using logarithmic smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' logarithmic linking method. Used internally by the \code{\link{Attribution}}
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses logarithmic smoothing to adjust attribution effects 
#' so that they can be summed up over multiple periods. Attribution effect 
#' are multiplied by the adjustment factor: 
#' \deqn{A_{t}' = A_{t} \times \frac{k_{t}}{k}}{At' = At * kt / k}
#' where \deqn{k_{t} = \frac{log(1 + R_{pt}) - 
#' log(1 + R_{bt})}{R_{pt} - R_{bt}}}
#' \deqn{k = \frac{log(1 + R_{p}) - log(1 + R_{b})}{R_{p} - R_{b}}}
#' In case if portfolio and benchmark returns are equal:
#' \deqn{k_{t} = \frac{1}{1 + R_{pt}}}{kt = 1 / (1 + Rpt)}
#' where \eqn{A_{t}'}{At'} - adjusted attribution effects at period \eqn{t},
#' \eqn{A_{t}}{At} - unadjusted attribution effects at period \eqn{t}, 
#' \eqn{R_{pt}}{Rpt} - portfolio returns at period \eqn{t}, 
#' \eqn{R_{bt}}{Rbt} - benchmark returns at period \eqn{t}, 
#' \eqn{R_{p}}{Rp} - total portfolio returns, 
#' \eqn{R_{b}}{Rb} - total benchmark returns, 
#' \eqn{n} - number of periods
#' The total arithmetic excess returns can be explained in terms of the sum 
#' of adjusted attribution effects: 
#' \deqn{R_{p} - R_{b} = \sum^{n}_{t=1}\left(Allocation_{t}+Selection_{t}+
#' Interaction_{t}\right)}
#' 
#' @aliases Carino
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
#' @param attributions  xts with attribution effects
#' @return returns a data frame with original attribution effects and total 
#' attribution effects over multiple periods
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Frongello}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E. 
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 19 \cr Bacon, C. \emph{Practical Portfolio Performance 
#' Measurement and Attribution}. Wiley. 2004. p. 191-193 \cr Carino, D. (1999) 
#' \emph{Combining attribution effects over time}. The Journal of Performance 
#' Measurement. Summer. p. 5-14 \cr
#' @keywords arithmetic attribution, Carino linking, logarithmic linking
#' @examples
#' 
#' data(attrib)
#' Carino(rp = attrib.returns[, 21], rb = attrib.returns[, 22], attributions = attrib.allocation, adjusted = FALSE)
#' 
#' @export
Carino <- 
function(rp, rb, attributions, adjusted)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # logarithmic linking. Used internally by the Attribution function
  
    # Inputs:
    # rp            xts of portfolio returns
    # rb            xts of benchmark returns
    # attributions  attribution effects (allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
  
    # FUNCTION:
    rpc = prod(1 + rp) - 1     # Cumulative returns          
    rbc = prod(1 + rb) - 1
    k = (log(1 + rpc) - log(1 + rbc)) / (rpc - rbc)
    kt = rp
    for (t in 1:nrow(kt)){
      if (rp[[t]] == rb[[t]]){
        # Carino factors if portfolio and benchmarkreturns are equal
        kt[[t]] = 1 / (1 + rp[[t]]) 
      } else{
        # if different
        kt[[t]] = (log(1 + rp[[t]]) - log(1 + rb[[t]])) / (rp[[t]] - rb[[t]])
      }
    }
    kt = matrix(rep(kt, ncol(attributions)), nrow(attributions), 
                ncol(attributions), byrow = FALSE)
    adj = attributions * kt / k 
    total = colSums(adj)
    if (adjusted == FALSE){
      attributions = rbind(as.data.frame(attributions), total)
    } else{
      attributions = rbind(as.data.frame(adj), total)
    }
    rownames(attributions)[nrow(attributions)] = "Total"
    
    return(attributions)
}