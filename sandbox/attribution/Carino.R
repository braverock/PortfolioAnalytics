#' calculates total attribution effects using logarithmic linking 
#' 
#' Calculates total attribution effects over multiple periods using 
#' logarithmic linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses logarithmic smoothing to adjust attribution effects 
#' so that they can be summed up over multiple periods. Attribution effect 
#' are multiplied by the adjustment factor: 
#' \deqn{A_{t}' = A_{t} \times \frac{k_{t}}{k}},
#' where \deqn{k_{t} = \frac{log(1 + r_{t}) - log(1 + b_{t})}{r_{t} - b_{t}}}, 
#' \deqn{k = \frac{log(1 + r) - log(1 + b)}{r - b}}. In case if portfolio and 
#' benchmark returns are equal \deqn{k_{t} = \frac{1}{1 + r_{t}}. 
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
#' @aliases Carino
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
#' @param attributions  xts with attribution effects
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Frongello}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009. 
#' Chapter 19
#' 
#' Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 191-193
#' 
#' Carino, D. (1999) \emph{Combining attribution effects over time}.
#' The Journal of Performance Measurement. Summer, 5–14.
#' @keywords arithmetic attribution, Carino linking, logarithmic linking
#' @examples
#' 
#' data(attrib)
#' Carino(rp, rb, allocation)
#' 
#' @export
Carino <- 
function(rp, rb, attributions)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # logarithmic linking. Used internally by the Attribution function
  
    # Inputs:
    # rp            xts of portfolio returns
    # rb            xts of benchmark returns
    # attributions  attribution effects (e.g. allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
  
    # FUNCTION:
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