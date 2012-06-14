#' calculates total attribution effects using GRAP linking 
#' 
#' Calculates total attribution effects over multiple periods using 
#' logarithmic linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses GRAP smoothing algorithms to adjust
#' attribution effects so that they can be summed up over multiple periods
#' Attribution effect are multiplied by the adjustment factor 
#' \deqn{A_{t}' = A_{t} \times G_{t}}, where 
#' \deqn{G_{t}=\overset{t-1}{\underset{i=1}{\prod}}(1+r_{i})\times\overset{n}{\underset{i=t+1}{\prod}}(1+b_{i})}
#' \deqn{A_{t}}' - adjusted attribution effects at period \deqn{t}
#' \deqn{A_{t}} - unadjusted attribution effects at period \deqn{t}
#' \deqn{r_{i}} - portfolio returns at period \deqn{i}
#' \deqn{b_{i}} - benchmark returns at period \deqn{i}
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{n} - number of periods
#' The total arithmetic excess returns can be explained in terms of the sum 
#' of adjusted attribution effects: 
#' \deqn{r - b = \overset{n}{\underset{t=1}{\sum}}\left(Allocation_{t}+Selection_{t}+Interaction_{t}\right)}
#'
#'
#' @aliases Grap
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
#' @param attributions  xts with attribution effects
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Carino}} \cr \code{\link{Frongello}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 196-199
#' @keywords attribution, GRAP linking
#' @examples
#' 
#' data(attrib)
#' Grap(rp, rb, allocation)
#' 
#' @export
Grap <-
function(rp, rb, attributions)
{   # @author Andrii Babii
  
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # GRAP linking. Used internally by the Attribution function
  
    # Inputs:
    # rp            xts of portfolio returns
    # rb            xts of benchmark returns
    # attributions  attribution effects (e.g. allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
  
    # FUNCTION:
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