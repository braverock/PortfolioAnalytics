#' calculates total attribution effects using Frongello linking 
#' 
#' Calculates total attribution effects over multiple periods using 
#' logarithmic linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses logarithmic smoothing algorithms to adjust
#' attribution effects so that they can be summed up over multiple periods
#' Adjusted attribution effect at period t are:
#' \deqn{A_{t}' = A_{t}\times\overset{t-1}{\underset{i=1}{\prod}}(1+r_{i})+b_{t}\times\overset{t-1}{\underset{i=1}{\sum}}A_{i}'}
#' \deqn{A_{t}}' - adjusted attribution effects at period \deqn{t}
#' \deqn{A_{t}} - unadjusted attribution effects at period \deqn{t}
#' \deqn{r_{i}} - portfolio returns at period \deqn{i}
#' \deqn{b_{i}} - benchmark returns at period \deqn{i}
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{n} - number of periods
#' 
#' @aliases Frongello
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @param attributions  xts with attribution effects
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Carino}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009. 
#' Chapter 19
#' 
#' Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 199-201
#' 
#' Frongello, A. (2002) \emph{Linking single period attribution results}.
#' Journal of Performance Measurement. Spring, 10–22.
#' @keywords attribution, Frongello linking
#' @examples
#' 
#' data(attrib)
#' Frongello(Rp, wp, Rb, wb, allocation)
#' 
#' @export
Frongello <-
function(Rp, wp, Rb, wb, attributions)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Frongello linking. Used internally by the Attribution function
  
    # Inputs:
    # Rp xts of portfolio returns
    # wp xts of portfolio weights
    # Rb xts of benchmark returns
    # wb xts of benchmark weights
    # attributions  attribution effects (e.g. allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
    
    # FUNCTION:
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)
    Rp = cbind(Rp, rp)
    Rb = cbind(Rb, rb)
    attr = attributions
    attr[1, ] = attributions[1, ] + Rb[1, ]
    
    for(i in 2:nrow(rp)){
        attr[i, ] = attr[i, ] * apply((1 + Rp[1:(i-1), ]), 2, prod) + Rb[i, ] * apply(attr[1:(i-1)], 2, sum)
    }
    total = colSums(attr)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}
