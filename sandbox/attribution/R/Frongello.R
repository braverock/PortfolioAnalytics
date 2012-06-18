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
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
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
#' Frongello(rp, rb, allocation)
#' 
#' @export
Frongello <-
function(rp, rb, attributions)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Frongello linking. Used internally by the Attribution function
  
    # Inputs:
    # rp xts of portfolio returns
    # rb xts of benchmark returns
    # attributions  attribution effects (e.g. allocation, selection, interaction)
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
    
    # FUNCTION:
    attr = attributions
    if (nrow(rp) > 1){
        attr[2, ] = attr[2, ] * (1 + rp[1, 1]) + rb[2, 1] * attr[1, ]
    }
    if (nrow(rp) > 2){
        for(i in 3:nrow(rp)){
            attr[i, ] = attr[i, ] * prod(1 + rp[1:(i-1), 1]) + rb[i, ] * colSums(attr[1:(i-1), ])
        }
    }
    total = colSums(attr)
    attributions = rbind(as.data.frame(attributions), total)
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}
