#' calculates total attribution effects using Frongello smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Frongello linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses Frongello smoothing algorithm to adjust
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
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period
#' @return returns a data frame with original attribution effects and total 
#' attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Carino}} \cr
#' \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 199-201
#' 
#' Frongello, A. (2002) \emph{Linking single period attribution results}.
#' Journal of Performance Measurement. Spring, 10–22.
#' @keywords arithmetic attribution, Frongello linking
#' @examples
#' 
#' data(attrib)
#' Frongello(rp, rb, allocation, adjusted = FALSE)
#' 
#' @export
Frongello <-
function(rp, rb, attributions, adjusted)
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
    adj = attributions
    if (nrow(rp) > 1){
        adj[2, ] = coredata(adj[2, ]) * drop((1 + rp[1, 1])) + drop(rb[2, 1]) * coredata(adj[1, ])
    }
    if (nrow(rp) > 2){
        for(i in 3:nrow(rp)){
            adj[i, ] = coredata(adj[i, ]) * drop(prod(1 + rp[1:(i-1), 1])) + drop(rb[i, ]) * coredata(colSums(adj[1:(i-1), ]))
        }
    }
    total = colSums(adj)
    if (adjusted == FALSE){
      attributions = rbind(as.data.frame(attributions), total)
    } else{
      attributions = rbind(as.data.frame(adj), total)
    }
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}
