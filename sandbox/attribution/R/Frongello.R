#' calculates total attribution effects using Frongello smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Frongello linking method. Used internally by the \code{\link{Attribution}}
#' function. Arithmetic attribution effects do not naturally link over time. 
#' This function uses Frongello smoothing algorithm to adjust
#' attribution effects so that they can be summed up over multiple periods
#' Adjusted attribution effect at period t are:
#' \deqn{A_{t}' = A_{t}\times\prod^{t-1}_{i=1}(1+r_{pi})+R_{bt}
#' \times\sum^{t-1}_{i=1}A_{i}'}
#' \eqn{A_{t}'}{At'} - adjusted attribution effects at period \eqn{t}, 
#' \eqn{A_{t}}{At} - unadjusted attribution effects at period \eqn{t}, 
#' \eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i},
#' \eqn{R_{bi}}{Rbi} - benchmark returns at period, 
#' \eqn{R_{p}}{Rp} - total portfolio returns,
#' \eqn{R_{b}}{Rb} - total benchmark returns, 
#' \eqn{n} - number of periods
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
#' Attribution}. Wiley. 2004. p. 199-201 \cr Frongello, A. (2002) \emph{Linking
#' single period attribution results}. Journal of Performance Measurement. 
#' Spring, p. 10-22. \cr
#' @keywords arithmetic attribution, Frongello linking
#' @examples
#' 
#' data(attrib)
#' Frongello(rp = attrib.returns[, 21], rb = attrib.returns[, 22], attributions = attrib.allocation, adjusted = FALSE)
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
      adj[2, ] = coredata(adj[2, ]) * drop((1 + rp[1, 1])) + drop(rb[2, 1]) * 
        coredata(adj[1, ])
    }
    if (nrow(rp) > 2){
      for(i in 3:nrow(rp)){
        adj[i, ] = coredata(adj[i, ]) * drop(prod(1 + rp[1:(i-1), 1])) + 
          drop(rb[i, ]) * coredata(colSums(adj[1:(i-1), ]))
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
