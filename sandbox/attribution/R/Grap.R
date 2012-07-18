#' calculates total attribution effects using GRAP smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' GRAP linking method. Used internally by the \code{\link{Attribution}} 
#' function. Arithmetic attribution effects do not naturally link over time.
#' This function uses GRAP smoothing algorithm to adjust attribution effects
#' so that they can be summed up over multiple periods
#' Attribution effect are multiplied by the adjustment factor 
#' \deqn{A_{t}' = A_{t} \times G_{t}}{At' = At * Gt} where 
#' \deqn{G_{t}=\prod^{t-1}_{i=1}(1+R_{pi})\times\prod^{n}_{t+1}(1+R_{bi})}
#' \eqn{A_{t}'}{At'} - adjusted attribution effects at period \eqn{t}, 
#' \eqn{A_{t}}{At} - unadjusted attribution effects at period \eqn{t}, 
#' \eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i},
#' \eqn{R_{bi}}{Rbi} - benchmark returns at period \eqn{i}, 
#' \eqn{R_{p}}{Rp} - total portfolio returns, 
#' \eqn{R_{b}}{Rb} - total benchmark returns, 
#' \eqn{n} - number of periods
#' The total arithmetic excess returns can be explained in terms of the sum 
#' of adjusted attribution effects: 
#' \deqn{R_{p} - R_{b} = \sum^{n}_{t=1}\left(Allocation_{t}+Selection_{t}+
#' Interaction_{t}\right)}
#'
#' @aliases Grap
#' @param rp xts of portfolio returns
#' @param rb xts of benchmark returns
#' @param attributions  xts with attribution effects
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period
#' @return returns a data frame with original attribution effects and total 
#' attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Carino}} \cr \code{\link{Frongello}} \cr
#' \code{\link{Attribution.geometric}}
#' @references   Bacon, C. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 196-199 \cr GRAP (Groupe de Recherche en 
#' Attribution de Performance) (1997) \emph{Synthese des modeles d'attribution
#' de performance}. Paris, Mars.\cr
#' @keywords arithmetic attribution, GRAP linking
#' @examples
#' 
#' data(attrib)
#' Grap(rp = attrib.returns[, 21], rb = attrib.returns[, 22], attributions = attrib.allocation, adjusted = FALSE)
#' 
#' @export
Grap <-
function(rp, rb, attributions, adjusted)
{   # @author Andrii Babii
  
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # GRAP linking. Used internally by the Attribution function
  
    # Inputs:
    # rp            xts of portfolio returns
    # rb            xts of benchmark returns
    # attributions  attribution effects (allocation, selection, interaction)
  
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
    g = matrix(rep(G, ncol(attributions)), nrow(attributions), 
               ncol(attributions), byrow = FALSE)
    adj = attributions * g
    total = colSums(adj)
    if (adjusted == FALSE){
      attributions = rbind(as.data.frame(attributions), total)
    } else{
      attributions = rbind(as.data.frame(adj), total)
    }
    rownames(attributions)[nrow(attributions)] = "Total"
    return(attributions)
}