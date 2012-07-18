#' calculates total attribution effects using Davies and Laker smoothing 
#' 
#' Calculates total attribution effects over multiple periods using 
#' Davies and Laker linking method. Used internally by the 
#' \code{\link{Attribution}} function. Arithmetic attribution effects do not 
#' naturally link over time. This function uses Davies and Laker linking method
#' to compute total attribution effects. 
#' Arithmetic excess returns are decomposed as follows:
#' \deqn{R_{p} - R_{b} = Allocation + Selection + Interaction}{Rp - Rb = 
#' Allocation + Selection + Interaction}
#' \deqn{Allocation = \prod^{T}_{t=1}(1+bs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
#' \deqn{Selection = \prod^{T}_{t=1}(1+rs_{t})-\prod^{T}_{t=1}(1+R_{bt})}
#' \deqn{Interaction = \prod^{T}_{t=1}(1+R_{pt})-\prod^{T}_{t=1}(1+rs_{t})-
#' \prod^{T}_{t=1}(1+bs_{t})+\prod^{T}_{t=1}(1+R_{bt})}
#' \eqn{R_{pi}}{Rpi} - portfolio returns at period \eqn{i}, 
#' \eqn{R_{bi}}{Rbi} - benchmark returns at period \eqn{i},
#' \eqn{rs_{i}}{rsi} - selection notional fund returns at period \eqn{i}, 
#' \eqn{bs_{i}}{bsi} - allocation notional fund returns at period \eqn{i}, 
#' \eqn{T} - number of periods
#' 
#' @aliases DaviesLaker
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @return This function returns the data.frame with original attribution 
#' effects and total attribution effects over multiple periods
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}} \cr \code{\link{Menchero}} \cr 
#' \code{\link{Grap}} \cr \code{\link{Carino}} \cr
#' \code{\link{Attribution.geometric}} \cr \code{\link{Frongello}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 201-204 \cr Davies, O. and Laker, D. (2001) 
#' \emph{Multiple-period performance attribution using the Brinson model}. 
#' Journal of Performance Measurement. Fall. p. 12-22 \cr
#' @keywords arithmetic attribution, Davies and Laker linking
#' @examples
#' 
#' data(attrib)
#' DaviesLaker(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], wb = attrib.weights[2, ])
#' 
#' @export
DaviesLaker <-
function(Rp, wp, Rb, wb)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to provide multi-period summary of attribution effects using
    # Davies and Laker linking. Used internally by the Attribution function
  
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
  
    # Outputs: 
    # This function returns the data.frame with original attribution effects
    # and total attribution effects over multiple periods
    
    # FUNCTION:
    WP = wp
    WB = wb
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    if (is.vector(WP)  & is.vector(WB)){
      rp = Return.portfolio(Rp, WP, geometric = FALSE)
      rb = Return.portfolio(Rb, WB, geometric = FALSE)
    } else{
      rp = Return.rebalancing(Rp, WP, geometric = FALSE)
      rb = Return.rebalancing(Rb, WB, geometric = FALSE)
    }
    colnames(rp) = "Total"
    colnames(rb) = "Total"
    # Allocation notional fund returns
    bs = reclass(rowSums((wp * coredata(Rb[, 1:ncol(wp)]))), Rp) 
    # Selection notional fund returns
    rs = reclass(rowSums((wb * coredata(Rp[, 1:ncol(wb)]))), Rp) 
    a = apply(1 + bs, 2, prod) - apply(1 + rb, 2, prod)
    s = apply(1 + rs, 2, prod) - apply(1 + rb, 2, prod)
    i = apply(1 + rp, 2, prod) - apply(1 + rs, 2, prod) - 
      apply(1 + bs, 2, prod) + apply(1 + rb, 2, prod)
    
    # Compute attribution effects (Brinson, Hood and Beebower model)
    allocation = (wp - wb) * coredata(Rb)
    selection = wb * (Rp - coredata(Rb))
    interaction = (wp - wb) * (Rp - coredata(Rb))
    n = ncol(allocation)               # number of segments
    allocation = cbind(allocation, rowSums(allocation))
    names(allocation)[n + 1] = "Total"  
    selection = cbind(selection, rowSums(selection))
    names(selection)[n + 1] = "Total"   
    interaction = cbind(interaction, rowSums(interaction))
    names(interaction)[n + 1] = "Total"
    
    allocation = rbind(as.data.frame(allocation), 
                       c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), 
                      c(rep(NA, ncol(selection) - 1), s))
    interaction = rbind(as.data.frame(interaction), 
                        c(rep(NA, ncol(interaction) - 1), i))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    rownames(interaction)[nrow(allocation)] = "Total"

    # Arithmetic excess returns + annualized arithmetic excess returns
    excess.returns = rp - coredata(rb)
    if (nrow(rp) > 1){
      er = Return.annualized.excess(rp, rb, geometric = FALSE)
      excess.returns = rbind(as.matrix(excess.returns), er)
    }
    colnames(excess.returns) = "Arithmetic"
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    result[[4]] = interaction
    names(result) = c("Excess returns", "Allocation", "Selection", 
                      "Interaction")
    return(result)
}
