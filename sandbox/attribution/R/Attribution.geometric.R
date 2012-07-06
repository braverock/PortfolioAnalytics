#' performs geometric attribution 
#' 
#' Performance attribution of geometric excess returns. Calculates total 
#' geometric attribution effects over multiple periods. Used internally by the
#' \code{\link{Attribution}} function. Geometric attribution effects in the
#' contrast with arithmetic do naturally link over time multiplicatively:
#' \deqn{\frac{(1+r)}{1+b}-1=\overset{n}{\underset{t=1}{\prod}}(1+A_{t}^{G})\times\overset{n}{\underset{t=1}{\prod}}(1+S{}_{t}^{G})-1}
#' 
#' where
#' \deqn{A_{t}^{G}} - total allocation effect at time t
#' \deqn{S_{t}^{G}} - total selection effect at time t
#' \deqn{A_{t}^{G}=\frac{1+b_{S}}{1+b_{t}}-1}
#' \deqn{S_{t}^{G}=\frac{1+r_{t}}{1+b_{S}}-1}
#' \deqn{b_{S}=\overset{n}{\underset{i=1}{\sum}}wp_{i}\times rb_{i}}
#' \deqn{b_{S}} - semi-notional fund
#' \deqn{w_{pt}} - portfolio weights at time t
#' \deqn{w_{bt}} - benchmark weights at time t
#' \deqn{r_{t}} - portfolio returns at time t
#' \deqn{b_{t}} - benchmark returns at time t
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{n} - number of periods
#' 
#' The multi-currency geometric attribution is handled following the Appendix A
#' (Bacon, 2004). 
#' 
#' The individual selection effects are computed using:
#' \deqn{w_{pi}\times\left(\frac{1+R_{pLi}}{1+R_{bLi}}-1\right)\times\left(\frac{1+R_{bLi}}{1+b_{SL}}\right)}
#' 
#' The individual allocation effects are computed using:
#' \deqn{(w_{pi}-w_{bi})\times\left(\frac{1+R_{bHi}}{1+b_{L}}-1\right)}
#' 
#' where
#' \deqn{b_{SH} = \underset{i}{\sum}((w_{pi} - w_{bi})R_{bHi} + w_{bi}R_{bLi})}
#' - total semi-notional return hedged into the base currency 
#' 
#' \deqn{b_{SL} = \underset{i}{\sum}w_{pi}R_{bLi}} - total semi-notional return
#' in the local currency
#' \deqn{w_{pi}} - portfolio weights of asset i
#' \deqn{w_{bi}} - benchmark weights of asset i
#' \deqn{R_{pLi}} - portfolio returns in the local currency
#' \deqn{R_{bLi}}} - benchmark returns in the local currency
#' \deqn{R_{bHi}} - benchmark returns hedged into the base currency
#' \deqn{b_{L}} - total benchmark returns in the local currency
#' \deqn{r_{L}} - total portfolio returns in the local currency
#' The total excess returns are decomposed into:
#' \deqn{\frac{(1+r)}{1+b}-1=\frac{1+r_{L}}{1+b_{SL}}\times\frac{1+b_{SH}}{1+b_{L}}\times\frac{1+b_{SL}}{1+b_{SH}}\times\frac{1+r}{1+r_{L}}\times\frac{1+b_{L}}{1+b}-1}
#' 
#' where the first term corresponds to the selection, second to the allocation,
#' third to the hedging cost transferred and the last two to the naive currency
#' attribution
#' 
#' @aliases Attribution.geometric
#' @param Rp xts of portfolio returns
#' @param wp xts of portfolio weights
#' @param Rb xts of benchmark returns
#' @param wb xts of benchmark weights
#' @param Rpl xts, data frame or matrix of portfolio returns in local currency
#' @param Rbl xts, data frame or matrix of benchmark returns in local currency
#' @param Rbh xts, data frame or matrix of benchmark returns hedged into the
#' base currency
#' @return This function returns the list with attribution effects (allocation
#' or selection effect) including total multi-period attribution effects
#' @author Andrii Babii
#' @seealso  \code{\link{Attribution}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 18-19
#' 
#' Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 5, 8, Appendix A
#' @keywords attribution, geometric attribution, geometric linking
#' @examples
#' 
#' data(attrib)
#' Attribution.geometric(Rp, wp, Rb, wb)
#' 
#' @export
Attribution.geometric <-
function(Rp, wp, Rb, wb, Rpl, Rbl, Rbh, currency = FALSE)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
  
    # Inputs:
    # Rp       xts of portfolio returns
    # wp       xts of portfolio weights
    # Rb       xts of benchmark returns
    # wb       xts of benchmark weights
  
    # Outputs: 
    # This function returns the list with attribution effects (allocation or
    # selection effect) including total multi-period  attribution effects
  
    # FUNCTION:
    WP = wp # Save original weights in order to avoid double conversion later
    WB = wb
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    
    # Get total portfolio returns
    if (is.vector(WP)  & is.vector(WB)){
      rp = Return.portfolio(Rp, WP)
      rb = Return.portfolio(Rb, WB)
    } else{
      rp = Return.rebalancing(Rp, WP)
      rb = Return.rebalancing(Rb, WB)
    }
    names(rp) = "Total"                    
    names(rb) = "Total"
    
    bs = reclass(rowSums((wp * coredata(Rb[, 1:ncol(wp)]))), rp) # Allocation notional fund returns
    if (!currency){
      allocation = ((1 + Rb) / (1 + rep(rb, ncol(Rp))) - 1) * coredata(wp - wb) # Geometric attribution effects for individual categories
      selection = wp * (Rp - coredata(Rb)) / (1 + rep(bs, ncol(Rp)))

    } else{
      Rpl = checkData(Rpl)
      Rbl = checkData(Rbl)
      Rbh = checkData(Rbh)
      
      bsl = reclass(rowSums(Rbl * wp), Rpl)
      bsh = reclass(rowSums(((wp - wb) * Rbh + wb * Rbl)), Rpl)
      rpl = reclass(rowSums(Rpl * wp), Rpl)
      rbl = reclass(rowSums(Rbl * wp), Rpl)
      allocation = (wp - wb) * ((1 + Rbh) / (1 + rep(rbl, ncol(Rbh))) - 1)
      selection = wp * ((1 + Rpl) / (1 + Rbl) - 1) * ((1 + Rbl) / (1 + rep(bsl, ncol(Rbl))))
      hedge = (1 + bsl) / (1 + bsh) - 1
      currency.attr = (1 + rp) * (1 + rbl) / (1 + rpl) / (1 + rb) - 1
      curr = cbind(hedge, currency.attr)
      colnames(curr) = c("Hedging", "Currency attribution")
    }
    
    # Total attribution effects are computed as a sum of individual effects
    allocation = cbind(allocation, rowSums(allocation)) 
    selection = cbind(selection, rowSums(selection))
    colnames(allocation)[ncol(allocation)] = "Total"
    colnames(selection)[ncol(selection)] = "Total"
    
    # Link single-period attribution effects
    a = (apply(1 + allocation[, ncol(allocation)], 2, prod) - 1)
    s = (apply(1 + selection[, ncol(selection)], 2, prod) - 1)
    allocation = rbind(as.data.frame(allocation), c(rep(NA, ncol(allocation) - 1), a))
    selection = rbind(as.data.frame(selection), c(rep(NA, ncol(selection) - 1), s))
    rownames(allocation)[nrow(allocation)] = "Total"
    rownames(selection)[nrow(selection)] = "Total"
    
    # Geometric excess returns + annualized geometric excess returns
    excess.returns = (1 + rp) / (1 + coredata(rb)) - 1
    if (nrow(rp) > 1){
      er = Return.annualized.excess(rp, rb)
      excess.returns = rbind(as.matrix(excess.returns), er)
    }
    colnames(excess.returns) = "Geometric"
    
    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    if (!currency){
      names(result) = c("Excess returns", "Allocation", "Selection")
    } else{
      result[[4]] = curr
      names(result) = c("Excess returns", "Allocation", "Selection", "Currency management")
    }
    
    return(result)
}