#' performs fixed income attribution
#' 
#' Performs fixed income attribution. The investment decision process for bond
#' managers is very different from that of equity managers, therefore for most
#' fixed income investment strategies the standard Brinson model is not 
#' suitable. Bonds are simply a series of defined future cash flows which are 
#' relatively easy to price. Fixed income performance is therefore driven by 
#' changes in the shape of the yield curve. Systematic risk in the form of 
#' duration is a key part of the investment process. Fixed income attribution
#' is, in fact, a specialist form of risk-adjusted attribution.
#' The arithmetic attribution is handled using weighted duration approach
#' (Van Breukelen, 2000). The allocation, selection and currency allocation 
#' effects for category i are:
#' \deqn{A_{i} = (D_{pi}\times w_{i}-D_{\beta}\times D_{bi}\times w_{pi})\times (-\Delta y_{bi} + \Delta y_{b})}
#' \deqn{S_{i} = D_{i}\times w_{i}\times (-\Delta y_{ri} + \Delta y_{bi})}
#' \deqn{C_{i} = (w_{pi} - w_{bi})\times (c_{i} + R_{fi} - c')}
#' where
#' \deqn{w_{pi}} - portfolio weights
#' \deqn{w_{bi}} - benchmark weights
#' \deqn{D_{i}} - modified duration in bond category i
#' \deqn{D_{\beta}=\frac{D_{r}}{D_{b}}} - duration beta
#' \deqn{D_{r}} - portfolio duration
#' \deqn{D_{b}} - benchmark duration
#' \deqn{D_{bi}} - benchmark duration for category i
#' \deqn{D_{pi}} - portfolio duration for category i
#' \deqn{\Delta y_{ri}} - change in portfolio yield for category i
#' \deqn{\Delta y_{bi}} - change in benchmark yield for category i
#' \deqn{\Delta y_{b}} - change in benchmark yield
#' \deqn{R_{ci} - currency returns for category i
#' \deqn{R_{fi}} - risk-free rate in currency of asset i
#' \deqn{c'= \underset{i}{\sum}w_{bi}\times(R_{ci}+R_{fi})}
#' The geometric attribution is adapted using Van Breukelen (2000) approach for
#' the arithmetic attribution. The individual allocation and selection effects
#' are computed as follows:
#' \deqn{A_{i}=D_{i}w_{pi}-D_{\beta}D_{bi}w_{bi}}
#' \deqn{S_{i}=\frac{D_{pi}}{D_{bi}}\times (R_{bi} - R_{fi}) + R_{fi}}
#' @aliases fixed income attribution
#' @param Rp T x n xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb T x n xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param Rf T x n xts, data frame or matrix with risk free rates
#' @param Dp T x n xts, data frame or matrix with portfolio modified duration
#' @param Db T x n xts, data frame or matrix with benchmark modified duration
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @param S (T + 1) x n xts, data frame or matrix with spot rates. The first date
#' should coincide with the first date of portfolio returns
#' @param geometric - TRUE/FALSE for geometric/arithmetic attribution
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @return list with total excess returns decomposed into allocation, selection 
#' (and currency effects)
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.levels}}, \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 7
#' 
#' Van Breukelen, G. \emph{Fixed income attribution}. Journal of Performance 
#' Measurement. Summer, 61–68. 2000
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' AttributionFixedIncome(Rp, wp, Rb, wb, Rf, Dp, Db, S, wbf, geometric = FALSE)
#' 
#' @export
AttributionFixedIncome <- 
function (Rp, wp, Rb, wb, Rf, Dp, Db, S, wbf, geometric = FALSE)
{   # @author Andrii Babii
    
    # DESCRIPTION:
    # Function to perform fixed income attribution
    
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # Rf       xts, data frame or matrix of risk-free rate
    # Dp       T x n xts, data frame or matrix with portfolio modified duration
    # Db       T x n xts, data frame or matrix with benchmark modified duration
    # S        xts, data frame or matrix with spot rates
    # wbf      vector, xts, data frame or matrix with benchmark weights of 
    #          currency forward contracts
    
    # Outputs: 
    # This function returns the 
    
    # FUNCTION:
    Rf = checkData(Rf)
    Rp = checkData(Rp)
    Rb = checkData(Rb)
    Dp = checkData(Dp)
    Db = checkData(Db)
    S = checkData(S)
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    wbf = Weight.transform(wbf, Rb)
    
    rp = reclass(rowSums(Rp * wp), Rp)
    rb = reclass(rowSums(Rb * wb), wb)
    rf = reclass(rowSums(Rf * wp), Rf)
    dp = reclass(rowSums(Dp * wp), Dp) # Portfolio duration
    db = reclass(rowSums(Db * wp), Db) # Benchmark duration
    Dbeta = dp / db
    DeltaYb = -(Rb - Rf) / Db # Implied benchmark yield changes
    DeltaYp = -(Rp - Rf) / Dp # Implied portfolio yield changes
    deltayb = rep(-(rp - rb), ncol(Dp)) / Dp # Implied total benchmark yield changes
    Rc = lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1 # Currency returns
    rc = reclass(rowSums((wb + wbf) * (Rc + Rf)), Rc)
    if (!geometric){
        allocation = (Dp * wp - rep(Dbeta, ncol(Dp)) * Db * wb) * (-DeltaYb + deltayb)
        selection = Dp * wp * (-DeltaYp + DeltaYb)
        currency = (wp - wb) * (Rc + Rf - rep(rc, ncol(Rc)))
        excess.returns = rp - rb
    } else{
        rcprime = rowSums(wb * (Rc + Rf))
        bd = reclass(rowSums(rep(Dbeta, ncol(Db)) * Db * wb * -DeltaYb), Db) + rcprime # Overal duration notional fund
        allocation = Dp * wp - rep(Dbeta, ncol(Dp)) * Db * wb * (-DeltaYb + deltayb) / rep(bd, ncol(Db))
        selection = Dp / Db * (Rb - Rf) + Rf
        excess.returns = (1 + rp) / (1 + rb) - 1
    }
    
    # Get total attribution effects 
    n = ncol(allocation)               # number of segments
    allocation = cbind(allocation, rowSums(allocation))
    names(allocation)[n + 1] = "Total"  
    selection = cbind(selection, rowSums(selection))
    names(selection)[n + 1] = "Total"   

    result = list()
    result[[1]] = excess.returns
    result[[2]] = allocation
    result[[3]] = selection
    names(result) = c("Excess returns", "Market allocation", "Issue selection")
    
    if (!geometric){
        currency = cbind(currency, rowSums(currency))
        names(currency)[ncol(currency)] = "Total"
        result[[4]] = currency
        names(result) = c("Excess returns", "Market allocation", "Issue selection", "Currency allocation")
    }
    return(result)
}