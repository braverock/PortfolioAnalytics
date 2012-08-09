#' fixed income attribution
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
#' effects for category \eqn{i} are:
#' \deqn{A_{i} = (D_{pi}\times w_{pi}-D_{\beta}\times D_{bi}\times w_{pi})
#' \times (-\Delta y_{bi} + \Delta y_{b})}
#' \deqn{S_{i} = D_{i}\times w_{pi}\times (-\Delta y_{ri} + \Delta y_{bi})}
#' \deqn{C_{i} = (w_{pi} - w_{bi})\times (c_{i} + R_{fi} - c')}{Ci = 
#' (wpi - wbi) * (ci + Rfi - c')}
#' where \eqn{w_{pi}}{wpi} - portfolio weights, 
#' \eqn{w_{bi}}{wbi} - benchmark weights, 
#' \eqn{D_{i}}{Di} - modified duration in bond category \eqn{i}.
#' Duration beta:
#' \deqn{D_{\beta}=\frac{D_{r}}{D_{b}}}{Dbeta = Dr / Db}
#' \eqn{D_{r}}{Dr} - portfolio duration, 
#' \eqn{D_{b}}{Db} - benchmark duration, 
#' \eqn{D_{bi}}{Dbi} - benchmark duration for category \eqn{i}, 
#' \eqn{D_{pi}}{Dpi} - portfolio duration for category \eqn{i}, 
#' \eqn{\Delta y_{ri}}{Delta yri} - change in portfolio yield 
#' for category \eqn{i},
#' \eqn{\Delta y_{bi}}{Delta ybi} - change in benchmark yield 
#' for category \eqn{i},
#' \eqn{\Delta y_{b}}{Delta yb} - change in benchmark yield,
#' \eqn{R_{ci}}{Rci} - currency returns for category \eqn{i},
#' \eqn{R_{fi}}{Rfi} - risk-free rate in currency of asset \eqn{i},
#' \deqn{c'= \sum_{i}w_{bi}\times(R_{ci}+R_{fi})}
#' The geometric attribution is adapted using Van Breukelen (2000) approach for
#' the arithmetic attribution. The individual allocation and selection effects
#' are computed as follows:
#' \deqn{A_{i}=D_{i}w_{pi}-D_{\beta}D_{bi}w_{bi}}{Ai = 
#' Di * wpi - Dbeta * Dbi * wbi}
#' \deqn{S_{i}=\frac{D_{pi}}{D_{bi}}\times (R_{bi} - R_{fi}) + R_{fi}}{Si = 
#' Dpi / Dbi * (Rbi - Rfi) + Rfi}
#' @aliases AttributionFixedIncome
#' @param Rp T x n xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb T x n xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param Rf T x n xts, data frame or matrix with risk free rates
#' @param Dp T x n xts, data frame or matrix with portfolio modified duration
#' @param Db T x n xts, data frame or matrix with benchmark modified duration
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @param S (T + 1) x n xts, data frame or matrix with spot rates. The first 
#' date should coincide with the first date of portfolio returns
#' @param geometric - TRUE/FALSE for geometric/arithmetic attribution
#' @return list with total excess returns decomposed into allocation, selection 
#' (and currency effects)
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.levels}}, 
#' \code{\link{Attribution.geometric}}
#' @references   Bacon, C. \emph{Practical Portfolio Performance Measurement 
#' and Attribution}. Wiley. 2004. Chapter 7 \cr Van Breukelen, G. \emph{Fixed 
#' income attribution}. Journal of Performance Measurement. Sumer. 
#' p. 61-68. 2000 \cr
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' AttributionFixedIncome(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], Rf = attrib.returns[, 23:32], Dp = attrib.returns[, 63:72], Db = attrib.returns[, 73:82], 
#' S = attrib.currency[, 11:20], wbf = attrib.weights[4, ], geometric = FALSE)
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
    WP = wp # Save original weights in order to avoid double conversion later
    WB = wb
    WBF = wbf
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    wbf = Weight.transform(wbf, Rb)
    if (ncol(Rb) == 1){
      Rb = matrix(rep(coredata(Rb), ncol(Rp)), nrow(Rp), ncol(Rp))
    }
    if (ncol(Rb) != ncol(Rp)){
      stop("Please use benchmark xts that has columns with benchmarks for each
            asset or one common benchmark for all assets")
    }
    if (ncol(Db) == 1){
      Db = matrix(rep(coredata(Db), ncol(Dp)), nrow(Dp), ncol(Dp))
    }
    if (ncol(Db) != ncol(Dp)){
      print("Please use benchmark xts that has columns with benchmarks for each
            asset or one common benchmark for all assets")
    }
    if (is.vector(WP)  & is.vector(WB) & is.vector(WBF)){
      rp = Return.portfolio(Rp, WP, geometric = geometric)
      rb = Return.portfolio(Rb, WB, geometric = geometric)
      rf = Return.portfolio(Rf, WP, geometric = geometric)
      dp = Return.portfolio(Dp, WP, geometric = geometric) # portfolio duration
      db = Return.portfolio(Db, WB, geometric = geometric) # benchmark duration
    } else{
      rp = Return.rebalancing(Rp, WP, geometric = geometric)
      rb = Return.rebalancing(Rb, WB, geometric = geometric)
      rf = Return.rebalancing(Rf, WP, geometric = geometric)
      dp = Return.rebalancing(Dp, WP, geometric = geometric)
      db = Return.rebalancing(Db, WB, geometric = geometric)
    }
    names(rp) = "Total"
    names(rb) = "Total"
    Dbeta = dp / coredata(db)
    # Implied benchmark yield changes
    DeltaYb = -(Rb - coredata(Rf)) / coredata(Db) 
    # Implied portfolio yield changes
    DeltaYp = -(Rp - coredata(Rf)) / coredata(Dp) 
    # Implied total benchmark yield changes
    deltayb = rep(rb - coredata(rp), ncol(Dp)) / coredata(Dp) 
    # Currency returns
    Rc = lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1 
    rc = reclass(rowSums((wb + wbf) * (Rc + coredata(Rf))), Rc)
    if (!geometric){
      allocation = (Dp * wp - rep(Dbeta, ncol(Dp)) * coredata(Db) * wb) * 
        coredata(-DeltaYb + deltayb)
      selection = Dp * coredata(wp) * coredata(-DeltaYp + coredata(DeltaYb))
      currency = (wp - wb) * (Rc + coredata(Rf) - rep(rc, ncol(Rc)))
      excess.returns = rp - coredata(rb)
    } else{
      rcprime = rowSums(wb * (Rc + Rf))
      bd = reclass(rowSums(rep(Dbeta, ncol(Db)) * Db * coredata(wb) * 
        coredata(-DeltaYb)), Db) + rcprime # Overal duration notional fund
      allocation = Dp * wp - rep(Dbeta, ncol(Dp)) * coredata(Db) * wb * 
        coredata(-DeltaYb + deltayb) / rep(bd, ncol(Db))
      selection = Dp / coredata(Db) * coredata(Rb - coredata(Rf)) + Rf
      excess.returns = (1 + rp) / (1 + coredata(rb)) - 1
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
      names(result) = c("Excess returns", "Market allocation", 
                        "Issue selection", "Currency allocation")
    }
    return(result)
}