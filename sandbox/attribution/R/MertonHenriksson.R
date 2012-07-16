#' Merton-Henriksson market timing model
#' 
#' Test of market-timing skill (the ability to profitably move from one asset
#' class to another) developed by Henriksson and Merton. The basic idea of the
#' test is to perform a multiple regression in which the dependent variable 
#' (portfolio excess return and a second variable that mimics the payoff to an 
#' option). This second variable is zero when the market excess return is at or
#' below zero and is 1 when it is above zero:
#' \deqn{R_{p}-R_{f}=\alpha+\beta (R_{b}-R_{f})+\gamma D+\varepsilon_{p}}{Rp - 
#' Rf = alpha + beta * (Rb - Rf) + gamma * D + epsilonp}
#' where all variables are familiar from the CAPM model, except for up-market 
#' return \eqn{D=max(0,R_{b}-R_{f}){D = max(0, Rb - Rf)} and market timing 
#' abilities \eqn{\gamma}{gamma}
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' the asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' the benchmark asset return
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthrough parameters
#' @author Andrii Babii
#' @seealso \code{\link{CAPM.beta}}, \code{\link{TreynorMazuy}}
#' @references J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill, p. 127-129.
#' \cr Roy D. Henriksson and Robert C. Merton, "On Market Timing and Investment
#' Performance. II. Statistical Procedures for Evaluating Forecast Skills," 
#' \emph{Journal of Business}, vol.54, October 1981, pp.513-533 \cr
#' @examples
#' 
#' data(managers)
#' MertonHenriksson(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12)
#' MertonHenriksson(managers[80:120,1:6], managers[80:120,7,drop=FALSE], managers[80:120,10,drop=FALSE])
#' MertonHenriksson(managers[80:120,1:6], managers[80:120,8:7], managers[80:120,10,drop=FALSE])
#' 
#' @export
MertonHenriksson <- function (Ra, Rb, Rf = 0, ...)
{ # @author Andrii Babii
  
    # FUNCTION
  
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    
    mh <- function (Ra, Rb, Rf)
    {
      D = pmax(0, Rb - Rf)
      y = Ra - Rf
      X = cbind(rep(1, length(Ra)), Rb - Rf, D)
      bhat = solve(t(X) %*% X) %*% t(X) %*% y
      return(bhat[3])
    }
    
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) 
      mh(Ra[, n[1]], Rb[, n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)
    
    if(length(result) == 1)
      return(result)
    else {
      result = matrix(result, ncol = Ra.ncols, nrow = Rb.ncols, byrow = TRUE)
      rownames(result) = paste("Gamma:", colnames(Rb))
      colnames(result) = colnames(Ra)
      return(result)
    }
}