#' Treynor-Mazuy market timing model
#' 
#' The Treynor-Mazuy model is essentially a quadratic extension of the basic
#' CAPM. It is estimated using a multiple regression. The second term in the
#' regression is the value of excess return squared. If the gamma coefficient
#' in the regression is positive, then the estimated equation describes a 
#' convex upward-sloping regression "line". The quadratic regression is:
#' \deqn{R_{p}-R_{f}=\alpha+\beta (R_{b} - R_{f})+\gamma (R_{b}-R_{f})^2+
#' \varepsilon_{p}}{Rp - Rf = alpha + beta(Rb -Rf) + gamma(Rb - Rf)^2 + 
#' epsilonp}
#' \eqn{\gamma}{gamma} is a measure of the curvature of the regression line.
#' If \eqn{\gamma}{gamma} is positive, this would indicate that the manager's
#' investment strategy demonstrates market timing ability.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' the asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' the benchmark asset return
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthrough parameters
#' @author Andrii Babii
#' @seealso \code{\link{CAPM.beta}}, \code{\link{MertonHendriksson}}
#' @references J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill, p. 129-133.
#' \cr J. L. Treynor and K. Mazuy, "Can Mutual Funds Outguess the Market?" 
#' \emph{Harvard Business Review}, vol44, 1966, pp. 131-136 \cr
#' @examples
#' 
#' data(managers)
#' TreynorMazuy(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12)
#' TreynorMazuy(managers[80:120,1:6], managers[80:120,7,drop=FALSE], managers[80:120,10,drop=FALSE])
#' TreynorMazuy(managers[80:120,1:6], managers[80:120,8:7], managers[80:120,10,drop=FALSE])
#'
#' @export
TreynorMazuy <- function (Ra, Rb, Rf = 0, ...)
{ # @author Andrii Babii
  
    # FUNCTION
  
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    
    tm <- function (Ra, Rb, Rf)
    {
      y = Ra - Rf
      X = cbind(rep(1, length(Ra)), Rb - Rf, (Rb - Rf)^2)
      bhat = solve(t(X) %*% X) %*% t(X) %*% y
      return(bhat[3])
    }
    
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) 
      tm(Ra[, n[1]], Rb[, n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)
    
    if(length(result) == 1)
      return(result)
    else {
      result = matrix(result, ncol = Ra.ncols, nrow = Rb.ncols, byrow = TRUE)
      rownames(result) = paste("Gamma:", colnames(Rb))
      colnames(result) = colnames(Ra)
      return(result)
    }
}