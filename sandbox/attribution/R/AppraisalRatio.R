#' Apraisal ratio = alpha / residual return
#' 
#' The alpha from CAPM divided by the standard deviation of residuals
#' 
#' \deqn{A_{p}=\frac{\alpha_{p}}{\sigma(\varepsilon_{p})}}{Ap = alphap / 
#' sigmap(epsilonp)}
#' 
#' Is appropriate for evaluating security selection ability. It captures the 
#' idea that an active portfolio manager has t odepart from the benchmark, that
#' is, take on residual risk, in order to produce alpha. The more alpha the 
#' investor produces for a given amout of residual risk, the higher the ratio.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' the asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' the benchmark asset return
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthrough parameters
#' @author Andrii Babii
#' @seealso \code{\link{InformationRatio}}
#' @references J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill, p. 101. \cr
#' Jack Treynor and Fischer Black, "How t oUse Security Analysis to Improve 
#' Portfolio Selection," \emph{Journal of Business}, vol.46, no.1, 
#' January 1973, pp. 66-86. \cr
#' 
#' @examples
#' 
#' data(managers)
#' AppraisalRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12)
#' AppraisalRatio(managers[80:120,1:6], managers[80:120,7,drop=FALSE], managers[80:120,10,drop=FALSE])
#' AppraisalRatio(managers[80:120,1:6], managers[80:120,8:7], managers[80:120,10,drop=FALSE])
#' 
#' @export
AppraisalRatio <- function (Ra, Rb, Rf = 0, ...)
{ # @author Andrii Babii
    
    # DESCRIPTION
    # AppraisalRatio = alpha / residual return
  
    # FUNCTION
  
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
      Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    
    ar <- function (Ra, Rb, Rf)
    {
      alpha = CAPM.alpha(Ra, Rb, Rf)
      beta = CAPM.beta(Ra, Rb, Rf)
      epsilon = Ra - Rf - rep(alpha, nrow(Ra)) - beta %x% Rb
      sigma = diag(t(epsilon) %*% epsilon) / (length(managers) - 2)
      AR = alpha / sigma
      return(AR)
    }
      
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) 
      ar(Ra[, n[1]], Rb[, n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)
    
    if(length(result) == 1)
      return(result)
    else {
      result = matrix(result, ncol = Ra.ncols, nrow = Rb.ncols, byrow = TRUE)
      rownames(result) = paste("Appraisal Ratio:", colnames(Rb))
      colnames(result) = colnames(Ra)
      return(result)
    }
}
