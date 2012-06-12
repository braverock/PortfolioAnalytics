#' Modigliani-Modigliani measure
#' 
#' The Modigliani-Modigliani measure is the portfolio return 
#' adjusted upward or downward to match the benchmark's standard 
#' deviation.  This puts the portfolio return and the benchmark
#' return on 'equal footing' from a standard deviation perspective.
#' 
#' This is also analogous to some approaches to 'risk parity' 
#' portfolios, which use (presumably costless) leverage
#' to increase the portfolio standard deviation to some target.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthrough parameters 
#' @author Andrii Babii, Brian G. Peterson
#' @references  J. Christopherson, D. Carino, W. Ferson. 
#' Portfolio Performance Measurement and Benchmarking. 2009. McGraw-Hill, 97-99.
#' @seealso \code{\link{SharpeRatio}}, \code{\link{TreynorRatio}}
#' @examples
#' 
#' data(managers)
#' round(Modigliani(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12),4)
#' round(Modigliani(managers[,1:6], managers[,8,drop=FALSE], Rf=.035/12),4)
#' 
#' @export
Modigliani<-function (Ra, Rb, Rf=0, ...)
{
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
        Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    mm<-function(Ra, Rb, Rf){
        shr = SharpeRatio(Ra, Rf, FUN = "StdDev")
        MM = shr*StdDev(Rb)+Rf
        return(MM)
    }
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) mm(Ra[, 
        n[1]], Rb[, n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)
    if (length(result) == 1) 
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Modigliani-Modigliani measure:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}
