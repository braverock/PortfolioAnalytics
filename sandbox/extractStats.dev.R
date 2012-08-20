#' extract some stats from a portfolio list run with ROI via
#' \code{\link{optimize.portfolio}}
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}
#' @export 
extractStats.optimize.portfolio.ROI <- function(object, prefix=NULL, ...) {
  
  trow<-c(out=object$out, object$weights)
  result<-trow
  
  rnames<-c('out',paste('w',names(object$weights),sep='.'))
  names(result)<-rnames
  return(result)
}


#' extract some stats from a portfolio list run with pso via
#' \code{\link{optimize.portfolio}}
#' 
#' This function will take everything in the objective_measures slot and \code{unlist} it.  
#' This may produce a very large number of columns or strange column names.
#' 
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}
#' @export 
extractStats.optimize.portfolio.pso <- function(object, prefix=NULL, ...) {
  
  trow<-c(out=object$out, object$weights)
  result<-trow
  
  rnames<-c('out',paste('w',names(object$weights),sep='.'))
  names(result)<-rnames
  return(result)
}