#' aggregate portfolio returns and weights up to the chosen level from the hierarchy
#' 
#' Aggregate returns and weights up to the chosen level from the hierarchy. 
#' Hierarchy can be used from the buildHierarchy function or 
#' defined manually in the same way as the buildHierarchy's output
#'
#' @aliases Weight.transform Return.level Weight.level
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param h  data.frame with portfolio hierarchy
#' @param level level from the hierarchy to which returns and weights will be aggregated
#' @author Andrii Babii
#' @seealso  \code{\link{buildHierarchy}} \cr \code{\link{Attribution}} \cr \code{\link{Return.portfolio}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009. Chapter 17
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Weight.transform(wp, Rp)
#' Return.level(Rp, wp, h, level = "Sector")
#' Weight.level(wp, h, level = "Sector")
#' 
#' @export
Weight.transform <- 
function(wp, Rp)
{   # @author Andrii Babii
    # Function to transform weights to the xts object conformable with returns used by aggregation and attribution functions
    #
    # wp                wp vector, xts, data frame or matrix of portfolio weights
    #
    # Rp                xts, data frame or matrix of portfolio returns

    if (is.vector(wp)){
        wp = as.xts(matrix(rep(wp, nrow(Rp)), nrow(Rp), ncol(Rp), byrow = TRUE), index(Rp))
        colnames(wp) = colnames(Rp)
    } else{
        if(as.Date(last(index(Rp))) < (as.Date(index(wp[1,]))+1)){
            stop(paste('last date in series',as.Date(last(index(Rp))),'occurs before beginning of first rebalancing period',as.Date(first(index(wp)))+1))
        }
        wp = checkData(wp, method = "xts")
        wp = merge(wp, xts(, index(Rp)))
        wp = na.locf(wp)
        if(as.Date(first(index(Rp))) > (as.Date(index(wp[1,]))+1)) {
            warning(paste('data series starts on',as.Date(first(index(Rp))),', which is after the first rebalancing period',as.Date(first(index(wp)))+1)) 
            wp = wp
        } else{
            wp = wp[2:nrow(wp)]
        }
    }
    return(wp)
}

Return.level <-
function(Rp, wp, h, level = "Sector")
{   # @author Andrii Babii
    # Function to aggregate returns up to the chosen level from the hierarchy
    #
    # Rp                xts, data frame or matrix of portfolio returns
    #
    # wp                wp vector, xts, data frame or matrix of portfolio weights
    #
    # h                 data.frame with portfolio hierarchy
    #
    # level             level from the hierarchy to which the aggregation will be performed
    
    Rp = checkData(Rp, method = "xts")
    wp = Weight.transform(wp, Rp)
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
        Rp = Rp[2:nrow(Rp)]
    }

    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(i in 1:length(h)){
        returns[, i] = rowSums(Rp[, h[[i]]] * wp[, h[[i]]])
    }
    colnames(returns) = names(h)
    return(returns)
}

Weight.level <-
function(wp, h, level = "Sector")
{   # @author Andrii Babii
    # Function to aggregate weights up to the chosen level from the hierarchy
    #
    # wp                wp vector, xts, data frame or matrix of portfolio weights
    #
    # h                 data.frame with portfolio hierarchy
    #
    # level             level from the hierarchy to which the aggregation will be performed
    
    wp = Weight.transform(wp, Rp)

    h = split(h$primary_id, h[level])
    weights = wp[, 1:length(h)]
    for(i in 1:length(h)){
        weights[, i] = rowSums(wp[, h[[i]]])
    }
    colnames(weights) = names(h)
    return(weights)
}
