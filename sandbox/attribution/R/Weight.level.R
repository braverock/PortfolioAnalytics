#' aggregates portfolio weights up to the chosen level from the hierarchy
#'
#' Aggregates weights up to the chosen level from the hierarchy.
#' Hierarchy can be used from the \code{buildHierarchy} function or 
#' defined manually in the same way as the \code{buildHierarchy}'s 
#' output. If for the selected level the values in the hierarchy are numeric, 
#' the aggregation of returns or weights is performed by quintiles.
#'
#' @aliases Weight.level
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param h  data.frame with portfolio hierarchy
#' @param level level from the hierarchy to which returns and weights will be 
#' aggregated
#' @author Andrii Babii
#' @seealso  \code{buildHierarchy} \cr \code{\link{Attribution}} \cr 
#' \code{\link{Return.level}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 17
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Weight.level(wp = attrib.weights[1, ], Rp = attrib.returns[, 1:10], attrib.hierarchy, level = "Sector")
#' 
#' @export
Weight.level <-
  function(wp, Rp, h, level = "Sector")
  {   # @author Andrii Babii
    
    # DESCRIPTION:
    # Function to aggregate weights up to the chosen level from the hierarchy
    
    # Inputs:
    # wp      vector, xts, data frame or matrix of portfolio weights
    # h       data.frame with portfolio hierarchy
    # level   level from the hierarchy to which the aggregation will be done
    
    # Outputs: 
    # This function returns portfolio weights at the chosen level
    
    # FUNCTION:
    # Transform data to the xts objects
    wp = Weight.transform(wp, Rp)
    
    # If level has numeric values we replace numeric values by quintiles
    if (is.numeric(h[[level]])){
      h = HierarchyQuintiles(h, level)
    }
    h = split(h$primary_id, h[level])
    weights = wp[, 1:length(h)]
    for(i in 1:length(h)){
      weights[, i] = rowSums(wp[, h[[i]]])
    }
    colnames(weights) = names(h)
    return(weights)
}