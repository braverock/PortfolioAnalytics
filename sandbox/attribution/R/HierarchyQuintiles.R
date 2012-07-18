#' replaces numeric values in the hierarchy by quintiles
#' 
#' Replaces numeric values in the hierarchy by text with corresponding 
#' quintiles
#'
#' @aliases HierarchyQuintiles
#' @param h  data.frame with portfolio hierarchy
#' @param level level from the hierarchy for which there are numeric values
#' @author Andrii Babii
#' @seealso  \code{buildHierarchy}
#' @keywords attribution, hierarchy
#' @examples
#' 
#' data(attrib)
#' HierarchyQuintiles(h = attrib.hierarchy, "MarketCap")
#' 
#' @export
HierarchyQuintiles <-
function(h, level)
{  # @author Andrii Babii
    h = na.omit(h)
    hnew = h[[level]]
    quintiles = quantile(h[[level]], c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
    for (i in 1:length(h[[level]])){
      if (h[[level]][i] >= quintiles[1] & h[[level]][i] < quintiles[2] 
          & !is.na(h[[level]][i])){
        hnew[i] = "Quintile 1"
      }
    if (h[[level]][i] >= quintiles[2] & h[[level]][i] < quintiles[3] 
        & !is.na(h[[level]][i])){
      hnew[i] = "Quintile 2"
    }
    if (h[[level]][i] >= quintiles[3] & h[[level]][i] < quintiles[4] 
        & !is.na(h[[level]][i])){
      hnew[i] = "Quintile 3"
    }
    if (h[[level]][i] >= quintiles[4] & h[[level]][i] < quintiles[5] 
        & !is.na(h[[level]][i])){
      hnew[i] = "Quintile 4"
    }
    if (h[[level]][i] >= quintiles[5] & h[[level]][i] <= quintiles[6] 
        & !is.na(h[[level]][i])){
      hnew[i] = "Quintile 5"
    }
    }
    h[[level]] = hnew
    return(h)
}
