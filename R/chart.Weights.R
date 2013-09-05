
#' boxplot of the weights of the optimal portfolios
#' 
#' Chart the optimal weights and upper and lower bounds on weights of a portfolio run via \code{\link{optimize.portfolio}}
#' 
#' @param object optimal portfolio object created by \code{\link{optimize.portfolio}}
#' @param neighbors set of 'neighbor' portfolios to overplot
#' @param \dots any other passthru parameters 
#' @param main an overall title for the plot: see \code{\link{title}}
#' @param las numeric in \{0,1,2,3\}; the style of axis labels
#'       \describe{
#'         \item{0:}{always parallel to the axis [\emph{default}],}
#'         \item{1:}{always horizontal,}
#'         \item{2:}{always perpendicular to the axis,}
#'         \item{3:}{always vertical.}
#'       }
#' @param xlab a title for the x axis: see \code{\link{title}}
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of \code{cex}
#' @param element.color color for the default plot lines
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @seealso \code{\link{optimize.portfolio}}
#' @rdname chart.Weights
#' @name chart.Weights
#' @aliases chart.Weights.optimize.portfolio.ROI chart.Weights.optimize.portfolio.DEoptim chart.Weights.optimize.portfolio.pso chart.Weights.optimize.portfolio.RP chart.Weights.optimize.portfolio.GenSA
#' @export
chart.Weights <- function(object, neighbors = NULL, ..., main="Weights", las = 3, xlab=NULL, cex.lab = 1, element.color = "darkgray", cex.axis=0.8){
  UseMethod("chart.Weights")
}

