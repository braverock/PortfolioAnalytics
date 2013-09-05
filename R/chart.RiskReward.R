

#' classic risk reward scatter
#' 
#' \code{neighbors} may be specified in three ways.  
#' The first is as a single number of neighbors.  This will extract the \code{neighbors} closest 
#' portfolios in terms of the \code{out} numerical statistic.
#' The second method consists of a numeric vector for \code{neighbors}.
#' This will extract the \code{neighbors} with portfolio index numbers that correspond to the vector contents.
#' The third method for specifying \code{neighbors} is to pass in a matrix.  
#' This matrix should look like the output of \code{\link{extractStats}}, and should contain
#' \code{risk.col},\code{return.col}, and weights columns all properly named. 
#' 
#' @param object optimal portfolio created by \code{\link{optimize.portfolio}}
#' @param neighbors set of 'neighbor' portfolios to overplot, see Details
#' @param \dots any other passthru parameters 
#' @param return.col string matching the objective of a 'return' objective, on vertical axis
#' @param risk.col string matching the objective of a 'risk' objective, on horizontal axis
#' @param chart.assets TRUE/FALSE. Includes a risk reward scatter of the assets in the chart
#' @param element.color color for the default plot scatter points
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}
#' @param xlim set the x-axis limit, same as in \code{\link{plot}}
#' @param ylim set the y-axis limit, same as in \code{\link{plot}}
#' @seealso \code{\link{optimize.portfolio}}
#' @rdname chart.RiskReward
#' @aliases chart.RiskReward.optimize.portfolio.DEoptim chart.RiskReward.optimize.portfolio.RP 
#' chart.RiskReward.optimize.portfolio.ROI chart.RiskReward.optimize.portfolio.pso 
#' chart.RiskReward.optimize.portfolio.GenSA
#' @export
chart.RiskReward <- function(object, neighbors, ..., return.col, risk.col, chart.assets, element.color, cex.axis, xlim, ylim){
  UseMethod("chart.RiskReward")
}

