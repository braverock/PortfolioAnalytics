#' @import xts
#' @import zoo
#' @import ROI.plugin.symphony
#' @import GenSA
#' @import mco
#' @import PerformanceAnalytics
#' @import foreach
#' @importFrom pso        lines plot points
#' @importFrom graphics   abline axis barplot box hist image layout lines
#'                        mtext par plot points segments strwidth text title
#' @importFrom grDevices  colorRamp heat.colors rgb                      
#' @importFrom stats      cor cov median na.omit optimize prcomp predict 
#'                        qnorm quantile rnorm runif sd var
#' @importFrom utils      head sessionInfo
#' @importFrom methods    hasArg
NULL

"_PACKAGE"

globalVariables(c('filter_constraint'))
