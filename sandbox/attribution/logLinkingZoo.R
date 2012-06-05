# logLinkingZoo.r
#
# purpose: function to be passed to rollApplyEZ() for carino linking over
#          user-specified attribution dates and aggregation periods
#
## authors: Eric Zivot 
## created: June 5, 2012


#' Log-linking Function For Use In rollApplyEZ
#' 
#' Function to be passed to rollApplyEZ() for carino linking over
#' user-specified attribution dates and aggregation periods
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x \code{zoo} object containing data to be aggregated.
#' @param asset.names Character vector of asset IDs
#' @param strategy.names Character vector of strategy abbreviations
#' @param component.names Character vector of component names. If
#' \code{component.type="manager"} then \code{component.names} are asset IDs;
#' if \code{component.type="strategy"} then \code{component.names} are strategy
#' abbreviations.if \code{component.type="factor"} then \code{component.names}
#' are factor IDs.
#' @param component.type character string indicating type of attribution. Valid
#' choices are \code{"manager"} for manager selection attribution;
#' \code{"strategy"} for strategy allocation attribution; \code{"factor"} for
#' factor attribution.
#' @param return.out character string indicating output from log-linking
#' function. Valid choices are \code{"linked"} for disaggregated results, and
#' \code{"linked.total"} for total results.
#' @return A numeric vector of attribution values with the same length as
#' \code{component.names} if \code{return.out="linked"} or a numeric value
#' giving total attribution if \code{return.out = "linked.total"}.
#' @author Eric Zivot.
#' @seealso \code{\link{logLinking}}
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' 
#' 
logLinking.zoo <- function(x, asset.names, strategy.names,
                           component.names, component.type = c("manager", "strategy", "factor"),
                           return.out = c("linked", "linked.total")) {
# Arguments
# x               zoo object containing data to be aggregated.
# asset.names     character vector of asset IDs
# strategy.names  character vector of strategy abbreviations
# component.names character vector of component names. If component.type="manager"
#                 then component.names are asset IDs; if component.type="strategy"
#                 then component.names are strategy abbreviations.
# component.type  character string indicating type of attribution. Valid choices
#                 are "manager" for manager selection attribution, and "strategy"
#                 for strategy allocation attribution.
# return.out      character string indicating output from log-linking function.
#                 Valid choices are "linked" for disaggregated results, and "linked.total"
#                 for total results.
# Details
# Value
# A numeric vector of attribution values with the same length as component.names
# if return.out="linked" or a numeric value giving total attribution if
# return.out = "linked.total"
  return.out = return.out[1]
  component.type = component.type[1]
  weighted.portfolio.returns = t(coredata(x[,asset.names,drop=FALSE]))
  weighted.benchmark.returns = t(coredata(x[, strategy.names, drop=FALSE]))
  component = t(coredata(x[, component.names, drop=FALSE]))
  if (component.type == "manager") {
    rownames(component) = asset.names
  } else if (component.type == "strategy") {
    rownames(component) = strategy.names
  } else {
    rownames(component) = component.names
  }
  carinoLinking = logLinking(weighted.portfolio.returns,
                             weighted.benchmark.returns,
                             component)
  return(carinoLinking[[return.out]])
}
