\name{logLinking.zoo}
\alias{logLinking.zoo}
\title{Log-linking Function For Use In rollApplyEZ}
\usage{
  logLinking.zoo(x, asset.names, strategy.names,
    component.names,
    component.type = c("manager", "strategy", "factor"),
    return.out = c("linked", "linked.total"))
}
\arguments{
  \item{x}{\code{zoo} object containing data to be
  aggregated.}

  \item{asset.names}{Character vector of asset IDs}

  \item{strategy.names}{Character vector of strategy
  abbreviations}

  \item{component.names}{Character vector of component
  names. If \code{component.type="manager"} then
  \code{component.names} are asset IDs; if
  \code{component.type="strategy"} then
  \code{component.names} are strategy abbreviations.if
  \code{component.type="factor"} then
  \code{component.names} are factor IDs.}

  \item{component.type}{character string indicating type of
  attribution. Valid choices are \code{"manager"} for
  manager selection attribution; \code{"strategy"} for
  strategy allocation attribution; \code{"factor"} for
  factor attribution.}

  \item{return.out}{character string indicating output from
  log-linking function. Valid choices are \code{"linked"}
  for disaggregated results, and \code{"linked.total"} for
  total results.}
}
\value{
  A numeric vector of attribution values with the same
  length as \code{component.names} if
  \code{return.out="linked"} or a numeric value giving
  total attribution if \code{return.out = "linked.total"}.
}
\description{
  Function to be passed to rollApplyEZ() for carino linking
  over user-specified attribution dates and aggregation
  periods
}
\details{
  %% ~~ If necessary, more details than the description
  above ~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
\author{
  Eric Zivot.
}
\seealso{
  \code{\link{logLinking}}
}

