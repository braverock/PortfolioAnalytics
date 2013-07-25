\name{optimize.portfolio.rebalancing_v1}
\alias{optimize.portfolio.rebalancing_v1}
\title{version 1 portfolio optimization with support for rebalancing or rolling periods}
\usage{
  optimize.portfolio.rebalancing_v1(R, constraints,
    optimize_method = c("DEoptim", "random", "ROI"),
    search_size = 20000, trace = FALSE, ..., rp = NULL,
    rebalance_on = NULL, training_period = NULL,
    trailing_periods = NULL)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{constraints}{an object of type "constraints"
  specifying the constraints for the optimization, see
  \code{\link{constraint}}}

  \item{optimize_method}{one of "DEoptim" or "random"}

  \item{search_size}{integer, how many portfolios to test,
  default 20,000}

  \item{trace}{TRUE/FALSE if TRUE will attempt to return
  additional information on the path or portfolios
  searched}

  \item{\dots}{any other passthru parameters}

  \item{rp}{a set of random portfolios passed into the
  function, to prevent recalculation}

  \item{rebalance_on}{a periodicity as returned by xts
  function periodicity and usable by endpoints}

  \item{training_period}{period to use as training in the
  front of the data}

  \item{trailing_periods}{if set, an integer with the
  number of periods to roll over, default NULL will run
  from inception}
}
\value{
  a list containing the optimal weights, some summary
  statistics, the function call, and optionally trace
  information
}
\description{
  This function may eventually be wrapped into
  optimize.portfolio
}
\details{
  For now, we'll set the rebalancing periods here, though I
  think they should eventually be part of the constraints
  object

  This function is massively parallel, and will require
  'foreach' and we suggest that you register a parallel
  backend.
}
\author{
  Kris Boudt, Peter Carl, Brian G. Peterson
}

