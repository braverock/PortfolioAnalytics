\name{optimize.portfolio}
\alias{optimize.portfolio}
\title{wrapper for constrained optimization of portfolios}
\usage{
  optimize.portfolio(R, constraints,
    optimize_method = c("DEoptim", "random", "ROI", "ROI_new", "pso"),
    search_size = 20000, trace = FALSE, ..., rp = NULL,
    momentFUN = "set.portfolio.moments")
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{constraints}{an object of type "constraints"
  specifying the constraints for the optimization, see
  \code{\link{constraint}}, if using closed for solver,
  need to pass a \code{\link{constraint_ROI}} object.}

  \item{optimize_method}{one of "DEoptim", "random",
  "ROI","ROI_new", "pso".  For using \code{ROI}, need to
  use a constraint_ROI object in constraints. For using
  \code{ROI_new}, pass standard \code{constratint} object
  in \code{constraints} argument.  Presently, ROI has
  plugins for \code{quadprog}}

  \item{search_size}{integer, how many portfolios to test,
  default 20,000}

  \item{trace}{TRUE/FALSE if TRUE will attempt to return
  additional information on the path or portfolios
  searched}

  \item{\dots}{any other passthru parameters}

  \item{rp}{matrix of random portfolio weights, default
  NULL, mostly for automated use by rebalancing
  optimization or repeated tests on same portfolios}

  \item{momentFUN}{the name of a function to call to set
  portfolio moments, default
  \code{\link{set.portfolio.moments}}}
}
\value{
  a list containing the optimal weights, some summary
  statistics, the function call, and optionally trace
  information
}
\description{
  This function aims to provide a wrapper for constrained
  optimization of portfolios that allows the user to
  specify box constraints and business objectives.
}
\details{
  This function currently supports DEoptim and random
  portfolios as back ends. Additional back end
  contributions for Rmetrics, ghyp, etc. would be welcome.

  When using random portfolios, search_size is precisely
  that, how many portfolios to test.  You need to make sure
  to set your feasible weights in generatesequence to make
  sure you have search_size unique portfolios to test,
  typically by manipulating the 'by' parameter to select
  something smaller than .01 (I often use .002, as .001
  seems like overkill)

  When using DE, search_size is decomposed into two other
  parameters which it interacts with, NP and itermax.

  NP, the number of members in each population, is set to
  cap at 2000 in DEoptim, and by default is the number of
  parameters (assets/weights) *10.

  itermax, if not passed in dots, defaults to the number of
  parameters (assets/weights) *50.
}
\author{
  Kris Boudt, Peter Carl, Brian G. Peterson
}

