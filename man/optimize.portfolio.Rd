\name{optimize.portfolio}
\alias{optimize.portfolio}
\title{wrapper for constrained optimization of portfolios}
\usage{
  optimize.portfolio(R, constraints,
    optimize_method = c("DEoptim", "random", "ROI", "ROI_old", "pso", "GenSA"),
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
  "ROI","ROI_old", "pso", "GenSA".  For using
  \code{ROI_old}, need to use a constraint_ROI object in
  constraints. For using \code{ROI}, pass standard
  \code{constratint} object in \code{constraints} argument.
  Presently, ROI has plugins for \code{quadprog} and
  \code{Rglpk}.}

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

  When using GenSA and want to set \code{verbose=TRUE},
  instead use \code{trace}.

  The extension to ROI solves a limit type of convex
  optimization problems: 1) Maxmimize portfolio return
  subject box constraints on weights 2) Minimize portfolio
  variance subject to box constraints (otherwise known as
  global minimum variance portfolio) 3) Minimize portfolio
  variance subject to box constraints and a desired
  portfolio return 4) Maximize quadratic utility subject to
  box constraints and risk aversion parameter (this is
  passed into \code{optimize.portfolio} as as added
  argument to the \code{constraints} object) 5) Mean CVaR
  optiimization subject to box constraints and target
  portfolio return Lastly, because these convex
  optimization problem are standardized, there is no need
  for a penalty term. Therefore, the \code{multiplier}
  argument in \code{\link{add.objective}} passed into the
  complete constraint object are ingnored by the solver.
  ROI also can solve quadratic and linear problems with
  group constraints by added a \code{groups} argument into
  the constraints object. This argument is a vector with
  each of its elements the number of assets per group. The
  group constraints, \code{cLO} and \code{cUP}, are also
  added to the constraints object.

  For example, if you have 9 assets, and would like to
  require that the the first 3 assets are in one group, the
  second 3 are in another, and the third are in another,
  then you add the grouping by \code{constraints$groups <-
  c(3,3,3)}. To apply the constraints that the first group
  must compose of at least 20% of the weight, the second
  group 15%, and the third group 10%, and that now signle
  group should compose of more that 50% of the weight, then
  you would add the lower group constraint as
  \code{constraints$cLO <- c(0.20, 0.15, 0.10)} and the
  upper constraints as \code{constraints$cUP <-
  rep(0.5,3)}. These group constraint can be set for all
  five optimization problems listed above.

  If you would like to interface with
  \code{optimize.portfolio} using matrix formulations, then
  use \code{ROI_old}.
}
\author{
  Kris Boudt, Peter Carl, Brian G. Peterson
}

