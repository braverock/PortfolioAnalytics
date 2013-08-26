\name{optimize.portfolio}
\alias{optimize_portfolio_v1}
\alias{optimize.portfolio}
\alias{optimize.portfolio_v2}
\title{constrained optimization of portfolios}
\usage{
  optimize.portfolio_v1(R, constraints,
    optimize_method = c("DEoptim", "random", "ROI", "ROI_old", "pso", "GenSA"),
    search_size = 20000, trace = FALSE, ..., rp = NULL,
    momentFUN = "set.portfolio.moments_v1")

  optimize.portfolio_v2(R, portfolio = NULL,
    constraints = NULL, objectives = NULL,
    optimize_method = c("DEoptim", "random", "ROI", "ROI_old", "pso", "GenSA"),
    search_size = 20000, trace = FALSE, ..., rp = NULL,
    momentFUN = "set.portfolio.moments", message = FALSE)

  optimize.portfolio(R, portfolio = NULL,
    constraints = NULL, objectives = NULL,
    optimize_method = c("DEoptim", "random", "ROI", "ROI_old", "pso", "GenSA"),
    search_size = 20000, trace = FALSE, ..., rp = NULL,
    momentFUN = "set.portfolio.moments", message = FALSE)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{portfolio}{an object of type "portfolio" specifying
  the constraints and objectives for the optimization}

  \item{constraints}{default=NULL, a list of constraint
  objects. An object of class v1_constraint' can be passed
  in here.}

  \item{objectives}{default=NULL, a list of objective
  objects.}

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
  \code{\link{set.portfolio.moments_v2}}}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}
}
\value{
  a list containing the following elements \itemize{
  \item{\code{weights}:}{ The optimal set weights.}
  \item{\code{objective_measures}:}{ A list containing the
  value of each objective corresponding to the optimal
  weights.} \item{\code{opt_values}:}{ A list containing
  the value of each objective corresponding to the optimal
  weights.} \item{\code{out}:}{ The output of the solver.}
  \item{\code{call}:}{ The function call.}
  \item{\code{portfolio}:}{ The portfolio object.}
  \item{\code{R}:}{ The asset returns.} \item{\code{data
  summary:}}{ The first row and last row of \code{R}.}
  \item{\code{elapsed_time:}}{ The amount of time that
  elapses while the optimization is run.}
  \item{\code{end_t:}}{ The date and time the optimization
  completed.} } When Trace=TRUE is specified, the following
  elements will be returned in addition to the elements
  above. The output depends on the optimization method and
  is specific to each solver. Refer to the documentation of
  the desired solver for more information.

  \code{optimize_method="random"} \itemize{
  \item{\code{random_portfolios}:}{ A matrix of the random
  portfolios.}
  \item{\code{random_portfolio_objective_results}:}{ A list
  of the following elements for each random portfolio.}
  \itemize{ \item{\code{out}:}{ The output value of the
  solver corresponding to the random portfolio weights.}
  \item{\code{weights}:}{ The weights of the random
  portfolio.} \item{\code{objective_measures}:}{ A list of
  each objective measure corresponding to the random
  portfolio weights.} } }

  \code{optimize_method="DEoptim"} \itemize{
  \item{\code{DEoutput:}}{ A list (of length 2) containing
  the following elements, see \code{\link{DEoptim}}.}
  \itemize{ \item{\code{optim}} \item{\code{member}} }
  \item{\code{DEoptim_objective_results}:}{ A list
  containing the following elements for each intermediate
  population.} \itemize{ \item{\code{out}:}{ The output of
  the solver.} \item{\code{weights}:}{ Population weights.}
  \item{\code{init_weights}:}{ Initial population weights.}
  \item{\code{objective_measures}:}{ A list of each
  objective measure corresponding to the weights} } }

  \code{optimize_method="pso"} \itemize{
  \item{\code{PSOoutput}:}{ A list containing the following
  elements, see \code{\link{psoptim}}:} \itemize{
  \item{par} \item{value} \item{counts} \item{convergence}
  \item{message} \item{stats} } }

  \code{optimize_method="GenSA"} \itemize{
  \item{\code{GenSAoutput:}}{ A list containing the
  following elements, see \code{\link{GenSA}}:} \itemize{
  \item{value} \item{par} \item{trace.mat} \item{counts} }
  }
}
\description{
  This function aims to provide a wrapper for constrained
  optimization of portfolios that allows the user to
  specify box constraints and business objectives. It will
  be the objective function\code{FUN} passed to any
  supported \R optimization solver.
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

  The extension to ROI solves a limited type of convex
  optimization problems: \itemize{ \item{Maxmimize
  portfolio return subject leverage, box, group, position
  limit, target mean return, and/or factor exposure
  constraints on weights} \item{Minimize portfolio variance
  subject to leverage, box, group, and/or factor exposure
  constraints (otherwise known as global minimum variance
  portfolio)} \item{Minimize portfolio variance subject to
  leverage, box, group, and/or factor exposure constraints
  and a desired portfolio return} \item{Maximize quadratic
  utility subject to leverage, box, group, target mean
  return, and/or factor exposure constraints and risk
  aversion parameter. (The risk aversion parameter is
  passed into \code{optimize.portfolio} as an added
  argument to the \code{portfolio} object)} \item{Mean CVaR
  optimization subject to leverage, box, group, position
  limit, target mean return, and/or factor exposure
  constraints and target portfolio return} } Because these
  convex optimization problem are standardized, there is no
  need for a penalty term. The \code{multiplier} argument
  in \code{\link{add.objective}} passed into the complete
  constraint object are ingnored by the ROI solver.

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
  c(3,3,3)}.

  To apply the constraints that the first group must
  compose of at least 20% of the weight, the second group
  15%, and the third group 10%, and that now signle group
  should compose of more that 50% of the weight, then you
  would add the lower group constraint as
  \code{constraints$cLO <- c(0.20, 0.15, 0.10)} and the
  upper constraints as \code{constraints$cUP <-
  rep(0.5,3)}. These group constraint can be set for all
  five convex optimization problems listed above, as well
  as for the global stochastic solvers DEoptim, random,
  pso, and GenSA.

  If you would like to interface with
  \code{optimize.portfolio} using matrix formulations, then
  use \code{ROI_old}.
}
\note{
  An object of class \code{v1_constraint} can be passed in
  for the \code{constraints} argument. The
  \code{v1_constraint} object was used in the previous 'v1'
  specification to specify the constraints and objectives
  for the optimization problem, see
  \code{\link{constraint}}. We will attempt to detect if
  the object passed into the constraints argument is a
  \code{v1_constraint} object and update to the 'v2'
  specification by adding the constraints and objectives to
  the \code{portfolio} object.
}
\author{
  Kris Boudt, Peter Carl, Brian G. Peterson, Ross Bennett
}
\seealso{
  \code{\link{portfolio.spec}}
}

