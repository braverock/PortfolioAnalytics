\name{add.constraint}
\alias{add.constraint}
\title{General interface for adding and/or updating optimization constraints.}
\usage{
  add.constraint(portfolio, type, enabled = TRUE,
    message = FALSE, ..., indexnum = NULL)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio' to add
  the constraint to, specifying the constraints for the
  optimization, see \code{\link{portfolio.spec}}}

  \item{type}{character type of the constraint to add or
  update, currently 'weight_sum' (also 'leverage' or
  'weight'), 'box', 'group', 'turnover', 'diversification',
  'position_limit', 'return', or 'factor_exposure'}

  \item{enabled}{TRUE/FALSE. The default is enabled=TRUE.}

  \item{message}{TRUE/FALSE. The default is message=FALSE.
  Display messages if TRUE.}

  \item{\dots}{any other passthru parameters to specify
  constraints}

  \item{indexnum}{if you are updating a specific
  constraint, the index number in the $constraints list to
  update}
}
\description{
  This is the main function for adding and/or updating
  constraints to the \code{{portfolio}} object.
}
\details{
  The following constraint types are supported: \itemize{
  \item{\code{weight_sum}, \code{weight}, \code{leverage}}{
  Specify constraint on the sum of the weights, see
  \code{\link{weight_sum_constraint}}}
  \item{\code{full_investment}}{ Special case to set
  \code{min_sum=1} and \code{max_sum=1} of weight sum
  constraints} \item{\code{dollar_neutral}, \code{active}}{
  Special case to set \code{min_sum=0} and \code{max_sum=0}
  of weight sum constraints} \item{\code{box}}{ Specify
  constraints for the individual asset weights, see
  \code{\link{box_constraint}}} \item{\code{long_only}}{
  Special case to set \code{min=0} and \code{max=1} of box
  constraints} \item{\code{group}}{ Specify a constraint on
  the sum of weights within groups and the number of assets
  with non-zero weights in groups, see
  \code{\link{group_constraint}}} \item{\code{turnover}}{
  Specify a constraint for target turnover. Turnover is
  calculated from a set of initial weights, see
  \code{\link{turnover_constraint}}}
  \item{\code{diversification}}{ Specify a constraint for
  target diversification of a set of weights, see
  \code{\link{diversification_constraint}}}
  \item{\code{position_limit}}{ Specify a constraint on the
  number of positions (i.e. assets with non-zero weights as
  well as the number of long and short positions, see
  \code{\link{position_limit_constraint}}}
  \item{\code{return}}{ Specify a constraint for target
  mean return, see \code{\link{return_constraint}}}
  \item{\code{factor_exposure}}{ Specify a constraint for
  risk factor exposures, see
  \code{\link{factor_exposure_constraint}}} }
}
\examples{
data(edhec)
returns <- edhec[, 1:4]
fund.names <- colnames(returns)
pspec <- portfolio.spec(assets=fund.names)
# Add the full investment constraint that specifies the weights must sum to 1.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)
# The full investment constraint can also be specified with type="full_investment"
pspec <- add.constraint(portfolio=pspec, type="full_investment")

# Another common constraint is that portfolio weights sum to 0.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0, max_sum=0)
pspec <- add.constraint(portfolio=pspec, type="dollar_neutral")
pspec <- add.constraint(portfolio=pspec, type="active")

# Add box constraints
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4)

min and max can also be specified per asset
pspec <- add.constraint(portfolio=pspec, type="box", min=c(0.05, 0, 0.08, 0.1), max=c(0.4, 0.3, 0.7, 0.55))
# A special case of box constraints is long only where min=0 and max=1
# The default action is long only if min and max are not specified
pspec <- add.constraint(portfolio=pspec, type="box")
pspec <- add.constraint(portfolio=pspec, type="long_only")

# Add group constraints
pspec <- add.constraint(portfolio=pspec, type="group", groups=c(3, 1), group_min=c(0.1, 0.15), group_max=c(0.85, 0.55), group_labels=c("GroupA", "GroupB"), group_pos=c(2, 1))

# Add position limit constraint such that we have a maximum number of three assets with non-zero weights.
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

# Add diversification constraint
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)

# Add turnover constraint
pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)

# Add target mean return constraint
pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{weight_sum_constraint}},
  \code{\link{box_constraint}},
  \code{\link{group_constraint}},
  \code{\link{turnover_constraint}},
  \code{\link{diversification_constraint}},
  \code{\link{position_limit_constraint},
  \code{\link{return_constraint},
  \code{\link{factor_exposure_constraint}}
}

