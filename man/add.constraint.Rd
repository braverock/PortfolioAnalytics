\name{add.constraint}
\alias{add.constraint}
\title{General interface for adding and/or updating optimization constraints.}
\usage{
  add.constraint(portfolio, type, enabled = FALSE, ...,
    indexnum = NULL)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio' to add
  the constraint to, specifying the constraints for the
  optimization, see \code{\link{portfolio.spec}}}

  \item{type}{character type of the constraint to add or
  update, currently 'weight_sum', 'box', 'group',
  'turnover', 'diversification', or 'volatility'}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}

  \item{indexnum}{if you are updating a specific
  constraint, the index number in the $objectives list to
  update}
}
\description{
  This is the main function for adding and/or updating
  constraints in an object of type \code{\link{portfolio}}.
}
\details{
  In general, you will define your constraints as:
  'weight_sum', 'box', 'group', 'turnover',
  'diversification', 'volatility', or 'position_limit'.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{constraint_v2}},
  \code{\link{weight_sum_constraint}},
  \code{\link{box_constraint}},
  \code{\link{group_constraint}},
  \code{\link{turnover_constraint}},
  \code{\link{diversification_constraint}},
  \code{\link{volatility_constraint}},
  \code{\link{position_limit_constraint}}
}

