\name{add.constraint}
\alias{add.constraint}
\title{General interface for adding and/or updating optimization constraints.}
\usage{
  add.constraint(portfolio, type, enabled = TRUE, ...,
    indexnum = NULL)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio' to add
  the constraint to, specifying the constraints for the
  optimization, see \code{\link{portfolio.spec}}}

  \item{type}{character type of the constraint to add or
  update, currently 'weight_sum', 'box', 'group',
  'turnover', 'diversification', or 'position_limit'}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify
  constraints}

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
  'diversification', or 'position_limit'.

  Special cases for the weight_sum constraint are
  "full_investment" and "dollar_nuetral" or "active" with
  appropriate values set for min_sum and max_sum. see
  \code{\link{weight_sum_constraint}}
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
  \code{\link{position_limit_constraint}}
}

