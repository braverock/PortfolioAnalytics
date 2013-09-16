\name{add.objective}
\alias{add.objective}
\alias{add.objective_v1}
\alias{add.objective_v2}
\title{General interface for adding optimization objectives, including risk, return, and risk budget}
\usage{
  add.objective_v1(constraints, type, name,
    arguments = NULL, enabled = TRUE, ..., indexnum = NULL)

  add.objective_v2(portfolio, constraints = NULL, type,
    name, arguments = NULL, enabled = TRUE, ...,
    indexnum = NULL)

  add.objective(portfolio, constraints = NULL, type, name,
    arguments = NULL, enabled = TRUE, ..., indexnum = NULL)
}
\arguments{
  \item{portfolio}{an object of type 'portfolio' to add the
  objective to, specifying the portfolio for the
  optimization, see \code{\link{portfolio}}}

  \item{constraints}{a 'v1_constraint' object for backwards
  compatibility, see \code{\link{constraint}}}

  \item{type}{character type of the objective to add or
  update, currently 'return','risk', 'risk_budget',
  'quadratic_utility', or 'weight_concentration'}

  \item{name}{name of the objective, should correspond to a
  function, though we will try to make allowances}

  \item{arguments}{default arguments to be passed to an
  objective function when executed}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters}

  \item{indexnum}{if you are updating a specific
  constraint, the index number in the $objectives list to
  update}
}
\description{
  This function is the main function for adding and
  updating business objectives in an object of type
  \code{\link{portfolio.spec}}.
}
\details{
  In general, you will define your objective as one of the
  following types: 'return', 'risk', 'risk_budget',
  'quadratic utility', or 'weight_concentration'. These
  have special handling and intelligent defaults for
  dealing with the function most likely to be used as
  objectives, including mean, median, VaR, ES, etc.

  Objectives of type 'turnover' and 'minmax' are also
  supported.
}
\author{
  Brian G. Peterson and Ross Bennett
}
\seealso{
  \code{\link{objective}}, \code{\link{portfolio.spec}}
}

