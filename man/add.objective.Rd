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

  \item{indexnum}{if you are updating a specific objective,
  the index number in the $objectives list to update}
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
\examples{
data(edhec)
returns <- edhec[,1:4]
fund.names <- colnames(returns)
portf <- portfolio.spec(assets=fund.names)
# Add some basic constraints
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")

# Creates a new portfolio object using portf and adds a quadratic utility
# objective. This will add two objectives to the portfolio object; 1) mean and
# 2) var. The risk aversion parameter is commonly referred to as lambda in the
# quadratic utility formulation that controls how much the portfolio variance
# is penalized.
portf.maxQU <- add.objective(portf, type="quadratic_utility",
                             risk_aversion=0.25)

# Creates a new portfolio object using portf and adds mean as an objective
portf.maxMean <- add.objective(portf, type="return", name="mean")

# Creates a new portfolio object using portf and adds StdDev as an objective
portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")

# Creates a new portfolio object using portf and adds ES as an objective.
# Note that arguments to ES are passed in as a named list.
portf.minES <- add.objective(portf, type="risk", name="ES",
                             arguments=list(p=0.925, clean="boudt"))

# Creates a new portfolio object using portf.minES and adds a risk budget
# objective with limits on component risk contribution.
# Note that arguments to ES are passed in as a named list.
portf.RiskBudgetES <- add.objective(portf.minES, type="risk_budget", name="ES",
                             arguments=list(p=0.925, clean="boudt"),
                             min_prisk=0, max_prisk=0.6)

# Creates a new portfolio object using portf.minES and adds a risk budget
# objective with equal component risk contribution.
# Note that arguments to ES are passed in as a named list.
portf.EqRiskES <- add.objective(portf.minES, type="risk_budget", name="ES",
                                    arguments=list(p=0.925, clean="boudt"),
                                    min_concentration=TRUE)

# Creates a new portfolio object using portf and adds a weight_concentration
# objective. The conc_aversion parameter controls how much concentration is
# penalized. The portfolio concentration is defined as the Herfindahl Hirschman
# Index of the weights.
portf.conc <- add.objective(portf, type="weight_concentration",
                            name="HHI", conc_aversion=0.01)
}
\author{
  Brian G. Peterson and Ross Bennett
}
\seealso{
  \code{\link{objective}}, \code{\link{portfolio.spec}}
}

