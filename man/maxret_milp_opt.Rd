\name{maxret_milp_opt}
\alias{maxret_milp_opt}
\title{Optimization function to solve minimum variance or maximum quadratic utility problems}
\usage{
  maxret_milp_opt(R, constraints, moments, target)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{constraints}{object of constraints in the portfolio
  object extracted with \code{get_constraints}}

  \item{moments}{object of moments computed based on
  objective functions}

  \item{target}{target return value}
}
\description{
  This function is called by optimize.portfolio to solve
  minimum variance or maximum quadratic utility problems
}
\author{
  Ross Bennett
}

