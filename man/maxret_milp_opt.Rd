\name{maxret_milp_opt}
\alias{maxret_milp_opt}
\title{Maximum Return MILP Optimization}
\usage{
  maxret_milp_opt(R, constraints, moments, target,
    solver = "glpk")
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{constraints}{object of constraints in the portfolio
  object extracted with \code{get_constraints}}

  \item{moments}{object of moments computed based on
  objective functions}

  \item{target}{target return value}

  \item{solver}{solver to use}
}
\description{
  This function is called by optimize.portfolio to solve
  maximum return problems via mixed integer linear
  programming.
}
\author{
  Ross Bennett
}

