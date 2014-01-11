\name{etl_milp_opt}
\alias{etl_milp_opt}
\title{Minimum ETL MILP Optimization}
\usage{
  etl_milp_opt(R, constraints, moments, target, alpha,
    solver = "glpk")
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{constraints}{object of constraints in the portfolio
  object extracted with \code{get_constraints}}

  \item{moments}{object of moments computed based on
  objective functions}

  \item{target}{target return value}

  \item{alpha}{alpha value for ETL/ES/CVaR}

  \item{solver}{solver to use}
}
\description{
  This function is called by optimize.portfolio to solve
  minimum ETL problems via mixed integer linear
  programming.
}
\author{
  Ross Bennett
}

