\name{gmv_opt_toc}
\alias{gmv_opt_toc}
\title{GMV/QU QP Optimization with Turnover Constraint}
\usage{
  gmv_opt_toc(R, constraints, moments, lambda, target,
    init_weights, solver = "quadprog")
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{constraints}{object of constraints in the portfolio
  object extracted with \code{get_constraints}}

  \item{moments}{object of moments computed based on
  objective functions}

  \item{lambda}{risk_aversion parameter}

  \item{target}{target return value}

  \item{init_weights}{initial weights to compute turnover}

  \item{solver}{solver to use}
}
\description{
  This function is called by optimize.portfolio to solve
  minimum variance or maximum quadratic utility problems
  with turnover constraint
}
\author{
  Ross Bennett
}

