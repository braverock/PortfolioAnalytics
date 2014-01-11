\name{maxret_opt}
\alias{maxret_opt}
\title{Maximum Return LP Optimization}
\usage{
  maxret_opt(R, moments, constraints, target,
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
  maximum return
}
\author{
  Ross Bennett
}

