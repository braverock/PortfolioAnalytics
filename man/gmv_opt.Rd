\name{gmv_opt}
\alias{gmv_opt}
\title{Optimization function to solve minimum variance or maximum quadratic utility problems}
\usage{
  gmv_opt(R, constraints, moments, lambda, target,
    lambda_hhi)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{constraints}{object of constraints in the portfolio
  object extracted with \code{get_constraints}}

  \item{moments}{object of moments computed based on
  objective functions}

  \item{lambda}{risk_aversion parameter}

  \item{target}{target return value}

  \item{lambda_hhi}{concentration aversion parameter}
}
\description{
  This function is called by optimize.portfolio to solve
  minimum variance or maximum quadratic utility problems
}
\author{
  Ross Bennett
}

