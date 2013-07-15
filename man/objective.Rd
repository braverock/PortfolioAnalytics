\name{objective}
\alias{objective}
\title{constructor for class 'objective'}
\usage{
  objective(name, target = NULL, arguments, enabled = TRUE,
    ..., multiplier = 1, objclass = "objective")
}
\arguments{
  \item{name}{name of the objective which will be used to
  call a function, like 'ES', 'VaR', 'mean'}

  \item{target}{univariate target for the objective,
  default NULL}

  \item{arguments}{default arguments to be passed to an
  objective function when executed}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthrough parameters}

  \item{multiplier}{multiplier to apply to the objective,
  usually 1 or -1}

  \item{objclass}{string class to apply, default
  'objective'}
}
\description{
  constructor for class 'objective'
}
\author{
  Brian G. Peterson
}

