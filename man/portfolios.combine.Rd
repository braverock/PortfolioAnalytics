\name{portfolios.combine}
\alias{portfolios.combine}
\title{Combine objects created by portfolio}
\usage{
  portfolios.combine(x)
}
\arguments{
  \item{x}{a list of objects created by
  \code{\link{portfolio.spec}}}
}
\value{
  a \code{portfolio.list} object
}
\description{
  This function takes a list of objects created by
  \code{\link{portfolio.spec}} and sets the class name
  attribute to 'portfolio.list' for use in generic
  functions
}

