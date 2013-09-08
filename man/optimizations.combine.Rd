\name{optimizations.combine}
\alias{optimizations.combine}
\title{Combine objects created by optimize.portfolio}
\usage{
  optimizations.combine(x)
}
\arguments{
  \item{x}{a list of objects created by
  \code{\link{optimize.portfolio}}}
}
\value{
  an \code{opt.list} object
}
\description{
  This function takes a list of objects created by
  \code{\link{optimize.portfolio}} and sets the class name
  attribute to 'opt.list' for use in generic functions
}

