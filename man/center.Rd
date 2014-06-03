\name{center}
\alias{center}
\title{Center}
\usage{
  center(x)
}
\arguments{
  \item{x}{matrix}
}
\value{
  matrix of centered data
}
\description{
  Center a matrix
}
\details{
  This function is used primarily to center a time series
  of asset returns or factors. Each column should represent
  the returns of an asset or factor realizations. The
  expected value is taken as the sample mean.

  x.centered = x - mean(x)
}

