\name{period.apply.EZ}
\alias{period.apply.EZ}
\title{Apply Function Over Specified Interval}
\usage{
  period.apply.EZ(x, INDEX, FUN, ...)
}
\arguments{
  \item{x}{data to apply \code{FUN} to}

  \item{INDEX}{numeric vector specifying indexing}

  \item{FUN}{an argument of type \code{function}}

  \item{\dots}{additional arguments for \code{FUN}}
}
\value{
  A vector with length of INDEX minus 1 or a matrix with
  number of rows length of INDEX minus 1.
}
\description{
  Modified version of period.apply() from package xts to
  work with logLinking.zoo
}
\details{
  Similar to the rest of the apply family, calculate a
  specified functions value given a shifting set of data
  values. The primary difference is that it is that
  \code{period.apply.EZ} applies a function to
  non-overlapping intervals along a vector. This is a
  modified of the function \code{period.apply} in package
  xts. The modification allows the output to be either a
  vector with length of \code{INDEX} minus 1 or a matrix
  with number of rows length of \code{INDEX} minus 1.
  Useful for applying arbitrary functions over an entire
  data object by an aribirtary index, as when \code{INDEX}
  is the result of a call to \code{endpoints}.
}
\author{
  Original code by Jeff Ryan. Modified by Eric Zivot.
}

