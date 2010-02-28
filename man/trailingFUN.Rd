\name{trailingFUN}
\alias{trailingFUN}
\title{apply a function over a configurable trailing period...}
\usage{trailingFUN(R, weights, n=0, FUN, FUNargs, ...)}
\description{apply a function over a configurable trailing period}
\details{this function is primarily designed for use with portfolio functions passing 
'x' or 'R' and weights, but may be usable for other things as well, see Exmample for a vector example.}
\arguments{\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}
\item{weights}{a vector of weights to test}
\item{\dots}{any other passthru parameters}
\item{n}{numeric number of trailing periods}
\item{FUN}{string describing the function to be called}
\item{FUNargs}{list describing any additional arguments}}
