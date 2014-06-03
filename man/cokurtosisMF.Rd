\name{cokurtosisMF}
\alias{cokurtosisMF}
\title{Cokurtosis Matrix Estimate}
\usage{
  cokurtosisMF(beta, stockM2, stockM4, factorM2, factorM4)
}
\arguments{
  \item{beta}{(N x k) matrix of factor loadings (i.e. the
  betas) from a statistical factor model}

  \item{stockM2}{vector of length N of the 2nd moment of
  the model residuals}

  \item{stockM4}{vector of length N of the 4th moment of
  the model residuals}

  \item{factorM2}{(k x k) matrix of the 2nd moment of the
  factor realizations from a statistical factor model}

  \item{factorM4}{(k x k^3) matrix of the 4th moment of the
  factor realizations from a statistical factor model}
}
\value{
  (N x N^3) cokurtosis matrix
}
\description{
  Estimate cokurtosis matrix using a statistical factor
  model
}
\details{
  This function estimates an (N x N^3) cokurtosis matrix
  from a statistical factor model with k factors, where N
  is the number of assets.
}

