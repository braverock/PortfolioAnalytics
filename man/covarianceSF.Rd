\name{covarianceSF}
\alias{covarianceSF}
\title{Covariance Matrix Estimate}
\usage{
  covarianceSF(beta, stockM2, factorM2)
}
\arguments{
  \item{beta}{vector of length N or (N x 1) matrix of
  factor loadings (i.e. the betas) from a single factor
  statistical factor model}

  \item{stockM2}{vector of length N of the variance (2nd
  moment) of the model residuals (i.e. idiosyncratic
  variance of the stock)}

  \item{factorM2}{scalar value of the 2nd moment of the
  factor realizations from a single factor statistical
  factor model}
}
\value{
  (N x N) covariance matrix
}
\description{
  Estimate covariance matrix using a single factor
  statistical factor model
}
\details{
  This function estimates an (N x N) covariance matrix from
  a single factor statistical factor model with k=1
  factors, where N is the number of assets.
}

