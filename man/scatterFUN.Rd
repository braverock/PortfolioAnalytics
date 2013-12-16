\name{scatterFUN}
\alias{scatterFUN}
\title{Apply a risk or return function to asset returns}
\usage{
  scatterFUN(R, FUN, arguments = NULL)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{FUN}{name of function}

  \item{arguments}{named list of arguments to FUN}
}
\description{
  This function is used to calculate risk or return metrics
  given a matrix of asset returns and will be used for a
  risk-reward scatter plot of the assets
}
\author{
  Ross Bennett
}

