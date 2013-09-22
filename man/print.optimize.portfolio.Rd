\name{print.optimize.portfolio}
\alias{print.optimize.portfolio.DEoptim}
\alias{print.optimize.portfolio.GenSA}
\alias{print.optimize.portfolio.pso}
\alias{print.optimize.portfolio.random}
\alias{print.optimize.portfolio.ROI}
\title{Printing output of optimize.portfolio}
\usage{
  \method{print}{optimize.portfolio.ROI} (x, ...,
    digits = 4)

  \method{print}{optimize.portfolio.random} (x, ...,
    digits = 4)

  \method{print}{optimize.portfolio.DEoptim} (x, ...,
    digits = 4)

  \method{print}{optimize.portfolio.GenSA} (x, ...,
    digits = 4)

  \method{print}{optimize.portfolio.pso} (x, ...,
    digits = 4)
}
\arguments{
  \item{x}{an object used to select a method}

  \item{\dots}{any other passthru parameters}

  \item{digits}{the number of significant digits to use
  when printing.}
}
\description{
  print method for optimize.portfolio objects
}
\author{
  Ross Bennett
}

