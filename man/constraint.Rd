\name{constraint}
\alias{constraint}
\title{constructor for class constraint...}
\usage{constraint(assets, ..., min, max, min_mult, max_mult, min_sum, max_sum, weight_seq)}
\description{constructor for class constraint}
\author{Peter Carl and Brian G. Peterson}
\arguments{\item{assets}{}
\item{...}{}
\item{min}{}
\item{max}{}
\item{min_mult}{}
\item{max_mult}{}
\item{min_sum}{}
\item{max_sum}{}
\item{weight_seq}{}}
\examples{exconstr <- constraint(assets=10, min_sum=1, max_sum=1, min=.01, max=.35, weight_seq=generatesequence())}
