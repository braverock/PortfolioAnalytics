\name{indexes}
\alias{indexes}
\docType{data}
\title{Six Major Economic Indexes}
\description{Monthly data of five indexes beginning on 2000-01-31 and ending 2009-12-31.
The indexes are: US Bonds, US Equities, International Equities, Commodities, US T-Bills, and Inflation}
\usage{data(indexes)}
\format{CSV converted into xts object with montly observations}
\examples{
data(indexes)

#preview the data
head(indexes)

#summary period statistics
summary(indexes)
}
\keyword{datasets}
