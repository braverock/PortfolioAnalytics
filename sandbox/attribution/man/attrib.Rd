\name{attrib}
\docType{data}
\alias{attrib}
\alias{attrib.hierarchy}
\alias{attrib.returns}
\alias{attrib.weights}
\alias{attrib.allocation}
\alias{attrib.currency}
\title{Hypothetical Portfolio Data with Returns, Weights, Hierarchy, Currency
Contracts and Allocation effects}
\description{
A dataset for examples in the attribution functions.
}
\usage{attrib}
\details{
Contains 3 xts objects, a data frame and a matrix.
\itemize{\item 'attrib.returns' - xts object with portfolio returns (9 stocks 
and one bond), benchmark returns (S\&P 500 total returns and 3-month treasury 
bills), total portfolio returns and total benchmark returns, risk-free rates, 
portfolio and benchmark returns in the local currency, benchmark returns hedged
into the corresponding currency; portfolio and benchmark duration for the 
portfolio with bonds. Quarterly returns for all series end in the fourth 
quarter of 2008 and begin in the second quarter of 2007 
\item 'attrib.weights' - matrix with portfolio weights, benchmark weights, 
portfolio weights of currency forward contract in the corresponding currencies
and benchmark weights of currenccy forward contract in the corresponding 
currencies \item 'attrib.hierarchy' - data frame with portfolio hierarchy
\item 'attrib.currency' - xts object with forward and spot rates for currency
contracts. Contains data for one more period than returns in order to compute 
corresponding currency  returns and currency surprises 
\item 'attrib.allocation' - xts with allocation effects for the above mentioned 
portfolio and benchmark of the same length as portfolio returns}
}
\examples{
data(attrib)
}
\keyword{datasets}
\keyword{ ts }