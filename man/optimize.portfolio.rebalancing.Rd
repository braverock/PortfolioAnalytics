% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimize.portfolio.R
\name{optimize.portfolio.rebalancing}
\alias{optimize.portfolio.rebalancing}
\alias{optimize.portfolio.rebalancing_v1}
\title{Portfolio Optimization with Rebalancing Periods}
\usage{
optimize.portfolio.rebalancing_v1(
  R,
  constraints,
  optimize_method = c("DEoptim", "random", "ROI"),
  search_size = 20000,
  trace = FALSE,
  ...,
  rp = NULL,
  rebalance_on = NULL,
  training_period = NULL,
  rolling_window = NULL
)

optimize.portfolio.rebalancing(
  R,
  portfolio = NULL,
  constraints = NULL,
  objectives = NULL,
  optimize_method = c("DEoptim", "random", "ROI", "CVXR"),
  search_size = 20000,
  trace = FALSE,
  ...,
  rp = NULL,
  rebalance_on = NULL,
  training_period = NULL,
  rolling_window = NULL
)
}
\arguments{
\item{R}{an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns}

\item{constraints}{default NULL, a list of constraint objects}

\item{optimize_method}{one of "DEoptim", "random", "pso", "GenSA", or "ROI"}

\item{search_size}{integer, how many portfolios to test, default 20,000}

\item{trace}{TRUE/FALSE if TRUE will attempt to return additional 
information on the path or portfolios searched}

\item{\dots}{any other passthru parameters to \code{\link{optimize.portfolio}}}

\item{rp}{a set of random portfolios passed into the function to prevent recalculation}

\item{rebalance_on}{character string of period to rebalance on. See 
\code{\link[xts]{endpoints}} for valid names.}

\item{training_period}{an integer of the number of periods to use as 
a training data in the front of the returns data}

\item{rolling_window}{an integer of the width (i.e. number of periods)
of the rolling window, the default of NULL will run the optimization 
using the data from inception.}

\item{portfolio}{an object of type "portfolio" specifying the constraints 
and objectives for the optimization}

\item{objectives}{default NULL, a list of objective objects}
}
\value{
a list containing the following elements
\describe{
  \item{\code{portfolio}:}{ The portfolio object.}
  \item{\code{R}:}{ The asset returns.}
  \item{\code{call}:}{ The function call.}
  \item{\code{elapsed_time:}}{ The amount of time that elapses while the 
  optimization is run.}
  \item{\code{opt_rebalancing:}}{ A list of \code{optimize.portfolio} 
  objects computed at each rebalancing period.}
}
}
\description{
Portfolio optimization with support for rebalancing periods for 
out-of-sample testing (i.e. backtesting)
}
\details{
Run portfolio optimization with periodic rebalancing at specified time periods. 
Running the portfolio optimization with periodic rebalancing can help 
refine the constraints and objectives by evaluating the out of sample
performance of the portfolio based on historical data.

If both \code{training_period} and \code{rolling_window} are \code{NULL}, 
then \code{training_period} is set to a default value of 36. 

If \code{training_period} is \code{NULL} and a \code{rolling_window} is 
specified, then \code{training_period} is set to the value of 
\code{rolling_window}.

The user should be aware of the following behavior when both 
\code{training_period} and \code{rolling_window} are specified and have 
different values
\describe{
  \item{\code{training_period < rolling_window}: }{For example, if you have 
  \code{rolling_window=60}, \code{training_period=50}, and the periodicity 
  of the data is the same as the rebalance frequency (i.e. monthly data with 
  \code{rebalance_on="months")} then the returns data used in the optimization 
  at each iteration are as follows:
  \itemize{
  \item 1: R[1:50,]
  \item 2: R[1:51,]
  \item ...
  \item 11: R[1:60,]
  \item 12: R[1:61,]
  \item 13: R[2:62,]
  \item ...
  }
  This results in a growing window for several optimizations initially while
  the endpoint iterator (i.e. \code{[50, 51, ...]}) is less than the 
  rolling window width.}
  \item{\code{training_period > rolling_window}: }{The data used in the initial 
  optimization is \code{R[(training_period - rolling_window):training_period,]}. 
  This results in some of the data being "thrown away", i.e. periods 1 to 
  \code{(training_period - rolling_window - 1)} are not used in the optimization.}
}

This function is a essentially a wrapper around \code{optimize.portfolio} 
and thus the discussion in the Details section of the 
\code{\link{optimize.portfolio}} help file is valid here as well.

This function is massively parallel and requires the 'foreach' package. It
is suggested to register a parallel backend.
}
\examples{
\dontrun{
data(edhec)
R <- edhec[,1:4]
funds <- colnames(R)

portf <- portfolio.spec(funds)
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type="risk", name="StdDev")

# Quarterly rebalancing with 5 year training period
bt.opt1 <- optimize.portfolio.rebalancing(R, portf,
optimize_method="ROI",
rebalance_on="quarters",
training_period=60)

# Monthly rebalancing with 5 year training period and 4 year rolling window
bt.opt2 <- optimize.portfolio.rebalancing(R, portf,
optimize_method="ROI",
rebalance_on="months",
training_period=60,
rolling_window=48)
}
}
\seealso{
\code{\link{portfolio.spec}} \code{\link{optimize.portfolio}}
}
\author{
Kris Boudt, Peter Carl, Brian G. Peterson
}
