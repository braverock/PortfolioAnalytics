#' @title Optimal Portfolio Weights and Performance
#' 
#' @description Converts output of PortfolioAnalytics function
#' optimize.portfolio which computes a minimum variance portfolio
#' to a list containing the portfolio weights vector, mean, volatility
#' and Sharpe Ratio.
#' 
#' @param opt List output of optimize.portfolio 
#' @param returns Multivariate xts object of portfolio assets returns
#' @param digits Integer number of significant digits with default NULL
#' @param names Character vector of names assigned to function output list
#' @param rf Numeric value with default 0.003
#' 
#' @details This functions uses the weights returned by optimize.portfolio,
#' along with the returns to compute the portfolio mean return and volatility,
#' and along with the risk-free rate rf it computes the Sharpe Ratio.
#'
#' @return Optimal a list containing the numeric weight vector, and the 
#' numeric values of the portfolio return mean, volatility, and Sharpe ratio
#' 
#' @author R. Douglas Martin
#' @export
#'
#' @examples
#' args(opt.outputMvo)
opt.outputMvo <- function(opt, returns, digits = NULL, names = NULL, rf = 0.003){
  wts <- opt$weights
  sigmasq <- as.numeric(t(wts)%*%var(returns)%*%wts)
  sigma <- sqrt(sigmasq)
  mu.ret <- apply(returns,2,mean)
  mu <- as.numeric(t(wts)%*%mu.ret)
  sr <- (mu-rf)/sigma
  if(is.null(digits))
  {names(sigma) <- "sigma"
  names(mu) <- "mu"
  output <- c(wts,mu,sigma)} else
  {if(is.null(names))
  {output <- list(wts = wts, mean = mu, stdev = sigma, sr = sr)
  output <- lapply(output,round,digits)}
    else
    {output <- list(wts,mu,sigma,sr)
    names(output) <- names
    output <- lapply(output,round,digits)}
  }
  output
}