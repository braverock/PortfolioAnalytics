#' @title Optimal Portfolio Weights and Performance Values
#' 
#' @description Converts output of `optimize.portfolio` to a list of the
#' portfolio weights, mean, volatility and Sharpe Ratio.
#' 
#' @param opt List output of `optimize.portfolio` 
#' @param returns Multivariate xts object of portfolio assets returns
#' @param digits Integer number of significant digits with default NULL
#' @param annualize Logical with default TRUE
#' @param frequency Returns frequency: "monthly", "weekly" or "daily"
#' @param rf Numeric value with default 0.0
#' 
#' @details This function uses the weights returned by optimize.portfolio,
#' along with the portfolio assets returns, and a risk-free rate, to
#' to compute the portfolio mean return, volatility, and Sharpe Ratio.
#'
#' @return A list containing the portfolio numeric weights, mean value,
#' volatility and Sharpe Ratio.
#' 
#' @author R. Douglas Martin
#' @export
#'
#' @examples
#' args(opt.outputMvo)
opt.outputMvo <- function(opt, returns, digits = NULL, annualize = TRUE, 
                             frequency = "monthly", rf = 0.0) 
{
  if(class(returns)[1] == "xts"){
    returns <- coredata(returns)
  }
  Wgts <- opt$weights
  sigmasq <- as.numeric(t(Wgts) %*% var(returns) %*% Wgts)
  StdDev <- sqrt(sigmasq)
  mu.ret <- apply(returns, 2, mean)
  Mean <- as.numeric(t(Wgts) %*% mu.ret)
  SR <- (Mean - rf)/StdDev
  a <- 1; b <- 1
  if(annualize){
    if(frequency == "monthly"){
      a <- 12; b <- sqrt(12)
    } else if(frequency == "weekly"){
      a <- 52; b <- sqrt(52)
    } else {
      a <- 260; b <- sqrt(260)
    }
  }      
  Mean <- a*Mean
  StdDev <- b*StdDev
  SR <- b*SR
  
  output <- list(Wgts = Wgts, Mean = Mean, StdDev = StdDev, SR = SR)
  if (!is.null(digits)) {
    output <- lapply(output, round, digits)
  }
  output
}


###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2023 R. Douglas Martin
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################