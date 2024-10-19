## This R script reproduces the graphs in Exhibits 6, 8, 10,
## 12, 14, 16, 18 of the paper "Minimum Downside Risk Portfolios"
## published in the Journal of Portfolio Management, Oct. 2024.
##
## Copy/past this script into your own computer R file. Then
## run code lines 11 to 262 to create functions that will be 
## needed, and run lines 267 to 271 to load packages needed.
## Then we recommend to run code that follows in convenient
## chunks to replicate each of the above Exhits

rm(list = ls())

# MV: Using CVXR
optimize_portfolio_MV_rebalance <- function(returns, training_period=NULL, rolling_window=NULL, 
                                            rebalance_on) {
  # Start time
  t1 <- Sys.time()
  
  # Get the number of assets and name of the assets
  num_assets <- ncol(returns)
  asset_names <- colnames(returns)
  
  # Set the training period for portfolio optimization with rebalancing
  if(is.null(training_period) & !is.null(rolling_window)) {
    training_period <- rolling_window
  }
  
  # Determine the dates when rebalancing takes place
  epoints <- endpoints(returns, on = rebalance_on)[which(endpoints(returns, on = rebalance_on) >= training_period)]
  
  # Get the rebalancing dates
  rebal_date_indx <- index(returns)[epoints]
  
  # Matrix to hold the weights
  mv_portfolio_wts <- matrix(NA, nrow = length(epoints), ncol = num_assets)
  colnames(mv_portfolio_wts) <- asset_names
  
  # Optimize portfolio at each date
  for (j in 1:length(epoints)) {
    ep <- epoints[j]
    returns_est_window <- returns[(ifelse(ep - rolling_window >= 1, ep - rolling_window, 1)):ep, ]
    
    # Calculate the mean returns and volatility of returns
    mu <- apply(returns_est_window, 2, mean)
    mu <- matrix(mu, nrow = num_assets)
    Sigma <- cov(returns_est_window)
    
    # Formulate the optimization problem
    wts_mv <- Variable(num_assets)
    ret <- t(mu) %*% wts_mv
    
    # Constraints (long-only and full-investment) for MV portfolio optimization
    constraints_mv <- list(wts_mv >= 0, 
                           sum_entries(wts_mv) == 1)
    
    # Objective function for MV portfolio optimization
    objective_mv <- quad_form(wts_mv, Sigma)
    
    # Solve the MV portfolio optimization problem
    prob_mv <- Problem(Minimize(objective_mv), constraints_mv)
    result_mv <- solve(prob_mv, solver = 'OSQP')
    # cat("\nMV Status of the solution: ", result_mv$status)
    # cat("\nMV Solver used: ", result_mv$solver)
    
    # Evaluate risk and return for current solution
    optimal_risk <- sqrt(result_mv$value)
    optimal_ret <- result_mv$getValue(ret)
    
    # Get the optimal weights
    wts <- as.vector(result_mv$getValue(wts_mv))
    names(wts) <- asset_names
    mv_portfolio_wts[j, ] <- t(wts)
  }
  
  # Convert the weights to xts
  rebal_mv_wts <- xts(mv_portfolio_wts, order.by = rebal_date_indx)
  
  # Save the portfolio weights in a list
  result <- list(wts = rebal_mv_wts)
  
  t2 <- Sys.time()
  cat("\nTime taken to run the portfolio optimization: ", t2-t1)
  
  return(result)
}



# MES: Using CVXR 
optimize_portfolio_MES_rebalance <- function(returns, alpha, training_period=NULL, rolling_window=NULL, 
                                             rebalance_on) {
  
  # Start time
  t1 <- Sys.time()
  
  # Get the number of assets and name of the assets
  num_assets <- ncol(returns)
  asset_names <- colnames(returns)
  
  # Set the training period for portfolio optimization with rebalancing
  if(is.null(training_period) & !is.null(rolling_window)) {
    training_period <- rolling_window
  }
  
  # Determine the dates when rebalancing takes place
  epoints <- endpoints(returns, on = rebalance_on)[which(endpoints(returns, on = rebalance_on) >= training_period)]
  
  # Get the rebalancing dates
  rebal_date_indx <- index(returns)[epoints]
  
  # Matrix to hold the weights
  mes_portfolio_wts <- matrix(NA, nrow = length(epoints), ncol = num_assets)
  colnames(mes_portfolio_wts) <- asset_names
  
  # Optimize portfolio at each date
  for (j in 1:length(epoints)) {
    ep <- epoints[j]
    returns_est_window <- returns[(ifelse(ep - rolling_window >= 1, ep - rolling_window, 1)):ep, ]
    
    # Formulate the optimization problem
    # Calculate values of parameters and define variables
    J <- nrow(returns_est_window)
    X <- as.matrix(returns_est_window)
    mu <- colMeans(returns_est_window)
    
    # Variables
    wts_mes <- Variable(num_assets)
    z <- Variable(J)
    eta <- Variable(1)
    
    # Constraints (long-only and full-investment) for MES portfolio optimization
    constraints_mes <- list(wts_mes >= 0, 
                            sum(wts_mes) == 1, 
                            z >= 0, 
                            z >= -X %*% wts_mes - eta
    )
    
    # Objective function for MES portfolio optimization
    # Problem formulation for MES
    objective_mes <- eta + (1/(J*alpha)) * sum(z)
    
    # Solve the MES portfolio optimization problem
    prob_mes <- Problem(Minimize(objective_mes), constraints = constraints_mes)
    result_mes <- solve(prob_mes, solver = 'SCS')
    # cat("\nMES Status of the solution: ", result_mes$status)
    # cat("\nMES Solver used: ", result_mes$solver)
    
    # Get the optimal weights
    wts <- as.vector(result_mes$getValue(wts_mes))
    names(wts) <- asset_names
    mes_portfolio_wts[j, ] <- t(wts)
  }
  
  # Convert the weights to xts
  rebal_mes_wts <- xts(mes_portfolio_wts, order.by = rebal_date_indx)
  
  # Save the portfolio weights in a list
  result <- list(wts = rebal_mes_wts)
  
  t2 <- Sys.time()
  cat("\nTime taken to run the portfolio optimization: ", t2-t1)
  
  return(result)
}



# MCSM: Using CVXR 
optimize_portfolio_MCSM_rebalance <- function(returns, alpha, training_period=NULL, rolling_window=NULL, 
                                              rebalance_on) {
  
  # Start time
  t1 <- Sys.time()
  
  # Get the number of assets and name of the assets
  num_assets <- ncol(returns)
  asset_names <- colnames(returns)
  
  # Set the training period for portfolio optimization with rebalancing
  if(is.null(training_period) & !is.null(rolling_window)) {
    training_period <- rolling_window
  }
  
  # Determine the dates when rebalancing takes place
  epoints <- endpoints(returns, on = rebalance_on)[which(endpoints(returns, on = rebalance_on) >= training_period)]
  
  # Get the rebalancing dates
  rebal_date_indx <- index(returns)[epoints]
  
  # Matrix to hold the weights
  mcsm_portfolio_wts <- matrix(NA, nrow = length(epoints), ncol = num_assets)
  colnames(mcsm_portfolio_wts) <- asset_names
  
  # Optimize portfolio at each date
  for (j in 1:length(epoints)) {
    ep <- epoints[j]
    returns_est_window <- returns[(ifelse(ep - rolling_window >= 1, ep - rolling_window, 1)):ep, ]
    
    # Formulate the optimization problem
    # Calculate values of parameters and define variables
    J <- nrow(returns_est_window)
    X <- as.matrix(returns_est_window)
    mu <- colMeans(returns_est_window)
    
    # Variables
    wts_mcsm <- Variable(num_assets)
    z <- Variable(J)
    eta <- Variable(1)
    f <- Variable(1)
    
    # Constraints (long-only and full-investment) for MCSM portfolio optimization
    constraints_mcsm <- list(wts_mcsm >= 0, 
                             sum_entries(wts_mcsm) == 1, 
                             z >= 0, 
                             z >= -X %*% wts_mcsm - eta, 
                             f >= p_norm(z, p=2)
    )
    
    # Objective function for MCSM portfolio optimization
    # Problem formulation for MCSM (following Krokhmal's algorithm)
    objective_mcsm <- eta + (1/(alpha*sqrt(J)))*f
    
    # Solve the MCSM portfolio optimization problem
    prob_mcsm <- Problem(Minimize(objective_mcsm), constraints = constraints_mcsm)
    result_mcsm <- solve(prob_mcsm, solver = 'SCS')
    # cat("\nMCSM Status of the solution: ", result_mcsm$status)
    # cat("\nMCSM solver used: ", result_mcsm$solver)
    
    # Get the optimal weights
    wts <- as.vector(result_mcsm$getValue(wts_mcsm))
    names(wts) <- asset_names
    mcsm_portfolio_wts[j, ] <- t(wts)
  }
  
  # Convert the weights to xts
  rebal_mcsm_wts <- xts(mcsm_portfolio_wts, order.by = rebal_date_indx)
  
  # Save the portfolio weights in a list
  result <- list(wts = rebal_mcsm_wts)
  
  t2 <- Sys.time()
  cat("\nTime taken to run the portfolio optimization: ", t2-t1)
  
  return(result)
}



# Turnover Control
TOcontrol <- function(wts, delta){
  idx <- index(wts)
  out <- copy(wts)
  TO <- rep(NA, length(idx))
  for(i in 2:length(idx)){
    currentTO <- sum(abs(coredata(wts[idx[i], ]) - coredata(wts[idx[i-1], ])))
    TO[i] <- currentTO
    if(currentTO <= delta){
      out[idx[i], ] <- out[idx[i-1], ]
    }
  }
  return(wts = out)
}

###

# Load packages
library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)


# Use CRSP daily data set
stocksCRSPdaily <- getPCRAData(dataset = "stocksCRSPdaily")
dateRange <- c("1993-01-01","2015-12-31")
smallcapTS <- selectCRSPandSPGMI(
  periodicity = "daily",
  dateRange = dateRange,
  stockItems = c("Date", "TickerLast", "CapGroupLast", "Return", 
                 "MktIndexCRSP", "Ret13WkBill"),
  factorItems = NULL,
  subsetType = "CapGroupLast",
  subsetValues = "SmallCap",
  outputType = "xts")

# Extract Market and RF from smallcapTS
Market <- smallcapTS[ , 107]
names(Market) <- "Market"
RF <- smallcapTS[ , 108]
names(RF)  <- "RF"

# Remove "MktIndexCRSP", "Ret13WkBill" from smallcapTS
smallcapTS <- smallcapTS[ , -c(107,108)]
dim(smallcapTS)

## Exhibit 6  (1 minute 30 seconds)

returns <- smallcapTS[ , 1:30]
dim(returns)

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%)
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.9) # Optimal for 1-30

# Compute returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05 <- Return.rebalancing(returns, mes_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)

# Combine MV, MES05, MES05_TOC returns
portf_ret_comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05", "MES05-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet", 
              main = "MV, MES05, MES05-TOC(0.9), Stocks 1-30", 
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 8 (1 minute 28 seconds)

returns <- smallcapTS[ , 31:60]
dim(returns)

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%)
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.5) # Optimal for 31-60

# Compute returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05 <- Return.rebalancing(returns, mes_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)

# Combine MV, MES05, MES05_TOC returns
portf_ret_comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05", "MES05-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet", 
              main = "MV, MES05, MES05-TOC(0.5), Stocks 31-60", 
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 10 (1 minute 26 seconds)

returns <- smallcapTS[ , 61:90]
dim(returns)

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%)
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.5) # Optimal for 61-90

# Compute returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05 <- Return.rebalancing(returns, mes_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)

# Combine MV, MES05, MES05_TOC returns
portf_ret_comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05", "MES05-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet", 
              main = "MV, MES05, MES05-TOC(0.5), Stocks 61-90", 
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Get Exhibit 12 microcaps data set

#' use CRSP daily microcaps data set
stocksCRSPdaily <- getPCRAData(dataset = "stocksCRSPdaily")
dateRange <- c("1993-01-01","2015-12-31")
microcapTS <- selectCRSPandSPGMI(
  periodicity = "daily",
  dateRange = dateRange,
  stockItems = c("Date", "TickerLast", "CapGroupLast", "Return", 
                 "MktIndexCRSP", "Ret13WkBill"),
  factorItems = NULL,
  subsetType = "CapGroupLast",
  subsetValues = "MicroCap",
  outputType = "xts")

# Extract Market and RF from microcapTS
Market <- microcapTS[ , 35]
names(Market) <- "Market"
RF <- microcapTS[ , 36]
names(RF)  <- "RF"

# Remove "MktIndexCRSP", "Ret13WkBill"
microcapTS <- microcapTS[ , -c(35, 36)]
returns <- microcapTS


## Exhibit 12 (1 minute 33 seconds)

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%)
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.5)

# Compute returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05 <- Return.rebalancing(returns, mes_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)

# Combine MV, MES05, MES05_TOC returns
portf_ret_comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05", "MES05-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet",
              main = "MV, MES05, MES05-TOC(0.5), Microcaps 34",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 14 (2 minutes 34 seconds)

returns <- smallcapTS[ , 1:30]

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%) 
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.9) # Optimal for 1-30

## MCSM15 (for tail probability 15%)
opt_mcsm_result <- optimize_portfolio_MCSM_rebalance(returns = returns, alpha = 0.15, 
                                                     rolling_window = rolling_window, 
                                                     rebalance_on = "months")

# Extract portfolio weights from the result
mcsm_wts <- opt_mcsm_result$wts
mcsm_wts <- mcsm_wts[complete.cases(mcsm_wts), ]

## MCSM15-TOC
mcsm_wts_toc <- TOcontrol(mcsm_wts, 0.8) # Optimal for 1-30

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)
MCSM15TOC <- Return.rebalancing(returns, mcsm_wts_toc)

# Combine MV, MES05TOC, MCSM05TOC gross cumulative returns
portf_ret_comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.9), MCSM15-TOC(0.8), Stocks 1-30",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 16 (2 minutes 35 seconds)

returns <- smallcapTS[ , 31:60]

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns, 
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%)
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.5) # Optimal for 31-60

## MCSM15 (for tail probability 15%)
opt_mcsm_result <- optimize_portfolio_MCSM_rebalance(returns = returns, alpha = 0.15, 
                                                     rolling_window = rolling_window, 
                                                     rebalance_on = "months")

# Extract portfolio weights from the result
mcsm_wts <- opt_mcsm_result$wts
mcsm_wts <- mcsm_wts[complete.cases(mcsm_wts), ]

## MCSM15-TOC
mcsm_wts_toc <- TOcontrol(mcsm_wts, 0.6) # Optimal for 31-60

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)
MCSM15TOC <- Return.rebalancing(returns, mcsm_wts_toc)

# Combine MV, MES05TOC, MCSM05TOC gross cumulative returns
portf_ret_comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.5), MCSM15-TOC(0.6), Stocks 31-60",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 18 (2 minutes 36 seconds)

returns <- smallcapTS[ , 61:90]

# Optimize Portfolio at Monthly Rebalancing and 260-Day Training
rolling_window <- 260

## MV
opt_mv_result <- optimize_portfolio_MV_rebalance(returns = returns,  
                                                 rolling_window = rolling_window, 
                                                 rebalance_on = "months")

# Extract portfolio weights from the result
mv_wts <- opt_mv_result$wts
mv_wts <- mv_wts[complete.cases(mv_wts), ]

## MES05 (for tail probability 5%) 
opt_mes_result <- optimize_portfolio_MES_rebalance(returns = returns, alpha = 0.05, 
                                                   rolling_window = rolling_window, 
                                                   rebalance_on = "months")

# Extract portfolio weights from the result
mes_wts <- opt_mes_result$wts
mes_wts <- mes_wts[complete.cases(mes_wts), ]

## MES05-TOC
mes_wts_toc <- TOcontrol(mes_wts, 0.6) # Optimal for 61-90

## MCSM15 (for tail probability 15%) 
opt_mcsm_result <- optimize_portfolio_MCSM_rebalance(returns = returns, alpha = 0.15, 
                                                     rolling_window = rolling_window, 
                                                     rebalance_on = "months")

# Extract portfolio weights from the result
mcsm_wts <- opt_mcsm_result$wts
mcsm_wts <- mcsm_wts[complete.cases(mcsm_wts), ]

## MCSM15-TOC
mcsm_wts_toc <- TOcontrol(mcsm_wts, 0.8) # Optimal for 61-90

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(returns, mv_wts)
MES05TOC <- Return.rebalancing(returns, mes_wts_toc)
MCSM15TOC <- Return.rebalancing(returns, mcsm_wts_toc)

# Combine MV, MES05TOC, MCSM15TOC gross cumulative returns
portf_ret_comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(portf_ret_comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

backtest.plot(portf_ret_comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.6), MCSM15-TOC(0.8), Stocks 61-90",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))

