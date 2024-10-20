## This R script reproduces all the Exhibits in the paper 
## "Minimum Downside Risk Portfolios" by R.D. Martin,
## S. Stoyanov, X. Zhao and P. Sarker, published in the 
## Oct. 2024 issue of the Journal of Portfolio Management.

##
## Copy/paste this script into your own computer R file. Then
## run code lines 23 to 229, which creates functions needed in
## subsequent code that replicates the Exhibits. We recommend
## running the subsequent code in chunks to replicate each of
## the Exhibits in the JPM paper.

## Running Times for an Intel(R) Core(TM) i7-10750H Processor:
## Exhibits 1-5: 10 seconds, with 9 seconds for Exhibit 5
## Exhibits 6,8,10,12: Approximately 2 minutes each
## Exhibits 14,16,18: Approximately 3.5 minutes each
## Other Exhibits: Negligible

#### Load needed R functions
####

## divHHImat.R
divHHImat <- function(wtsmat){
  n <- nrow(wtsmat)
  if(n < 1){
    warning("empty data set")
    return()
  }
  diversification <- rep(0, n)
  for(i in 1:n){
    diversification[i] <- 1 - sum(wtsmat[i,]^2)
  }
  DIV <- diversification
  return(DIV)
}

## TOcontrol.R
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

## ToDivMES.R
ToDivMES <- function(x){
  ## wts: a list with xts objects wts.MV, wts.MES05, wts.MES05TOC
  
  # TO Values
  wts.MV <- x$wts.MV
  MV.TO <- 100*coredata(turnOver(wts.MV))
  muMV.TO <- round(mean(MV.TO), 1)
  sdMV.TO <- round(sd(MV.TO), 1)
  
  wts.MES05 <- x$wts.MES05
  MES05.TO <- 100*coredata(turnOver(wts.MES05))
  muMES05.TO <- round(mean(MES05.TO), 1)
  sdMES05.TO <- round(sd(MES05.TO), 1)
  
  wts.MES05TOC <- x$wts.MES05TOC
  MES05.TOC <- 100*coredata(turnOver(wts.MES05TOC))
  muMES05.TOC <- round(mean(MES05.TOC), 1)
  sdMES05.TOC <- round(sd(MES05.TOC), 1)
  
  # DIV Values
  MV.DIV <- 100*coredata(divHHI(wts.MV))
  muMV.DIV <- round(mean(MV.DIV), 1)
  sdMV.DIV <- round(sd(MV.DIV), 1)
  
  MES05.DIV <- 100*coredata(divHHI(wts.MES05))
  muMES05.DIV <- round(mean(MES05.DIV), 1)
  sdMES05.DIV <- round(sd(MES05.DIV), 1)
  
  MES05TOC.DIV <- 100*coredata(divHHI(wts.MES05TOC))
  muMES05TOC.DIV <- round(mean(MES05TOC.DIV), 1)
  sdMES05TOC.DIV <- round(sd(MES05TOC.DIV), 1)
  
  # TO and DIV data frame
  muSdTO_DIV <- rbind(c(muMV.TO, sdMV.TO, muMV.DIV, sdMV.DIV),
                      c(muMES05.TO, sdMES05.TO, muMES05.DIV, sdMES05.DIV),
                      c(muMES05.TOC, sdMES05.TOC, muMES05TOC.DIV, sdMES05TOC.DIV))
  muSdTO_DIV <- data.frame(muSdTO_DIV)
  names(muSdTO_DIV) <- c("TO Mean", "TO StdDev", "DIV Mean", "DIV StdDev")
  row.names(muSdTO_DIV) <- c("MV", "MES05", "MES05-TOC")
  return(muSdTO_DIV) 
}

## ToDivMCSM.R
ToDivMCSM <- function(x){
  ## wts: a list with xts objects wts.MV, wts.MES05, wts.MCSM15
  
  # TO Values
  wts.MV <- x$wts.MV
  MV.TO <- 100*coredata(turnOver(wts.MV))
  muMV.TO <- round(mean(MV.TO), 1)
  sdMV.TO <- round(sd(MV.TO), 1)
  
  wts.MES05 <- x$wts.MES05
  MES05.TO <- 100*coredata(turnOver(wts.MES05))
  muMES05.TO <- round(mean(MES05.TO), 1)
  sdMES05.TO <- round(sd(MES05.TO), 1)
  
  wts.MCSM15 <- x$wts.MCSM15
  MCSM15.TO <- 100*coredata(turnOver(wts.MCSM15))
  muMCSM15.TO <- round(mean(MCSM15.TO), 1)
  sdMCSM15.TO <- round(sd(MCSM15.TO), 1)
  
  # DIV Values
  MV.DIV <- 100*coredata(divHHI(wts.MV))
  muMV.DIV <- round(mean(MV.DIV), 1)
  sdMV.DIV <- round(sd(MV.DIV), 1)
  
  MES05.DIV <- 100*coredata(divHHI(wts.MES05))
  muMES05.DIV <- round(mean(MES05.DIV), 1)
  sdMES05.DIV <- round(sd(MES05.DIV), 1)
  
  MCSM15.DIV <- 100*coredata(divHHI(wts.MCSM15))
  muMCSM15.DIV <- round(mean(MCSM15.DIV), 1)
  sdMCSM15.DIV <- round(sd(MCSM15.DIV), 1)
  
  # TO and DIV data frame
  muSdTO_DIV <- rbind(c(muMV.TO, sdMV.TO, muMV.DIV, sdMV.DIV),
                      c(muMES05.TO, sdMES05.TO, muMES05.DIV, sdMES05.DIV),
                      c(muMCSM15.TO, sdMCSM15.TO, muMCSM15.DIV, sdMCSM15.DIV))
  muSdTO_DIV <- data.frame(muSdTO_DIV)
  names(muSdTO_DIV) <- c("TO Mean", "TO StdDev", "DIV Mean", "DIV StdDev")
  row.names(muSdTO_DIV) <- c("MV", "MES05", "MCSM15")
  return(muSdTO_DIV) 
}


## Pushpak function to calculate the mean and stdev of TO and DIV
## Here we changed Pushpak's function name MeanStd_TODIV to ToDivMeanSd

ToDivMeanSd <- function(weights_list){
  
  # List objects for storing turnover and diversification for all portfolios
  turnover_list <- list()
  diversification_list <- list()
  
  # Calculate turnover and diversification for all portfolios
  for (i in 1:length(weights_list)) {
    portfolio_name <- names(weights_list)[i]
    
    # Extract portfolio weights from the list object
    assign(paste0("wts_", portfolio_name), weights_list[[i]])
    
    # Calculate turnover and diversification and save in separate list objects
    turnover_list[[paste0("TO_", portfolio_name)]] <- 100*coredata(PCRA::turnOver(get(paste0("wts_", portfolio_name))))
    diversification_list[[paste0("DIV_", portfolio_name)]] <- 100*coredata(divHHI(get(paste0("wts_", portfolio_name))))
  }
  
  # Calculate mean of turnover
  mu_turnover_list <- lapply(turnover_list, mean)
  mu_turnover_list <- lapply(mu_turnover_list, round, 1)
  
  # Calculate standard deviation of turnover
  sd_turnover_list <- lapply(turnover_list, sd)
  sd_turnover_list <- lapply(sd_turnover_list, round, 1)
  
  # Calculate mean of diversification
  mu_diversification_list <- lapply(diversification_list, mean)
  mu_diversification_list <- lapply(mu_diversification_list, round, 1)
  
  # Calculate standard deviation of diversification
  sd_diversification_list <- lapply(diversification_list, sd)
  sd_diversification_list <- lapply(sd_diversification_list, round, 1)
  
  # Mean and stdev of turnover
  mu_turnover <- t(as.data.frame(mu_turnover_list))
  row.names(mu_turnover) <- names(weights_list)
  colnames(mu_turnover) <- "TO Mean"
  
  sd_turnover <- t(as.data.frame(sd_turnover_list))
  row.names(sd_turnover) <- names(weights_list)
  colnames(sd_turnover) <- "TO StdDev"
  
  # Mean and stdev of diversification
  mu_diversification <- t(as.data.frame(mu_diversification_list))
  row.names(mu_diversification) <- names(weights_list)
  colnames(mu_diversification) <- "DIV Mean"
  
  sd_diversification <- t(as.data.frame(sd_diversification_list))
  row.names(sd_diversification) <- names(weights_list)
  colnames(sd_diversification) <- "DIV StdDev"
  
  # Combine into a single dataframe
  muStd_TO_DIV <- cbind(mu_turnover, sd_turnover, mu_diversification, sd_diversification)
  
  return(muStd_TO_DIV)
}


## ratioFromThresholdTdist.R
ratioFromThresholdTdist <- function(eta = 1.0, df = 5)
{
  integrand.top <- function(x, eta)
    (x - eta) * dt(x, df)
  
  value.top <- integrate(integrand.top, eta, 
                         Inf, eta = eta)$value
  
  integrand.bottom <- function(x, eta)
    (x - eta)^2 * dt(x, df)
  
  value.bottom <- integrate(integrand.bottom, eta, Inf, 
                            eta = eta)$value
  ratio <- value.top/sqrt(value.bottom)
  ratio
}

## thresholdFromTailProbTdist.R
thresholdFromTailProbTdist <- function(qtl, df = 5,
                                       interval = c(1e-6, 20)) # 1e-6
{
  # Tail probabilities gamma = 1 - alpha, e.g., if alpha = 0.9,
  # then the upper tail probability is gamma = 0.1
  obj <- function(q, eta)
    q - ratioFromThresholdTdist(eta, df = df)
  
  uniroot(obj, interval = interval, q = qtl, check.conv = TRUE, tol = 1e-8)$root
}

#### End of functions needed for the following R script.
####

## Load data for Exhibits 1 - 5
library(PCRA)
library(xts)
stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2004-01-01", "2005-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", 
                "MktIndexCRSP", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")

returns <- returnsAll[ , 1:30]
RFts <- returnsAll[ , 108] 
RFmean <- mean(RFts)


## Exhibit 1

library(PortfolioAnalytics)
library(CVXR)
library(data.table)
pspec <- portfolio.spec(assets = names(returns))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(portfolio = pspecFI, type = "long_only")

## Mean-ES Long-Only Efront p = 5% with ES Ratio
p <- 0.05
pspecESLO <- add.objective(pspecLO, type = "risk", name = "ES",
                           arguments = list(p = p))
meanESlo.ef <- create.EfficientFrontier(returns, portfolio = pspecESLO, 
                                        type = "mean-ES")

xlim = c(0.0, 0.20)
ylim = c(-0.004, 0.015)
chart.EfficientFrontier(meanESlo.ef, match.col = "ES", type = "l",
                        chart.assets = TRUE, rf = RFmean,
                        labels.assets = FALSE, cex.assets = 1,
                        main = NULL,
                        RAR.text = "ES ratio", pch = 16, lwd = 1.5,
                        cex = 2.5, cex.axis = 1.1,
                        xlim = xlim, ylim = ylim) 


## Exhibit 2

efDat <- meanESlo.ef$frontier
efMat <- as.matrix(efDat[ , ])
dimnames(efMat)[[1]] <- 1:25
MU <- efMat[ , 1]
efWts <- efMat[ , -(1:3)]
DIV <- divHHImat(efWts)
plot(MU, DIV, type = "b", pch = 20, ylim = c(0,1))
abline(h = 0.9, lty = "dotted")


## Exhibit 3

p <- 0.05
pspecESLO <- add.objective(pspecLO, type = "risk", name = "ES",
                           arguments = list(p = p))
pspecLObox <- add.constraint(portfolio = pspecFI, type = "box", 
                             min = 0.02, max = 0.1)
pspecESLObox <- add.objective(pspecLObox, type = "risk", name = "ES",
                              arguments = list(p = p))
pspecLSbox <- add.constraint(portfolio = pspecFI, type = "box", 
                             min = -0.1, max = 0.1)
pspecESLSbox <- add.objective(pspecLSbox, type = "risk", name = "ES",
                              arguments = list(p = p))

portfList <- combine.portfolios(list(pspecESLSbox, pspecESLO, pspecESLObox))
legendLabels <- c("Long Short Box (-0.1, 0.1)", "Long Only", "Long Only Box (0.02, 0.1)") 

chart.EfficientFrontierOverlay(returns, portfolio_list = portfList, 
                               type = "mean-ES", match.col = "ES", 
                               legend.loc = "topright", chart.assets = TRUE,
                               legend.labels = legendLabels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(2, 2, 3),
                               col = c("dark green", "black", "dark red"),
                               lty = c("dashed", "solid", "dotted"),
                               xlim = xlim, ylim = ylim, main = NULL) 



## Exhibit 4

pspecES05 <- add.objective(portfolio = pspecLO, type = "risk", name = "ES", 
                           arguments = list(p=0.05))
pspecES25 <- add.objective(portfolio = pspecLO, type = "risk", name = "ES", 
                           arguments = list(p=0.25))
pspecES50 <- add.objective(portfolio = pspecLO, type = "risk", name = "ES", 
                           arguments = list(p=0.50))

# Combine the portfolios into a list
portfESlist <- combine.portfolios(list(pspecES50, pspecES25, pspecES05))

# Plot the efficient frontier overlay of the portfolios with varying tail probabilities
legendESlabels <- c("ES (p = 0.50)", "ES (p = 0.25)", "ES (p = 0.05)")

portfList <- combine.portfolios(list(pspecESLSbox, pspecESLO, pspecESLObox))
legendLabels <- c("Long Short Box (-0.1, 0.1)", "Long Only", "Long Only Box (0.02, 0.1)") 

chart.EfficientFrontierOverlay(returns, portfolio_list = portfESlist, 
                               type = "mean-ES", match.col = "ES", 
                               legend.loc = "topright", chart.assets = TRUE,
                               legend.labels = legendESlabels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(2,2,2),
                               col = c("dark green" , "black", "dark red"),
                               lty = c("solid", "dashed", "dotted"),
                               xlim = xlim, ylim = ylim, main = NULL)


## Exhibit 5 (9 seconds)

pspecESLO_050 <- add.objective(pspecLO, type = "risk", name = "ES",
                               arguments = list(p = 0.050))

# The follow function takes about 1 minute (check)
# reduce n.portfolios for faster, but less accurate, rendition
chart.EfficientFrontierCompare(returns, pspecESLO_050, risk_type = "ES", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("StdDev", "ES"),
                               n.portfolios = 10,
                               lwd=c(1.2, 1.2, 1.0, 1.0),
                               col = c(2,1,1,1), lty = c(2,1,4,4),
                               xlim = c(0.00, 0.08), ylim = c(0.0, 0.013), 
                               legend.loc = "topleft", main = NULL)
# A slightly better plot is obtained with larger n.portfolios, with very
# small % changes in Risk and Return


## Load packages

library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)

# Load CRSP daily smallcap returns for Exhibits 6-7, 8-9, 10-11

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


## Exhibit 6 (2 minutes and 0 seconds)

ret <- smallcapTS[ , 1:30]

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))

# Optimize Portfolio with Monthly Rebalancing
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.9) # Optimal for 1-30

# For table below
wts.comb21 <- list(wts.MV = wts.MV, wts.MES05 = wts.MES05, 
                   wts.MES05TOC = wts.MES05TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05 <- Return.rebalancing(ret, wts.MES05)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)

# Combine MV, MES05, MES05_TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05, MES05TOC, Market))
names(ret.comb) <- c("MV", "MES05", "MES05-TOC", "Market")

# For table below
ret.comb21 <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05, MES05-TOC(0.9), Stocks 1-30",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 7

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb21)

ret.comb21Short <- ret.comb21["2006/2014", ]

dat <- ret.comb21Short
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Exhibit 8 (1 minute and 55 seconds)

ret <- smallcapTS[ , 31:60]

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))

# Optimize Portfolio at Monthly Rebalancing and 500-Day Training
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.5) # Optimal for 31-60

# For table below
wts.comb22 <- list(wts.MV = wts.MV, wts.MES05 = wts.MES05, 
                   wts.MES05TOC = wts.MES05TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05 <- Return.rebalancing(ret, wts.MES05)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)

# Combine MV, MES05, MES05_TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05", "MES05-TOC", "Market")

# For table below
ret.comb22 <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05, MES05-TOC(0.5), Stocks 31-60",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 9

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb22)

ret.comb22Short <- ret.comb22["2006/2014", ]

dat <- ret.comb22Short
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Exhibit 10 (1 minute and 55 seconds)

ret <- smallcapTS[ , 61:90]

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))

# Optimize Portfolio at Monthly Rebalancing
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.5) # Optimal for 61-90

# For table below
wts.comb23 <- list(wts.MV = wts.MV, wts.MES05 = wts.MES05, 
                   wts.MES05TOC = wts.MES05TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05 <- Return.rebalancing(ret, wts.MES05)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)

# Combine MV, MES05, MES05_TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05", "MES05-TOC", "Market")

# For table below
ret.comb23 <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05, MES05-TOC(0.5), Stocks 61-90",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))



## Exhibit 11

# Create TO and DIV values data frame

muSdTO_DIV <- ToDivMeanSd(wts.comb23)

ret.comb23Short <- ret.comb23["2006/2014", ]
dat <- ret.comb23Short
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats

## Load packages again (not necessary)
library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)

## Load daily microcap returns for Exhibits 12-13

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


# Remove "MktIndexCRSP", "Ret13WkBill" from smallcapTS
microcapTS <- microcapTS[ , -c(35, 36)]


## Exhibit 12 (1 minute and 58 seconds)
ret <- microcapTS

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))

# Optimize Portfolio at Monthly Rebalancing
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.5)

# For table below
wts.comb24 <- list(wts.MV = wts.MV, wts.MES05 = wts.MES05, 
                   wts.MES05TOC = wts.MES05TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05 <- Return.rebalancing(ret, wts.MES05)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)

# Combine MV, MES05, MES05_TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05, MES05TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05", "MES05-TOC", "Market")

# For table below
ret.comb24 <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05, MES05-TOC(0.5), 34 Microcap Stocks",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))




## Exhibit 13

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb24)

ret.comb24Short <- ret.comb24["2006/2014", ]
dat <- ret.comb24Short
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Load packages again (not necessary to repeat)

library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)

# Load CRSP smallcap returns for Exhibits 14-15, 16-17, 18-19

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


## Exhibit 14 (3 minutes and 28 seconds)
ret <- smallcapTS[ , 1:30]

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))
pspecMCSM15 <- add.objective(pspecLO, type = "risk", name = "CSM", 
                             arguments = list(p=0.15))

# Optimize Portfolio with Monthly Rebalancing 
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

bt.MCSM15 <- optimize.portfolio.rebalancing(ret, pspecMCSM15,
                                            optimize_method = "CVXR",
                                            rebalance_on = "months",
                                            rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]
wts.MCSM15 <- extractWeights(bt.MCSM15)
wts.MCSM15 <- wts.MCSM15[complete.cases(wts.MCSM15),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.9) 
wts.MCSM15TOC <- TOcontrol(wts.MCSM15, 0.8) 

# For table below
wts.comb31TOC <- list(wts.MV = wts.MV, wts.MES05TOC = wts.MES05TOC, 
                      wts.MCSM15TOC = wts.MCSM15TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)
MCSM15TOC <- Return.rebalancing(ret, wts.MCSM15TOC)


# Combine MV, MES05, MES05_TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

# For table below
ret.comb31TOC <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.9), MCSM15-TOC(0.8), Stocks 1-30",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))


## Exhibit 15

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb31TOC)

ret.comb31TOCShort <- ret.comb31TOC["2006/2014", ]
dat <- ret.comb31TOCShort
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Exhibit 16 (3 minutes and 41 seconds)
ret <- smallcapTS[ , 31:60]

# Optimize Portfolio with Monthly Rebalancing 
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

bt.MCSM15 <- optimize.portfolio.rebalancing(ret, pspecMCSM15,
                                            optimize_method = "CVXR",
                                            rebalance_on = "months",
                                            rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]
wts.MCSM15 <- extractWeights(bt.MCSM15)
wts.MCSM15 <- wts.MCSM15[complete.cases(wts.MCSM15),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.5) 
wts.MCSM15TOC <- TOcontrol(wts.MCSM15, 0.6) 

# For table below
wts.comb32TOC <- list(wts.MV = wts.MV, wts.MES05TOC = wts.MES05TOC, 
                      wts.MCSM15TOC = wts.MCSM15TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)
MCSM15TOC <- Return.rebalancing(ret, wts.MCSM15TOC)


# Combine cumulative gross returns
ret.comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

# For table below
ret.comb32TOC <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.5), MCSM15-TOC(0.6), Stocks 31-60",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))



## Exhibit 17

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb32TOC)

ret.comb32TOCShort <- ret.comb32TOC["2006/2014", ]
dat <- ret.comb32TOCShort
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Exhibit 18 (3 minutes and 41 seconds)
ret <- smallcapTS[ , 61:90]

# Generate MV, and MES portfolios
pspec <- portfolio.spec(assets = names(ret))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(pspecFI, type = "long_only")
pspecMV <- add.objective(pspecLO, type = "risk", name = "var")
pspecMES05 <- add.objective(pspecLO, type = "risk", name = "ES",
                            arguments = list(p=0.05))
pspecMCSM15 <- add.objective(pspecLO, type = "risk", name = "CSM", 
                             arguments = list(p=0.15))

# Optimize Portfolio with Monthly Rebalancing 
window <- 260
bt.MV <- optimize.portfolio.rebalancing(ret, pspecMV,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = window)

bt.MES05 <- optimize.portfolio.rebalancing(ret, pspecMES05,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           rolling_window = window)

bt.MCSM15 <- optimize.portfolio.rebalancing(ret, pspecMCSM15,
                                            optimize_method = "CVXR",
                                            rebalance_on = "months",
                                            rolling_window = window)

# Extract time series of portfolio weights
wts.MV <- extractWeights(bt.MV)
wts.MV <- wts.MV[complete.cases(wts.MV),]
wts.MES05 <- extractWeights(bt.MES05)
wts.MES05 <- wts.MES05[complete.cases(wts.MES05),]
wts.MCSM15 <- extractWeights(bt.MCSM15)
wts.MCSM15 <- wts.MCSM15[complete.cases(wts.MCSM15),]

wts.MES05TOC <- TOcontrol(wts.MES05, 0.6) # Use this value of delta
wts.MCSM15TOC <- TOcontrol(wts.MCSM15, 0.8) # Use this value of delta

# For table below
wts.comb33TOC <- list(wts.MV = wts.MV, wts.MES05TOC = wts.MES05TOC, 
                      wts.MCSM15TOC = wts.MCSM15TOC)

# Compute cumulative returns of the portfolios
MV <- Return.rebalancing(ret, wts.MV)
MES05TOC <- Return.rebalancing(ret, wts.MES05TOC)
MCSM15TOC <- Return.rebalancing(ret, wts.MCSM15TOC)


# Combine MV, MES05, MES15TOC gross cumulative returns
ret.comb <- na.omit(merge(MV, MES05TOC, MCSM15TOC, Market, all=F))
names(ret.comb) <- c("MV", "MES05-TOC", "MCSM15-TOC", "Market")

# For table below
ret.comb33TOC <- ret.comb[ , -4]

backtest.plot(ret.comb, plotType = "cumRet",
              main = "MV, MES05-TOC(0.6), MCSM15-TOC(0.8), Stocks 61-90",
              colorSet = c("red","darkgreen","darkblue","black"), 
              ltySet = c(3, 1, 1, 1), lwdSet = c(0.7, 0.7, 0.7, 0.7))



## Exhibit 19

# Create TO and DIV values data frame
muSdTO_DIV <- ToDivMeanSd(wts.comb33TOC)

ret.comb33TOCShort <- ret.comb33TOC["2006/2014", ]
dat <- ret.comb33TOCShort
SR <- RPESE::SR.SE(dat)$SR
DSR <- RPESE::DSR.SE(dat)$DSR
SR_DSR <- data.frame(round(sqrt(252)*cbind(SR,DSR),2))

# Combine the two data frames
portStats <- data.frame(muSdTO_DIV, SR_DSR)
row.names(portStats) <- names(dat)
portStats


## Exhibt 20

## Thresholds from Tail Probs
thresholds <- function(dof, Z = FALSE)
{
  # Z = FALSE controls row.names suffix ".T" versus ".Z"
  tailProbs <- c(0.01, 0.05, 0.10, 0.15, 0.20)
  n <- length(tailProbs)
  ThresholdTdist <- rep(0,n)
  for(i in 1:n) {
    ThresholdTdist[i] <- thresholdFromTailProbTdist(tailProbs[i], df = dof)
  }
  rnd <- 5
  ThresholdTdist <- round(ThresholdTdist,rnd)
  
  # TupperBnd <- function(alpha) qt((1-(1 - alpha)^2), df = dof)
  TupperBnd <- function(tailProbs) qt((1-tailProbs^2), df = dof)
  
  Tquantiles <- round(qt(1 - tailProbs, df = dof),rnd)
  Tprobs <- round(1 - pt(ThresholdTdist, df = dof), rnd)
  # TupperBnd <- round(TupperBnd(1 - tailProbs),rnd)
  TupperBnd <- round(TupperBnd(tailProbs),rnd)
  
  UBprobs <- tailProbs^2
  dat <- data.frame(rbind(TupperBnd, ThresholdTdist, Tquantiles, UBprobs, Tprobs))
  if(Z == TRUE){
    row.names(dat) <- c("UpperBnd.Z", "Threshold.Z", "Quantile.Z", 
                        "UpperBndProbs.Z", "ThresholdProbs.Z")
  } else {
    row.names(dat) <- c("UpperBnd.T", "Threshold.T", "Quantile.T", 
                        "UpperBndProbs.T", "ThresholdProbs.T")
  }
  names(dat) <- c("1%", "5%", "10%", "15%", "20%")
  return(dat) # UBprobs relatively close to Tprob even with df = 3
}

thresh.Z <- thresholds(500, Z = TRUE)
thresh.T <- thresholds(10, Z = FALSE)
datOut <- rbind(thresh.Z, thresh.T)
datOut
