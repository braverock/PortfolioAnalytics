## 2 GETTING STARTED


# load package
library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)

## load data
data(edhec)

# Use edhec for a returns object
ret_edhec <- tail(edhec, 60)
colnames(ret_edhec) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", 
                       "GM", "LSE", "MA", "RV", "SS", "FF")
print(head(ret_edhec, 5))

# Get a character vector of the asset names
fund_edhec <- colnames(ret_edhec)

# Time Series Plot of Edhec Return
tsPlotMP(ret_edhec, layout = c(2, 7), main = "Time Series Plot of Edhec Return")


## 3 MAXIMIZING MEAN RETURN


# Create portfolio object
pspec_maxret <- portfolio.spec(assets = fund_edhec)
# Add constraints to the portfolio object
pspec_maxret <- add.constraint(pspec_maxret, type = "full_investment")
pspec_maxret <- add.constraint(portfolio = pspec_maxret, type = "box",
                              min = rep(0.02, 13),
                              max = c(rep(0.15, 8), rep(0.1, 5)))
# Add objective to the portfolio object
pspec_maxret <- add.objective(portfolio = pspec_maxret, 
                             type = "return", name = "mean")
pspec_maxret


# Run the optimization with default solver
opt_maxret <- optimize.portfolio(R = ret_edhec, portfolio = pspec_maxret, 
                                 optimize_method = "CVXR", trace = TRUE)
opt_maxret
opt_maxret$solver

# Run the optimization with specific solver
opt_maxret_glpk <- optimize.portfolio(R = ret_edhec, portfolio = pspec_maxret, 
                                 optimize_method = c("CVXR", "GLPK"), trace = TRUE)
opt_maxret_glpk$solver

# Run back-testing
bt_maxret <- optimize.portfolio.rebalancing(R = ret_edhec, portfolio = pspec_maxret,
                                            optimize_method = "CVXR",
                                            rebalance_on = "quarters",
                                            training_period = 36)

# show results
class(bt_maxret)
names(bt_maxret)


## 4 MINIMIZING VARIANCE


# Create portfolio object
pspec_gmv <- portfolio.spec(assets = fund_edhec)
# Add full-investment constraint
pspec_gmv <- add.constraint(pspec_gmv, type = "full_investment")
# Add objective of minimizing variance
pspec_gmv <- add.objective(portfolio = pspec_gmv, type = "risk", name = "var")

## Run the optimization with default CVXR solver
opt_gmv <- optimize.portfolio(ret_edhec, pspec_gmv, optimize_method = "CVXR")
opt_gmv

# portfolio object
pspec_mv <- add.constraint(pspec_gmv, type = "long_only")
pspec_mv <- add.constraint(pspec_mv, type = "group",
                            groups = list(groupA=1,
                                          groupB=c(2:12),
                                          groupC=13),
                            group_min = c(0, 0.05, 0.05),
                            group_max = c(0.4, 0.8, 0.5))
pspec_mv <- add.constraint(pspec_mv, type = "return", return_target = 0.003)
pspec_mv

# optimization
opt_mv <- optimize.portfolio(ret_edhec, pspec_mv, optimize_method = "CVXR")
opt_mv

## Run the optimization with specific CVXR solver
opt_mv_ecos <- optimize.portfolio(ret_edhec, pspec_mv, optimize_method = c("CVXR", "ECOS"))
opt_mv_ecos
opt_mv$solver
opt_mv_ecos$solver


## 5 MAXIMIZING QUADRATIC UTILITY


# Generate MVO portfolio
pspec_mvo <- portfolio.spec(assets = fund_edhec)
pspec_mvo <- add.constraint(pspec_mvo, type = "full_investment")
pspec_mvo <- add.constraint(pspec_mvo, type = "long_only")
# Add objectives
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "return", name = "mean")
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "risk", name = "var",
                           risk_aversion = 20)


## Run optimization
opt_mvo <- optimize.portfolio(ret_edhec, pspec_mvo, optimize_method = "CVXR")
opt_mvo


## 6 MINIMIZING EXPECTED SHORTFALL


# Generate min-ES portfolio
pspec_es <- portfolio.spec(assets = fund_edhec)
pspec_es <- add.constraint(pspec_es, type = "full_investment")
pspec_es <- add.constraint(pspec_es, type = "long_only")
# Add objective of minimizing ES by using the default gamma
pspec_es <- add.objective(portfolio = pspec_es, type = "risk", name = "ES")
# Add objective of minimizing ES by using the specific gamma=0.1
pspec_es_1 <- add.objective(portfolio = pspec_es, type = "risk", name = "ES",
                          arguments = list(p=0.1))

# GMES with default gamma=0.05
opt_es <- optimize.portfolio(ret_edhec, pspec_es, optimize_method = "CVXR")
opt_es
# GMES with specific gamma=0.1
opt_es_1 <- optimize.portfolio(ret_edhec, pspec_es_1, optimize_method = "CVXR")
opt_es_1


## 7 MINIMIZING EXPECTED QUADRATIC SHORTFALL


# Generate min-EQS portfolio
pspec_eqs <- portfolio.spec(assets = fund_edhec)
pspec_eqs <- add.constraint(pspec_eqs, type = "full_investment")
pspec_eqs <- add.constraint(pspec_eqs, type = "long_only")
# Add objective of minimizing EQS
pspec_eqs <- add.objective(portfolio = pspec_eqs, type = "risk", name = "EQS",
                          arguments = list(p=0.05))

# GMEQS with default gamma=0.05
opt_eqs <- optimize.portfolio(ret_edhec, pspec_eqs, optimize_method = "CVXR")
opt_eqs


## 8 MAXIMIZING MEAN RETURN PER UNIT RISK


# Create portfolio object
pspec_sr <- portfolio.spec(assets = fund_edhec)
## Add constraints of maximizing Sharpe Ratio
pspec_sr <- add.constraint(pspec_sr, type = "full_investment")
pspec_sr <- add.constraint(pspec_sr, type = "long_only")
## Add objectives of maximizing Sharpe Ratio
pspec_sr <- add.objective(pspec_sr, type = "return", name = "mean")
pspec_sr <- add.objective(pspec_sr, type = "risk", name = "var")

# Optimization
optimize.portfolio(ret_edhec, pspec_sr, optimize_method = "CVXR", maxSR = TRUE)

# Create portfolio object
pspec_ESratio <- portfolio.spec(assets = fund_edhec)
## Add constraints of maximizing return per unit ES
pspec_ESratio <- add.constraint(pspec_ESratio, type = "full_investment")
pspec_ESratio <- add.constraint(pspec_ESratio, type = "long_only")
## Add objectives of maximizing return per unit ES
pspec_ESratio <- add.objective(pspec_ESratio, type = "return", name = "mean")
pspec_ESratio <- add.objective(pspec_ESratio, type = "risk", name = "ES",
                               arguments = list(p=0.05))

# Optimization
optimize.portfolio(ret_edhec, pspec_ESratio, optimize_method = "CVXR", ESratio = TRUE)

# Create portfolio object
pspec_EQSratio <- portfolio.spec(assets = fund_edhec)
## Add constraints of maximizing return per unit EQS
pspec_EQSratio <- add.constraint(pspec_EQSratio, type = "full_investment")
pspec_EQSratio <- add.constraint(pspec_EQSratio, type = "long_only")
## Add objectives of maximizing return per unit EQS
pspec_EQSratio <- add.objective(pspec_EQSratio, type = "return", name = "mean")
pspec_EQSratio <- add.objective(pspec_EQSratio, type = "risk", name = "EQS",
                                arguments = list(p=0.05))

# Optimization
optimize.portfolio(ret_edhec, pspec_EQSratio, optimize_method = "CVXR", EQSratio = TRUE)


## 9 COMPARATIVE PERFORMANCE OF PORTFOLIOS


# use CRSP daily data set
stocksCRSPdaily <- getPCRAData(dataset = "stocksCRSPdaily")
smallcapTS <- selectCRSPandSPGMI(
  periodicity = "daily",
  stockItems = c("Date", "TickerLast", "CapGroupLast", "Return"),
  factorItems = NULL,
  subsetType = "CapGroupLast",
  subsetValues = "SmallCap",
  outputType = "xts")

# find top 30 small cap stocks based on the market capitalization
smallcapDT <- factorsSPGMI[CapGroupLast == "SmallCap"]
scSize <- smallcapDT[, mean(LogMktCap), by = "TickerLast"]
names(scSize)[2] <- "Size"
scSize <- scSize[order(scSize$Size, decreasing = TRUE),]
sc30largest <- scSize[,TickerLast][1:30]

# daily return of top 30 stocks
retD_CRSP <- smallcapTS[ , sc30largest]
print(head(retD_CRSP, 3))

# monthly return of top 30 stocks in last 5 years
ep <- endpoints(retD_CRSP, on= "months", k=1)
prod1 <- function(x){apply(x+1, 2, prod)}
retM_CRSP <- period.apply(retD_CRSP, INDEX = ep, FUN = prod1) - 1
retM_CRSP_5 <- tail(retM_CRSP, 60)

# time series plot of 10 stocks
tsPlotMP(retM_CRSP_5[, 1:10])

# Test run time for Backtesting with GMV, GMES, GMEQS portfolios
start_time1 <- Sys.time() 

# Generate GMV, GMES and GMEQS portfolios
pspec_sc <- portfolio.spec(assets = sc30largest)
pspec_sc <- add.constraint(pspec_sc, type = "full_investment")
pspec_sc <- add.constraint(pspec_sc, type = "long_only")
pspec_GMV <- add.objective(pspec_sc, type = "risk", name = "var")
pspec_GMES <- add.objective(pspec_sc, type = "risk", name = "ES")
pspec_GMEQS <- add.objective(pspec_sc, type = "risk", name = "EQS")

# Optimize Portfolio at Monthly Rebalancing and 500-Day Training
bt.GMV <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMV,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)
bt.ES <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMES,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)
bt.EQS <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMEQS,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)

# Extract time series of portfolio weights
wts.GMV <- extractWeights(bt.GMV)
wts.GMV <- wts.GMV[complete.cases(wts.GMV),]
wts.ES <- extractWeights(bt.ES)
wts.ES <- wts.ES[complete.cases(wts.ES),]
wts.EQS <- extractWeights(bt.EQS)
wts.EQS <- wts.EQS[complete.cases(wts.EQS),]

# Compute cumulative returns of three portfolios
GMV <- Return.rebalancing(retM_CRSP, wts.GMV)
ES <- Return.rebalancing(retM_CRSP, wts.ES)
EQS <- Return.rebalancing(retM_CRSP, wts.EQS)

# Combine GMV, ES and EQS portfolio cumulative returns
ret.comb <- na.omit(merge(GMV, ES, EQS, all=F))
names(ret.comb) <- c("GMV", "GMES", "GMEQS")

backtest.plot(ret.comb, colorSet = c("black", "darkblue", "darkgreen"), 
              ltySet = c(3, 2, 1))

# Return run-time for Backtesting with GMV, GMES, GMEQS portfolios
end_time1 <- Sys.time()
runningtime1 <- end_time1 - start_time1
cat("The run time for Figure 9.2 is", format(round(runningtime1, 2)))

# Test run-time for Backtesting with SR, ESratio, EQSratio portfolios
start_time2 <- Sys.time()

# Generate GMV, GMES and GMEQS portfolios
pspec_sc_ratio <- add.objective(pspec_sc, type = "return", name = "mean")
pspec_Sr <- add.objective(pspec_sc_ratio, type = "risk", name = "var")
pspec_ESr <- add.objective(pspec_sc_ratio, type = "risk", name = "ES")
pspec_EQSr <- add.objective(pspec_sc_ratio, type = "risk", name = "EQS")

# Optimize Portfolio at Monthly Rebalancing and 500-Day Training
bt.Sr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_Sr, maxSR = TRUE,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)
bt.ESr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_ESr,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)
bt.EQSr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_EQSr,
                                           optimize_method = "CVXR",
                                           rebalance_on = "months",
                                           training_period = 30,
                                           rolling_window = 500)

# Extract time series of portfolio weights
wts.Sr <- extractWeights(bt.Sr)
wts.Sr <- wts.Sr[complete.cases(wts.Sr),]
wts.ESr <- extractWeights(bt.ESr)
wts.ESr <- wts.ESr[complete.cases(wts.ESr),]
wts.EQSr <- extractWeights(bt.EQSr)
wts.EQSr <- wts.EQSr[complete.cases(wts.EQSr),]

# Compute cumulative returns of three portfolios
Sr <- Return.rebalancing(retM_CRSP, wts.Sr, rebalance_on = "months")
ESr <- Return.rebalancing(retM_CRSP, wts.ESr, rebalance_on = "months")
EQSr <- Return.rebalancing(retM_CRSP, wts.EQSr, rebalance_on = "months")

# Combine Sr, ESr and EQSr portfolio cumulative returns
ret.comb <- na.omit(merge(Sr, ESr, EQSr, all=F))
names(ret.comb) <- c("Sharpe ratio", "ES ratio", "EQS ratio")
backtest.plot(ret.comb, colorSet = c("black", "darkblue", "darkgreen"), 
              ltySet = c(3, 2, 1))

# Return run-time for Backtesting with SR, ESratio, EQSratio portfolios
end_time2 <- Sys.time()
runningtime2 <- end_time2 - start_time2
cat("The run time for Figure 9.3 is", format(round(runningtime2, 2)))

# mean-var efficient frontier
meanvar.ef <- create.EfficientFrontier(R = retM_CRSP_5, portfolio = pspec_sc, 
                                       type = "mean-StdDev")
meanvar.ef
chart.EfficientFrontier(meanvar.ef, match.col = "StdDev", type = "l",
                        chart.assets = FALSE, main = "Mean-StdDev Efficient Frontier",
                        RAR.text = "Sharpe ratio", pch = 1)

# Show Efficient Frontier Data
meanvar.ef$frontier[, 1:2]
sr = meanvar.ef$frontier[, 1]/meanvar.ef$frontier[, 2]
cat("maximum Sharpe ratio:", max(sr))
cat("mean of the maximum SR portfolio:", meanvar.ef$frontier[, 1][sr == max(sr)])
cat("StdDev of the maximum SR portfolio:", meanvar.ef$frontier[, 2][sr == max(sr)])

# Generate Mean-StdDev Efficient Frontier
pspec_MV <- add.objective(pspec_sc, type = "risk", name = "var")
pspec_MV <- add.objective(portfolio = pspec_MV, type = "return", name = "mean")
opt_MV <- optimize.portfolio(retM_CRSP_5, pspec_MV, optimize_method = "CVXR", 
                              maxSR = TRUE, trace = TRUE)
opt_MV

# Show Mean-StdDev Efficient Frontier
chart.EfficientFrontier(opt_MV, match.col = "StdDev", chart.assets = FALSE, 
                        main = "Mean-StdDev Efficient Frontier",
                        RAR.text = "Sharpe Ratio", pch = 1, xlim = c(0, 0.1))

# Initialize Portfolio
pspec_sc_init <- portfolio.spec(assets = sc30largest)
pspec_sc_init <- add.constraint(pspec_sc_init, type = "full_investment")

# Portfolio with long-only constraints
pspec_sc_lo <- add.constraint(portfolio = pspec_sc_init, type = "long_only")

# Portfolio with long-only box constraints
pspec_sc_lobox <- add.constraint(portfolio = pspec_sc_init, type = "box", 
                                 min = 0.02, max = 0.1)

# Portfolio with long-short box constraints
pspec_sc_lsbox <- add.constraint(portfolio = pspec_sc_init, type = "box", 
                                 min = -0.1, max = 0.1)

# Combine the portfolios into a list
portf_list <- combine.portfolios(list(pspec_sc_lo, pspec_sc_lobox, pspec_sc_lsbox))

# Plot the efficient frontier overlay of the portfolios with varying constraints
legend_labels <- c("Long Only", "Long Only Box", "Long Short Box")
chart.EfficientFrontierOverlay(R = retM_CRSP_5, portfolio_list = portf_list, 
                               type = "mean-StdDev", match.col = "StdDev", 
                               legend.loc = "bottomright", chart.assets = FALSE,
                               legend.labels = legend_labels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(3,3,3),
                               col = c("black", "dark red", "dark green"),
                               main = "Overlay Mean-StdDev Efficient Frontiers",
                               xlim = c(0.03, 0.11), ylim = c(0.005, 0.035))

# Mean-ES Efficient Frontier
meanetl.ef <- create.EfficientFrontier(R = retM_CRSP_5, portfolio = pspec_sc, 
                                       type = "mean-ES")
chart.EfficientFrontier(meanetl.ef, match.col = "ES", type = "l",
                        chart.assets = FALSE, main = "Mean-ES Efficient Frontier",
                        RAR.text = "ES ratio", pch = 1)

# Overlay Mean-ES Efficient Frontiers
legend_labels <- c("Long Only ES (p=0.05)", 
                   "Long Only Box ES (p=0.05)", "Long Short Box ES (p=0.05)")
chart.EfficientFrontierOverlay(R = retM_CRSP_5, portfolio_list = portf_list, 
                               type = "mean-ES", match.col = "ES",
                               legend.loc = "bottomright", chart.assets = FALSE,
                               legend.labels = legend_labels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(3,3,3),
                               col = c("black", "dark red", "dark green"),
                               main = "Overlay Mean-ES Efficient Frontiers",
                               xlim = c(0.03, 0.17), ylim = c(0.005, 0.035))

# Create long-only ES portfolios with different tail probabilities
ES_05 <- add.objective(portfolio = pspec_sc_lo, type = "risk", name = "ES", 
                       arguments = list(p=0.05))
ES_10 <- add.objective(portfolio = pspec_sc_lo, type = "risk", name = "ES", 
                       arguments = list(p=0.1))
ES_15 <- add.objective(portfolio = pspec_sc_lo, type = "risk", name = "ES", 
                       arguments = list(p=0.15))

# Combine the portfolios into a list
portf_ES_list <- combine.portfolios(list(ES_05, ES_10, ES_15))

# Plot the efficient frontier overlay of the portfolios with varying tail probabilities
legend_ES_labels <- c("ES (p=0.05)", "ES (p=0.1)", "ES (p=0.15)")
chart.EfficientFrontierOverlay(R = retM_CRSP_5, portfolio_list = portf_ES_list, 
                               type = "mean-ES", match.col = "ES", 
                               legend.loc = "bottomright", chart.assets = FALSE,
                               legend.labels = legend_ES_labels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(3,3,3),
                               col = c("black", "dark red", "dark green"),
                               main = "Overlay Mean-ES Efficient Frontiers",
                               xlim = c(0.035, 0.165), ylim = c(0.005, 0.03))

# Mean-EQS Efficient Frontier
meaneqs.ef <- create.EfficientFrontier(R = retM_CRSP_5, portfolio = pspec_sc, 
                                       type = "mean-EQS")
chart.EfficientFrontier(meaneqs.ef, match.col = "EQS", type = "l",
                        chart.assets = FALSE, main = "Mean-EQS Efficient Frontier",
                        RAR.text = "EQS ratio", pch = 1)

# usage example: minStd Portfolio
minstd_port <- add.objective(pspec_sc, type = "risk", name = "StdDev")
minstd_w <- optimize.portfolio(retM_CRSP_5, minstd_port, optimize_method = "CVXR")$weight

# risk values with default alpha = 0.05
extract_risk(retM_CRSP_5, minstd_w)

# risk values with specific alpha
extract_risk(retM_CRSP_5, minstd_w, ES_alpha = 0.1, EQS_alpha = 0.1)

# example 1: Compare StdDev of minStd and minES portfolios with guideline
chart.EfficientFrontierCompare(R = retM_CRSP_5, portfolio = pspec_sc, risk_type = "StdDev",
                               match.col = c("StdDev", "ES"), lwd = c(2, 2))

# example 2: Compare ES of minStd, minES and minEQS portfolios without guideline
chart.EfficientFrontierCompare(R = retM_CRSP_5, portfolio = pspec_sc, risk_type = "ES",
                               match.col = c("StdDev", "ES", "EQS"), guideline = FALSE,
                               col = c(1,2,4), lty = c(1, 2, 4), lwd = c(2, 2, 2))

