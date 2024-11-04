## Running Times for the following code with an Intel(R)
## Core(TM) i7-10750H Processor. Unless listed as follows, 
## sections take less than 10 seconds:
## Section  9.1:  3 min and 40 seconds
## Section  9.2:  4 min and 50 seconds
## Section 11.1:  1 min and 10 seconds
## Section   12:  1 min and 16 seconds


## Section 2

library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)
library(PCRA)


## SECTION 2.3

data(edhec)
class(edhec)
ret_edhec <- tail(edhec, 60) # Extract the last 5 years
range(index(edhec)) # Start and end dates of `edhec`
range(index(ret_edhec)) # Start and end dates of ret_edhec
# names(edhec) # Names of `edhec` long, so use shorter names
colnames(ret_edhec) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM", "LSE", "MA", "RV", "SS", "FF")
print(head(ret_edhec, 5))

tsPlotMP(ret_edhec, layout = c(2, 7))


## SECTION 3.1

# Create portfolio object
fund_edhec <- colnames(ret_edhec)
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


## SECTION 3.2

# Run the optimization with default solver
opt_maxret <- optimize.portfolio(R = ret_edhec, portfolio = pspec_maxret, optimize_method = "CVXR")
opt_maxret
opt_maxret$solver

# Run the optimization with a different solver
opt_maxret_glpk <- optimize.portfolio(R = ret_edhec, portfolio = pspec_maxret, optimize_method = c("CVXR", "GLPK"))
opt_maxret_glpk$solver

class(opt_maxret)

names(opt_maxret)

opt_maxret$weights

opt.outputMvo(opt_maxret, ret_edhec, digits =3)


## SECTION 3.3

bt_maxret <- optimize.portfolio.rebalancing(R = ret_edhec, portfolio = pspec_maxret,
                                            optimize_method = "CVXR",
                                            rebalance_on = "quarters", training_period = 36)

names(bt_maxret)

names(bt_maxret$opt_rebalancing)


## SECTION 4.1

# Create portfolio object
pspec_gmv <- portfolio.spec(assets = fund_edhec)
# Add full-investment constraint
pspec_gmv <- add.constraint(pspec_gmv, type = "full_investment")
# Add objective of minimizing variance
pspec_gmv <- add.objective(portfolio = pspec_gmv, type = "risk", name = "var")

opt_gmv <- optimize.portfolio(ret_edhec, pspec_gmv, optimize_method = "CVXR")
opt.outputMvo(opt_gmv, ret_edhec, digits =3)


## SECTION 4.2

# portfolio object
pspec_gmv <- add.constraint(pspec_gmv, type = "long_only")
pspec_gmvGroup <- add.constraint(pspec_gmv, type = "group",
                                 groups = list(groupA=1,
                                               groupB=c(2:12),
                                               groupC=13),
                                 group_min = c(0, 0.05, 0.05),
                                 group_max = c(0.4, 0.8, 0.5))
pspec_gmvGroup <- add.constraint(pspec_gmvGroup, type = "return", return_target = 0.003)
pspec_gmvGroup

# optimization
opt_gmvGroup <- optimize.portfolio(ret_edhec, pspec_gmvGroup,    optimize_method = "CVXR")
opt.outputMvo(opt_gmvGroup, ret_edhec, digits =3)

opt_gmvGroup_ecos <- optimize.portfolio(ret_edhec, pspec_gmvGroup, optimize_method = c("CVXR", "ECOS"))
opt.outputMvo(opt_gmvGroup_ecos, ret_edhec, digits =3)

opt_gmvGroup$solver
opt_gmvGroup_ecos$solver


## SECTION 5.1

pspec_qu <- portfolio.spec(assets = fund_edhec)
pspec_qu <- add.constraint(pspec_qu, type = "full_investment")
pspec_qu <- add.constraint(pspec_qu, type = "long_only")
# Add objectives
pspec_qu <- add.objective(portfolio = pspec_qu, type = "return", name = "mean")
pspec_qu <- add.objective(portfolio = pspec_qu, type = "risk", name = "var",
                          risk_aversion = 20)


## SECTION 5.2

opt_qu <- optimize.portfolio(ret_edhec, pspec_qu, optimize_method = "CVXR")
opt.outputMvo(opt_qu, ret_edhec, digits =3)


## SECTION 6.1

pspec_es <- portfolio.spec(assets = fund_edhec)
pspec_es <- add.constraint(pspec_es, type = "full_investment")
pspec_es <- add.constraint(pspec_es, type = "long_only")
# Add objective of minimizing ES by using the default gamma
pspec_gmes <- add.objective(portfolio = pspec_es, type = "risk", name = "ES") # Uses default tail probability 0.05
# Add objective of minimizing ES by using the specific gamma=0.1
pspec_gmes_1 <- add.objective(portfolio = pspec_es, type = "risk", name = "ES", arguments = list(p=0.1))


## SECTION 6.2

# GMES with default gamma=0.05
opt_gmes <- optimize.portfolio(ret_edhec, pspec_gmes, optimize_method = "CVXR")
opt_gmes
# GMES with specific gamma=0.1
opt_gmes_1 <- optimize.portfolio(ret_edhec, pspec_gmes_1, optimize_method = "CVXR")
opt_gmes_1


## SECTION 7.1

pspec_csm <- portfolio.spec(assets = fund_edhec)
pspec_csm <- add.constraint(pspec_csm, type = "full_investment")
pspec_csm <- add.constraint(pspec_csm, type = "long_only")
# Add objective of minimizing CSM
pspec_mcsm <- add.objective(portfolio = pspec_csm, type = "risk", name = "CSM",
                            arguments = list(p=0.05))


## SECTION 7.2

opt_mcsm <- optimize.portfolio(ret_edhec, pspec_mcsm, optimize_method = "CVXR")
opt_mcsm


## SECTION 8.1

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


## SECTION 8.2

# Create portfolio object
pspec_ESratio <- portfolio.spec(assets = fund_edhec)
## Add constraints of maximizing return per unit ES
pspec_ESratio <- add.constraint(pspec_ESratio, type = "full_investment")
pspec_ESratio <- add.constraint(pspec_ESratio, type = "long_only")
## Add objectives of maximizing return per unit ES
pspec_ESratio <- add.objective(pspec_ESratio, type = "return", name = "mean")
pspec_ESratio <- add.objective(pspec_ESratio, type = "risk", name = "ES", arguments = list(p=0.05))

# Optimization
optimize.portfolio(ret_edhec, pspec_ESratio, optimize_method = "CVXR", ESratio = TRUE)


## SECTION 8.3

# Create portfolio object
pspec_CSMratio <- portfolio.spec(assets = fund_edhec)
## Add constraints of maximizing return per unit CSM
pspec_CSMratio <- add.constraint(pspec_CSMratio, type = "full_investment")
pspec_CSMratio <- add.constraint(pspec_CSMratio, type = "long_only")
## Add objectives of maximizing return per unit CSM
pspec_CSMratio <- add.objective(pspec_CSMratio, type = "return", name = "mean")
pspec_CSMratio <- add.objective(pspec_CSMratio, type = "risk", name = "CSM",
                                arguments = list(p=0.05))

# Optimization
optimize.portfolio(ret_edhec, pspec_CSMratio, optimize_method = "CVXR", CSMratio = TRUE)


## Section 9

# Get daily returns of the 30 smallcap stocks
library(PCRA)
library(PortfolioAnalytics)
library(xts)
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

names(retD_CRSP)

# monthly return of top 30 stocks needed for monthly rebalancing
ep <- endpoints(retD_CRSP, on= "months", k=1)
prod1 <- function(x){apply(x+1, 2, prod)}
retM_CRSP <- period.apply(retD_CRSP, INDEX = ep, FUN = prod1) - 1


## SECTION 9.1

# Generate GMV, GMES and GMCSM portfolios
pspec_sc <- portfolio.spec(assets = sc30largest)
pspec_sc <- add.constraint(pspec_sc, type = "full_investment")
pspec_sc <- add.constraint(pspec_sc, type = "long_only")

pspec_GMV <- add.objective(pspec_sc, type = "risk", name = "var")
pspec_GMES <- add.objective(pspec_sc, type = "risk", name = "ES")
pspec_GMCSM <- add.objective(pspec_sc, type = "risk", name = "CSM")

# Optimize Portfolio at Monthly Rebalancing and 500-Day Training
bt.GMV <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMV,
                                         optimize_method = "CVXR",
                                         rebalance_on = "months",
                                         rolling_window = 500)
bt.ES <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMES,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = 500)
bt.CSM <- optimize.portfolio.rebalancing(retD_CRSP, pspec_GMCSM,
                                         optimize_method = "CVXR",
                                         rebalance_on = "months",
                                         rolling_window = 500)

# Extract time series of portfolio weights
wts.GMV <- extractWeights(bt.GMV)
wts.GMV <- wts.GMV[complete.cases(wts.GMV),]

wts.ES <- extractWeights(bt.ES)
wts.ES <- wts.ES[complete.cases(wts.ES),]

wts.CSM <- extractWeights(bt.CSM)
wts.CSM <- wts.CSM[complete.cases(wts.CSM),]

# Compute cumulative returns of three portfolios
GMV <- Return.rebalancing(retM_CRSP, wts.GMV)
ES <- Return.rebalancing(retM_CRSP, wts.ES)
CSM <- Return.rebalancing(retM_CRSP, wts.CSM)

# Combine GMV, ES and CSM portfolio cumulative returns
ret.comb <- na.omit(merge(GMV, ES, CSM, all=F))
names(ret.comb) <- c("GMV", "GMES", "GMCSM")

backtest.plot(ret.comb, colorSet = c("black", "darkblue", "darkgreen"), ltySet = c(3, 2, 1))


## SECTION 9.2

# Generate Sr, ESr and CSMr portfolios
pspec_sc_ratio <- add.objective(pspec_sc, type = "return", name = "mean")
pspec_Sr <- add.objective(pspec_sc_ratio, type = "risk", name = "var")
pspec_ESr <- add.objective(pspec_sc_ratio, type = "risk", name = "ES")
pspec_CSMr <- add.objective(pspec_sc_ratio, type = "risk", name = "CSM")

# Optimize Portfolio at Monthly Rebalancing and 500-Day Training
bt.Sr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_Sr, maxSR = TRUE,
                                        optimize_method = "CVXR",
                                        rebalance_on = "months",
                                        rolling_window = 500)
bt.ESr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_ESr,
                                         optimize_method = "CVXR",
                                         rebalance_on = "months",
                                         rolling_window = 500)
bt.CSMr <- optimize.portfolio.rebalancing(retD_CRSP, pspec_CSMr,
                                          optimize_method = "CVXR",
                                          rebalance_on = "months",
                                          rolling_window = 500)

# Extract time series of portfolio weights
wts.Sr <- extractWeights(bt.Sr)
wts.Sr <- wts.Sr[complete.cases(wts.Sr),]

wts.ESr <- extractWeights(bt.ESr)
wts.ESr <- wts.ESr[complete.cases(wts.ESr),]

wts.CSMr <- extractWeights(bt.CSMr)
wts.CSMr <- wts.CSMr[complete.cases(wts.CSMr),]

# Compute cumulative returns of three portfolios
Sr <- Return.rebalancing(retM_CRSP, wts.Sr, rebalance_on = "months")
ESr <- Return.rebalancing(retM_CRSP, wts.ESr, rebalance_on = "months")
CSMr <- Return.rebalancing(retM_CRSP, wts.CSMr, rebalance_on = "months")

# Combine Sr, ESr and CSMr portfolio cumulative returns
ret.comb.ratios <- na.omit(merge(Sr, ESr, CSMr, all=F))
names(ret.comb.ratios) <- c("Sharpe ratio", "ES ratio", "CSM ratio")

backtest.plot(ret.comb.ratios, colorSet = c("black", "darkblue", "darkgreen"), ltySet = c(3, 2, 1))


## SECTION 10

# monthly return of top 30 stocks in last 5 years
ep <- endpoints(retD_CRSP, on= "months", k=1)
prod1 <- function(x){apply(x+1, 2, prod)}
retM_CRSP <- period.apply(retD_CRSP, INDEX = ep, FUN = prod1) - 1
retM_CRSP_5 <- tail(retM_CRSP, 60)

tsPlotMP(retM_CRSP_5, layout = c(2,15), yname = "RETURNS", 
         stripText.cex = 0.7, axis.cex = 0.7)


## SECTION 10.1

# mean-var efficient frontier
pspec_sc <- portfolio.spec(names(retM_CRSP_5))
pspec_sc <- add.constraint(pspec_sc, type = "full_investment")
pspec_sc <- add.constraint(pspec_sc, type = "long_only")

meanvar.ef <- create.EfficientFrontier(R = retM_CRSP_5, 
                                       portfolio = pspec_sc, type = "mean-StdDev")

chart.EfficientFrontier(meanvar.ef, match.col = "StdDev", type = "l",
                        chart.assets = FALSE, main = NULL,
                        RAR.text = "Max Sharpe ratio", pch = 1)

meanvar.ef$frontier[, 1:2]

sr <- meanvar.ef$frontier[, 1]/meanvar.ef$frontier[, 2]
maximumSR <- max(sr)
meanMaxSR <-  meanvar.ef$frontier[, 1][sr == max(sr)]
stdevMaxSR <- meanvar.ef$frontier[, 2][sr == max(sr)]
dat <- (round(c(maximumSR, meanMaxSR, stdevMaxSR), 3))
dat <- data.frame(dat)
names(dat) <- NULL
row.names(dat) <- c("maximum SR", "maxSRport Mean", "maxSRport Stdev")
dat

# Mean-StdDev Efficient Frontier
pspec_MV <- add.objective(pspec_sc, type = "risk", name = "var")
pspec_MV <- add.objective(portfolio = pspec_MV, type = "return", name = "mean")
opt_MV <- optimize.portfolio(retM_CRSP_5, pspec_MV, optimize_method = "CVXR", maxSR = TRUE)
opt.outputMvo(opt_MV, retM_CRSP_5, annualize = FALSE, digits = 3)


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
legend_labels <- c("Long Only", "Long Only Box (0.02,0.1)", "Long Short Box (-0.01,0.1)")
chart.EfficientFrontierOverlay(R = retM_CRSP_5, portfolio_list = portf_list, 
                               type = "mean-StdDev", match.col = "StdDev", 
                               legend.loc = "bottomright", chart.assets = FALSE,
                               legend.labels = legend_labels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(3,3,3),
                               col = c("black", "dark red", "dark green"),
                               main = NULL,
                               xlim = c(0.03, 0.11), ylim = c(0.005, 0.035))


## SECTION 10.2

# Mean-ES Efficient Frontier
meanes.ef <- create.EfficientFrontier(R = retM_CRSP_5, portfolio = pspec_sc, type = "mean-ES")
chart.EfficientFrontier(meanes.ef, match.col = "ES", type = "l",
                        chart.assets = FALSE, main = NULL,
                        RAR.text = "Max ES ratio", pch = 1)

legend_labels <- c("Long Only ES (p=0.05)", 
                   "Long Only Box ES (p=0.05)", "Long Short Box ES (p=0.05)")
chart.EfficientFrontierOverlay(R = retM_CRSP_5, portfolio_list = portf_list, 
                               type = "mean-ES", match.col = "ES",
                               legend.loc = "bottomright", chart.assets = FALSE,
                               legend.labels = legend_labels, cex.legend = 1,
                               labels.assets = FALSE, lwd = c(3,3,3),
                               col = c("black", "dark red", "dark green"),
                               main = NULL,
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
                               main = NULL,
                               xlim = c(0.035, 0.165), ylim = c(0.005, 0.03))


## SECTION 10.3

# Mean-CSM Efficient Frontier
meancsm.ef <- create.EfficientFrontier(R = retM_CRSP_5, portfolio = pspec_sc, 
                                       type = "mean-CSM")
chart.EfficientFrontier(meancsm.ef, match.col = "CSM", type = "l",
                        chart.assets = FALSE, main = NULL,
                        RAR.text = "Max CSM ratio", pch = 1)


## SECTION 11.1

args(chart.EfficientFrontierCompare)

# Compare StdDev of minStd and minES portfolios with guideline
chart.EfficientFrontierCompare(R = retM_CRSP_5, portfolio = pspec_sc, risk_type = "StdDev", match.col = c("StdDev", "ES"), lwd = c(2, 2), xlim = c(0.0,0.14), ylim = c(0.005,0.032))

# Compare ES of minStd and minES portfolios with guideline
chart.EfficientFrontierCompare(R = retM_CRSP_5, portfolio = pspec_sc, 
                               risk_type = "ES", match.col = c("ES", "StdDev"), lwd = c(2, 2), 
                               xlim = c(0.0,0.14), ylim = c(0.005,0.032))

# Compare ES of minStd, minES and minCSM portfolios without guideline
chart.EfficientFrontierCompare(R = retM_CRSP_5, portfolio = pspec_sc, 
                               risk_type = "ES",match.col = c("StdDev", "ES", "CSM"), 
                               guideline = FALSE, col = c("darkred","darkgreen","blue"), 
                               lty = c("dotted", "solid", "dashed"), lwd = c(1, 1.5, 1.5))


## SECTION 11.2

minStdDev_port <- add.objective(pspec_sc, type = "risk", name = "StdDev")
minStdDev_opt <- optimize.portfolio(retM_CRSP_5, minStdDev_port,
                                    optimize_method = "CVXR")
minStdDev_w <- minStdDev_opt$weight

minES_port <- add.objective(pspec_sc, type = "risk", name = "ES")
minES_opt <- optimize.portfolio(retM_CRSP_5, minES_port, 
                                optimize_method = "CVXR")
minES_w <- minES_opt$weight

# Extract risk StdDev portfolio
extract_risk(retM_CRSP_5, minStdDev_w)

# Extract risk ES portfolio
extract_risk(retM_CRSP_5, minES_w)


minStdDev_opt_covRob <-optimize.portfolio(retM_CRSP_5,minStdDev_port,
                                          optimize_method =
                                            "CVXR", momentFUN = 'custom.covRob.Mcd')
extract_risk(retM_CRSP_5, minStdDev_w, moment_setting =
               minStdDev_opt_covRob$moment_values)


## SECTION 12

# Compare mean-var frontiers with classic and robust
# covariance matrix estimators.
sampleCov = meanvar.efficient.frontier(pspec_sc, retM_CRSP_5, optimize_method = 'CVXR')
robustCov = meanvar.efficient.frontier(pspec_sc, retM_CRSP_5, optimize_method = 'CVXR', momentFUN = 'custom.covRob.TSGS')
plotFrontiers(retM_CRSP_5, frontiers=list(sampleCov, robustCov), risk='StdDev')

