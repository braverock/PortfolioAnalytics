## ----setup, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
Sys.setlocale("LC_TIME", "English")


## ---- message=FALSE----------------------------------------------------------------------------------------------------------
library(PortfolioAnalytics)
library(CVXR)
library(data.table)
library(xts)


## ----------------------------------------------------------------------------------------------------------------------------
data(edhec)
# Use the first 4 columns in edhec for a returns object
returns <- edhec[, 1:4]
colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))
# Get a character vector of the asset names
funds <- colnames(returns)


## ----------------------------------------------------------------------------------------------------------------------------
load("stocksCRSPdaily.rda")

stocks = stocksCRSPdaily[CapGroup == "SmallCap"]
returnMat = tapply(stocks[, ReturnD], list(stocks$Date, stocks$TickerLast), I)
smallcapD = xts(returnMat, as.Date(rownames(returnMat)))
sc_30 = c("TGNA", "AVP", "PBI", "THC", "AVY", "HAS", "TSS", "SPXC", "R", "HP", "J", 
          "DBD", "HAR", "BIG", "HSC", "MLHR", "AXE", "MATX", "KBH", "BGG", "CRS", 
          "UVV", "MENT", "HTLD", "BRC", "FUL", "ESND", "BOBE", "PIR", "WTS")
returns_sc = smallcapD[, sc_30]
print(head(returns_sc, 3))
funds_sc = colnames(returns_sc)


## ----------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_maxret <- portfolio.spec(assets=funds)
# Add constraints to the portfolio object
pspec_maxret <- add.constraint(pspec_maxret, type="full_investment")
pspec_maxret <- add.constraint(portfolio = pspec_maxret, type = "box",
                              min = c(0.02, 0.05, 0.03, 0.02),
                              max = c(0.55, 0.6, 0.65, 0.5))
# Add objective to the portfolio object
pspec_maxret <- add.objective(portfolio = pspec_maxret, 
                             type = "return", name = "mean")
pspec_maxret


## ----------------------------------------------------------------------------------------------------------------------------
# Run the optimization with default solver
opt_maxret <- optimize.portfolio(R=returns, portfolio=pspec_maxret, 
                                 optimize_method="CVXR", trace=TRUE)
opt_maxret
opt_maxret$solver

# Run the optimization with specific solver
opt_maxret_glpk <- optimize.portfolio(R=returns, portfolio=pspec_maxret, 
                                 optimize_method=c("CVXR", "GLPK"), trace=TRUE)
opt_maxret_glpk$solver


## ----warning=FALSE-----------------------------------------------------------------------------------------------------------
bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_maxret,
                                            optimize_method="CVXR",
                                            rebalance_on="quarters",
                                            training_period=36)


## ----------------------------------------------------------------------------------------------------------------------------
class(bt_maxret)
names(bt_maxret)


## ----------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_gmv <- portfolio.spec(assets=funds)
# Add full-investment constraint
pspec_gmv <- add.constraint(pspec_gmv, type="full_investment")
# Add objective of minimizing variance
pspec_gmv <- add.objective(portfolio = pspec_gmv, type = "risk", name = "var")


## ----------------------------------------------------------------------------------------------------------------------------
opt_gmv <- optimize.portfolio(returns, pspec_gmv, optimize_method = "CVXR")
opt_gmv


## ----------------------------------------------------------------------------------------------------------------------------
# portfolio object
pspec_mv <- add.constraint(pspec_gmv, type = "long_only")
pspec_mv <- add.constraint(pspec_mv, type = "group",
                            groups=list(groupA=1,
                                        groupB=c(2, 3),
                                        groupC=4),
                            group_min=c(0, 0.25, 0.01),
                            group_max=c(0.45, 0.6, 0.5))
pspec_mv <- add.constraint(pspec_mv, type = "return", return_target=0.006)
pspec_mv

# optimization
opt_mv <- optimize.portfolio(returns, pspec_mv, optimize_method = "CVXR")
opt_mv

# backtesting
bt_mv <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_mv,
                                            optimize_method="CVXR",
                                            rebalance_on="quarters",
                                            training_period=36)


## ----------------------------------------------------------------------------------------------------------------------------
opt_mv_scs <- optimize.portfolio(returns, pspec_mv, 
                                 optimize_method = c("CVXR", "SCS"))
opt_mv_scs

opt_mv$solver
opt_mv_scs$solver


## ----------------------------------------------------------------------------------------------------------------------------
pspec_mvo <- portfolio.spec(assets=funds)
pspec_mvo <- add.constraint(pspec_mvo, type="full_investment")
pspec_mvo <- add.constraint(pspec_mvo, type="long_only")
# Add objectives
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "return", name = "mean")
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "risk", name = "var",
                           risk_aversion = 5)


## ----------------------------------------------------------------------------------------------------------------------------
opt_mvo <- optimize.portfolio(returns, pspec_mvo, optimize_method = "CVXR")
opt_mvo


## ----------------------------------------------------------------------------------------------------------------------------
pspec_es <- portfolio.spec(assets=funds)
pspec_es <- add.constraint(pspec_es, type="full_investment")
# Add objective of minimizing ES by using the default gamma
pspec_es <- add.objective(portfolio = pspec_es, type = "risk", name = "ES")
# Add objective of minimizing ES by using the specific gamma
pspec_es_85 <- add.objective(portfolio = pspec_es, type = "risk", name = "ES",
                          arguments = list(p=0.85))


## ----------------------------------------------------------------------------------------------------------------------------
opt_es <- optimize.portfolio(returns, pspec_es, optimize_method = "CVXR")
opt_es
opt_es_85 <- optimize.portfolio(returns, pspec_es_85, optimize_method = "CVXR")
opt_es_85


## ----------------------------------------------------------------------------------------------------------------------------
pspec_eqs <- portfolio.spec(assets=funds)
pspec_eqs <- add.constraint(pspec_eqs, type="full_investment")
# Add objective of minimizing EQS
pspec_eqs <- add.objective(portfolio = pspec_eqs, type = "risk", name = "EQS",
                          arguments = list(p=0.95))


## ----------------------------------------------------------------------------------------------------------------------------
opt_eqs <- optimize.portfolio(returns, pspec_eqs, optimize_method = "CVXR")
opt_eqs


## ---- warning=FALSE----------------------------------------------------------------------------------------------------------
## generate GMV, ES and EQS Portfolio with default gamma=0.05
pspec_sc <- portfolio.spec(assets=funds_sc)
pspec_sc <- add.constraint(pspec_sc, type="full_investment")
pspec_sc <- add.constraint(pspec_sc, type="long_only")

pspec_GMV <- add.objective(pspec_sc, type="risk", name="var")
pspec_ES <- add.objective(pspec_sc, type="risk", name="ES")
pspec_EQS <- add.objective(pspec_sc, type="risk", name="EQS")

## Optimize Portfolio at Monthly Rebalancing and 500-Day Training
bt.GMV <- optimize.portfolio.rebalancing(returns_sc, pspec_GMV,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=30,
                                           rolling_window=500)
bt.ES <- optimize.portfolio.rebalancing(returns_sc, pspec_ES,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=30,
                                           rolling_window=500)
bt.EQS <- optimize.portfolio.rebalancing(returns_sc, pspec_EQS,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=30,
                                           rolling_window=500)

## Extract time series of portfolio weights
wts.GMV = extractWeights(bt.GMV)
wts.GMV <- wts.GMV[complete.cases(wts.GMV),]

wts.ES = extractWeights(bt.ES)
wts.ES <- wts.ES[complete.cases(wts.ES),]

wts.EQS = extractWeights(bt.EQS)
wts.EQS <- wts.EQS[complete.cases(wts.EQS),]

## Generate monthly return
ep = endpoints(returns_sc, on= "months", k=1)
sum1 = function(x){apply(x, 2, sum)}
returnM = period.apply(returns_sc, INDEX = ep, FUN = sum1)

## Compute cumulative returns of three portfolios
GMV = Return.rebalancing(returnM, wts.GMV)
ES = Return.rebalancing(returnM, wts.ES)
EQS = Return.rebalancing(returnM, wts.EQS)

# Combine GMV, ES and EQS portfolio cumulative returns
ret.comb <- na.omit(merge(GMV, ES, EQS, all=F))
names(ret.comb) = c("GMV", "ES", "EQS")

# Compute cumulative geometric portfolios returns
R <- ret.comb
geometric = TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}

# Cumulative returns panel (Peter Carl)
p <- xts::plot.xts(c.xts[,1], col="black", main = "Cumulative returns",
                   grid.ticks.lwd=1, grid.ticks.lty = "solid", grid.ticks.on = "years",
                   labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
                   lty = "dotted", ylim = c(min(c.xts), max(c.xts)))
p <- xts::addSeries(c.xts[,2], on=1, lwd=2, col="dark blue", lty="dashed")
p <- xts::addSeries(c.xts[,3], on=1, lwd=2, col="dark green", lty="solid")
p <- xts::addLegend("topleft", on = 1,
                    legend.names = names(c.xts),
                    lty = c(3, 2, 1), lwd = rep(2, NCOL(c.xts)),
                    col = c("black", "dark blue", "dark green"),
                    bty = "o", box.col = "white",
                    bg=rgb(t(col2rgb("white")), alpha = 200,
                           maxColorValue = 255) )

## Drawdowns panel(Peter Carl)
d.xts <- PerformanceAnalytics::Drawdowns(R)
p <- xts::addSeries(d.xts[,1], col="black", lwd=2, main="Drawdown",
                    ylim = c(min(d.xts), 0), lty=3)
p <- xts::addSeries(d.xts[,2], on=2, lwd=2, col="dark blue", lty=2)
p <- xts::addSeries(d.xts[,3], on=2, lwd=2, col="dark green", lty=1)

## panel 1 and 2 ylim
ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2])
ylim2 <- c(p$Env$ylim[[4]][1], p$Env$ylim[[4]][2])
ylim <- c(ylim1, ylim2)
# get longest drawdown dates for xts object
dt <- table.Drawdowns(R, top = 1) # just want to find the worst drawdown
dt2 <- t(dt[,c("From", "To")])
x <- as.vector(dt2[,NCOL(dt2)])
y <- as.xts(matrix(rep(ylim, length(x)),ncol=length(ylim), byrow=TRUE), order.by=as.Date(x))
i=1
p <- xts::addPolygon(y[i:(i+1),1:2], on=-1, col="lightgrey") # top panel
p <- xts::addPolygon(y[i:(i+1),3:4], on=-2, col="lightgrey") # lower panel

p


## ----------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_sr <- portfolio.spec(assets=funds)
## Add constraints of maximizing Sharpe Ratio
pspec_sr <- add.constraint(pspec_sr, type="full_investment")
pspec_sr <- add.constraint(pspec_sr, type="long_only")
## Add objectives of maximizing Sharpe Ratio
pspec_sr <- add.objective(pspec_sr, type = "return", name = "mean")
pspec_sr <- add.objective(pspec_sr, type="risk", name="var")

# Optimization
optimize.portfolio(returns, pspec_sr, optimize_method = "CVXR", maxSR=TRUE)


## ----------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_ESratio <- portfolio.spec(assets=funds)
## Add constraints of maximizing return per unit ES
pspec_ESratio <- add.constraint(pspec_ESratio, type="full_investment")
pspec_ESratio <- add.constraint(pspec_ESratio, type="long_only")
## Add objectives of maximizing return per unit ES
pspec_ESratio <- add.objective(pspec_ESratio, type = "return", name = "mean")
pspec_ESratio <- add.objective(pspec_ESratio, type="risk", name="ES")

# Optimization
optimize.portfolio(returns, pspec_ESratio, optimize_method = "CVXR", ESratio=TRUE)


## ----------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_EQSratio <- portfolio.spec(assets=funds)
## Add constraints of maximizing return per unit EQS
pspec_EQSratio <- add.constraint(pspec_EQSratio, type="full_investment")
pspec_EQSratio <- add.constraint(pspec_EQSratio, type="long_only")
## Add objectives of maximizing return per unit EQS
pspec_EQSratio <- add.objective(pspec_EQSratio, type = "return", name = "mean")
pspec_EQSratio <- add.objective(pspec_EQSratio, type="risk", name="EQS",
                                arguments = list(p=0.95))

# Optimization
optimize.portfolio(returns, pspec_EQSratio, optimize_method = "CVXR", EQSratio=TRUE)


## ----warning=FALSE-----------------------------------------------------------------------------------------------------------
meanvar.ef <- create.EfficientFrontier(R=returns_sc, portfolio=pspec_sc, type="mean-StdDev")
meanvar.ef
meanvar.ef$frontier[, 1:2]


## ----------------------------------------------------------------------------------------------------------------------------
sr = meanvar.ef$frontier[, 1]/meanvar.ef$frontier[, 2]
cat("maximum Sharpe ratio:", max(sr))
cat("mean of the maximum SR portfolio:", meanvar.ef$frontier[, 1][sr == max(sr)])
cat("StdDev of the maximum SR portfolio:", meanvar.ef$frontier[, 2][sr == max(sr)])


## ---- warning=FALSE----------------------------------------------------------------------------------------------------------
# MVO
## Add mean return objective
pspec_GMV <- add.objective(portfolio=pspec_GMV, type="return", name="mean")
opt_GMV <- optimize.portfolio(returns_sc, pspec_GMV, optimize_method = "CVXR", trace = TRUE)
chart.EfficientFrontier(opt_GMV, match.col="StdDev", type="l", 
                        chart.assets = FALSE, main="Mean-StdDev Efficient Frontier",
                        RAR.text="Sharpe Ratio", pch=4)


## ---- warning=FALSE----------------------------------------------------------------------------------------------------------
# mean-ES
meanetl.ef <- create.EfficientFrontier(R=returns_sc, portfolio=pspec_sc, type="mean-ES")
chart.EfficientFrontier(meanetl.ef, match.col="ES", type="l", col="blue",
                        chart.assets = FALSE, main="Mean-ES Efficient Frontier",
                        RAR.text="ES ratio", pch=4)


## ---- warning=FALSE----------------------------------------------------------------------------------------------------------
# mean-EQS
meaneqs.ef <- create.EfficientFrontier(R=returns_sc, portfolio=pspec_sc, type="mean-EQS")
chart.EfficientFrontier(meaneqs.ef, match.col="EQS", type="l", col="blue",
                        chart.assets = FALSE, main="Mean-EQS Efficient Frontier",
                        RAR.text="EQS ratio", pch=4)

