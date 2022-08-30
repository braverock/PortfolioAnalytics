## ----setup, include=FALSE------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
Sys.setlocale("LC_TIME", "English")


## ---- message=FALSE------------------------------------------------------------------------------------------------------------
library(PortfolioAnalytics)
library(CVXR)


## ------------------------------------------------------------------------------------------------------------------------------
data(edhec)
# Use the first 4 columns in edhec for a returns object
returns <- edhec[, 1:4]
colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))
# Get a character vector of the asset names
funds <- colnames(returns)


## ------------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------
# Run the optimization
opt_maxret <- optimize.portfolio(R=returns, portfolio=pspec_maxret, 
                                 optimize_method="CVXR", trace=TRUE)
opt_maxret


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------
bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_maxret,
                                            optimize_method="CVXR",
                                            rebalance_on="quarters",
                                            training_period=36)


## ------------------------------------------------------------------------------------------------------------------------------
class(bt_maxret)
names(bt_maxret)


## ------------------------------------------------------------------------------------------------------------------------------
# Create portfolio object
pspec_gmv <- portfolio.spec(assets=funds)
# Add full-investment constraint
pspec_gmv <- add.constraint(pspec_gmv, type="full_investment")
# Add objective of minimizing variance
pspec_gmv <- add.objective(portfolio = pspec_gmv, type = "risk", name = "var")


## ------------------------------------------------------------------------------------------------------------------------------
opt_gmv <- optimize.portfolio(returns, pspec_gmv, optimize_method = "CVXR")
opt_gmv


## ------------------------------------------------------------------------------------------------------------------------------
bt_gmv <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_gmv,
                                            optimize_method="CVXR",
                                            rebalance_on="quarters",
                                            training_period=36)


## ------------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------
pspec_mvo <- portfolio.spec(assets=funds)
pspec_mvo <- add.constraint(pspec_mvo, type="full_investment")
pspec_mvo <- add.constraint(pspec_mvo, type="long_only")
# Add objectives
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "return", name = "mean")
pspec_mvo <- add.objective(portfolio = pspec_mvo, type = "risk", name = "var",
                           risk_aversion = 5)


## ------------------------------------------------------------------------------------------------------------------------------
opt_mvo <- optimize.portfolio(returns, pspec_mvo, optimize_method = "CVXR")
opt_mvo


## ------------------------------------------------------------------------------------------------------------------------------
pspec_es <- portfolio.spec(assets=funds)
pspec_es <- add.constraint(pspec_es, type="full_investment")
# Add objective of minimizing ES
pspec_es <- add.objective(portfolio = pspec_es, type = "risk", name = "ES",
                          arguments = list(p=0.85))


## ------------------------------------------------------------------------------------------------------------------------------
opt_es <- optimize.portfolio(returns, pspec_es, optimize_method = "CVXR")
opt_es


## ------------------------------------------------------------------------------------------------------------------------------
pspec_eqs <- portfolio.spec(assets=funds)
pspec_eqs <- add.constraint(pspec_eqs, type="full_investment")
# Add objective of minimizing EQS
pspec_eqs <- add.objective(portfolio = pspec_eqs, type = "risk", name = "EQS",
                          arguments = list(p=0.85))


## ------------------------------------------------------------------------------------------------------------------------------
opt_eqs <- optimize.portfolio(returns, pspec_eqs, optimize_method = "CVXR")
opt_eqs


## ------------------------------------------------------------------------------------------------------------------------------
# Optimize Portfolio at Quarterly Rebalancing and 36-Month Training
bt_es <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_es,
                                        optimize_method="CVXR",
                                        rebalance_on="quarters",
                                        training_period=36)
bt_eqs <- optimize.portfolio.rebalancing(R=returns, portfolio=pspec_eqs,
                                         optimize_method="CVXR",
                                         rebalance_on="quarters",
                                         training_period=36)


## ------------------------------------------------------------------------------------------------------------------------------
# Extract time series of portfolio weights
wts_gmv <- extractWeights(bt_gmv)
wts_gmv <- wts_gmv[complete.cases(wts_gmv),]

wts_es <- extractWeights(bt_es)
wts_es <- wts_es[complete.cases(wts_es),]

wts_eqs <- extractWeights(bt_eqs)
wts_eqs <- wts_eqs[complete.cases(wts_eqs),]

# Compute cumulative returns of portfolios
port_gmv <- Return.rebalancing(returns, wts_gmv)
port_es <- Return.rebalancing(returns, wts_es)
port_eqs <- Return.rebalancing(returns, wts_eqs)

# Combine cumulative returns
ret.comb <- na.omit(merge(port_gmv, port_es, port_eqs, all=F))
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
                   ylim = c(min(c.xts), max(c.xts)))
p <- xts::addSeries(c.xts[,2], on=1, lwd=2, col="dark green", lty="dashed")
p <- xts::addSeries(c.xts[,3], on=1, lwd=2, col="dark blue", lty="dotted")
p <- xts::addLegend("topleft", on = 1,
                    legend.names = names(c.xts),
                    lty = c(1, 2, 3), lwd = rep(2, NCOL(c.xts)),
                    col = c("black", "dark green", "dark blue"),
                    bty = "o", box.col = "white",
                    bg=rgb(t(col2rgb("white")), alpha = 200,
                           maxColorValue = 255) )

## Drawdowns panel(Peter Carl)
d.xts <- PerformanceAnalytics::Drawdowns(R)
p <- xts::addSeries(d.xts[,1], col="black", lwd=2, main="Drawdown",
                    ylim = c(min(d.xts), 0), lty="solid")
p <- xts::addSeries(d.xts[,2], on=2, lwd=2, col="dark green", lty="dashed")
p <- xts::addSeries(d.xts[,3], on=2, lwd=2, col="dark blue", lty="dotted")

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


## ------------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------------------
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


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------
init <- portfolio.spec(assets=funds)
init <- add.constraint(portfolio=init, type="full_investment")
init <- add.constraint(portfolio=init, type="long_only")

# MVO
meanvar.ef <- create.EfficientFrontier(R=returns, portfolio=init, type="mean-StdDev")
meanvar.ef
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l",
                        main="MVO Efficient Frontier",
                        RAR.text="Sharpe Ratio", pch=4)


## ----warning=FALSE-------------------------------------------------------------------------------------------------------------
# mean-ES
meanetl.ef <- create.EfficientFrontier(R=returns, portfolio=init, type="mean-ES")
chart.EfficientFrontier(meanetl.ef, match.col="ES",
                        main="mean-ES Efficient Frontier",
                        type="l", col="blue", RAR.text="ES Ratio")


## ------------------------------------------------------------------------------------------------------------------------------
# mean-EQS
meaneqs.ef <- create.EfficientFrontier(R=returns, portfolio=init, type="mean-EQS")
chart.EfficientFrontier(meaneqs.ef, match.col="EQS", chart.assets = FALSE, 
                        main="mean-EQS Efficient Frontier", 
                        type="l", col="red", RAR.text="EQS Ratio")

