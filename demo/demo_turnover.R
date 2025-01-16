## ----- turnover --------------------------------------------------------------

library(PCRA)
library(data.table)
# Select 10 midcap stocks and risk-free rate
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return",
                "MktIndexCRSP", "Ret13WkBill")
dateRange <- c("1997-01-31", "2001-12-31")
ret <- selectCRSPandSPGMI(periodicity = "monthly",
                          dateRange = dateRange, 
                          stockItems = stockItems, 
                          factorItems = NULL,
                          subsetType = "CapGroupLast",
                          subsetValues = "MidCap",
                          outputType = "xts")
dim(ret)
# names(ret)[68:69] # 68 is the Market return and 69 is the RiskFree return
k <- 5  # The next line is for users to experiment with different stocks
nset <- seq(k,k+45,by = 5)
length(nset)
midcap10andRF <- ret[,c(nset,69)]
midcap10 <- midcap10andRF[,1:10]
riskFree <- mean(midcap10andRF[,11])
print(riskFree)
funds <- colnames(midcap10)

## ----echo=F-------------------------------------------------------------------
pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.TO20default = add.constraint(pspec.lo, type="turnover", turnover_target = 0.2, weight_initial = c(1, rep(0, 9)))
pspec.TO20default = add.objective(portfolio=pspec.TO20default, type="risk", name="ES")
optTO20default = optimize.portfolio(midcap10, pspec.TO20default, optimize_method="CVXR")
round(optTO20default$weights, 4)
optTO20default$opt_values

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.TO50default = add.constraint(pspec.lo, type="turnover", turnover_target = 0.1)
pspec.TO50default = add.objective(portfolio=pspec.TO50default, type="risk", name="var")
optTO50default = optimize.portfolio(midcap10, pspec.TO50default, optimize_method="CVXR")
round(optTO50default$weights, 4)
optTO50default$opt_values
