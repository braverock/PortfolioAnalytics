############################## 1. EQS example ###################################
library(PortfolioAnalytics)
library(PCRA)

# Load Data
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
k <- 5
nset <- seq(k,k+45,by = 5)
midcap10andRF <- ret[,c(nset,69)]
midcap10 <- midcap10andRF[,1:10]
riskFree <- mean(midcap10andRF[,11])
funds <- colnames(midcap10)

# Build long-only EQS portfolio
pspec = portfolio.spec(assets=funds)
pspec = add.constraint(pspec,type = "full_investment")
pspec = add.constraint(pspec, type="long_only")
pspecEQS = add.objective(portfolio=pspec, type="risk", name="EQS")
optimize.portfolio(midcap10, pspecEQS, optimize_method = "CVXR")

############################## 2. Efficient Frontier ###################################
# Test 1: minEQS with large turnover target, small penalty
pspecEQS.TO2s = add.constraint(pspecEQS, type="turnover", turnover_target = 0.7, turnover_penalty = 1e-10)
optTO2s = optimize.portfolio(midcap10, pspecEQS.TO2s, optimize_method="CVXR")
round(optTO2s$weights, 6) # I increased the precision
optTO2s$opt_values

# Test 2: minEQS with small turnover target, small penalty
pspecEQS.TO05s = add.constraint(pspecEQS, type="turnover", turnover_target = 0.3, turnover_penalty = 1e-10)
optTO05s = optimize.portfolio(midcap10, pspecEQS.TO05s, optimize_method="CVXR")
round(optTO05s$weights, 6)
optTO05s$opt_values

# Test 3: EQS with turnover Efficient Frontier
# Compare TO with no TO for EQS portfolio
# Note: For CSM and EQS efficient frontiers, chart.EfficientFrontier() doesn't work.
#       You can only use plotFrontiers()
legend_labels <- c("No TOC", "TOC = 0.7", "TOC = 0.3")

eqs.NTO = meaneqs.efficient.frontier(pspecEQS, midcap10, optimize_method = 'CVXR')
eqs.TO2s = meaneqs.efficient.frontier(pspecEQS.TO2s, midcap10, optimize_method = 'CVXR')
eqs.TO05s = meaneqs.efficient.frontier(pspecEQS.TO05s, midcap10, optimize_method = 'CVXR')
plotFrontiers(midcap10, frontiers=list(eqs.NTO, eqs.TO2s, eqs.TO05s), 
              legend.labels = legend_labels, risk='EQS')
