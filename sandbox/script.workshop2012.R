### For R/Finance workshop on Portfolio Analytics
# Chicago, 10 May 2012
# Peter Carl and Brian Peterson

### Load the necessary packages
# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(PerformanceAnalytics)
require(quantmod)
require(DEoptim)
require(doMC)
registerDoMC()
require(TTR)
require(RQuantLib) # Needed for calc'ing bond returns

####### Script WBS
# Parse data from EDHEC or HFRI
## Which styles?
### Relative value
#### FIA
#### Converts
#### EMN
#### Event driven
### Directional
#### US Eq LS
#### Macro
#### CTA
#### Distressed

# Download the following file to the working directory:
# http://www.edhec-risk.com/indexes/pure_style/data/table/history.csv
### @TODO: Is there a way to download it directly? Maybe not, seems to require a login
x=read.csv(file="history.csv", sep=";", header=TRUE, check.names=FALSE)
x.dates = as.Date(x[,1], format="%d/%m/%Y")
x.data = apply(x[,-1], MARGIN=2, FUN=function(x){as.numeric(sub("%","", x, fixed=TRUE))/100}) # get rid of percentage signs
edhec = xts(x.data, order.by=x.dates)
colnames(edhec)
# Drop some indexes and reorder
edhec.R = edhec[,c("Convertible Arbitrage", "Equity Market Neutral","Fixed Income Arbitrage", "Event Driven", "CTA Global", "Global Macro", "Long/Short Equity")]

# Statistical analysis of hedge fund indexes
## Returns through time
postscript(file="EDHEC-Cumulative-Returns.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
par(cex.lab=.8) # should set these parameters once at the top
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 4, 2))
chart.CumReturns(edhec.R, main = "EDHEC Index Returns", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= rainbow8equal, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(5, 4, 0, 2))
chart.Drawdown(edhec.R, main = "", ylab = "Drawdown", colorset = rainbow8equal, cex.axis=.6, cex.lab=.7)
par(op)
## Distributions
## Risk
postscript(file="EDHEC-BarVaR-Returns.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
# Generate charts of EDHEC index returns with ETL and VaR through time
charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="EDHEC Index Returns", clean='boudt', show.cleaned=TRUE, show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rep("black", NCOL(edhec.R)))
# @TODO change the colorset of the names to match the other charts
dev.off()
## Returns and Risk
## Autocorrelation

# Acquire data for factors

## Factor set of five commonly used factors

### Equities
# Get S&P price returns from FRED for now, TR later
# @TODO: Get total returns for S&P factor rather than price returns
getSymbols("SP500", src="FRED") # daily price series
# Calculate monthly returns
SP500.R=monthlyReturn(SP500)
colnames(SP500.R)="SP500"

### Bonds
# Calculate total returns from the yeild of the 10 year constant maturity index maintained by the Fed
getSymbols("GS10", src="FRED") #load US Treasury 10y yields from FRED
# Dates should be end of month, not beginning of the month as reported
index(GS10) = as.Date(as.yearmon(index(GS10)), frac=1)
GS10.pr <- GS10  #set this up to hold price returns
GS10.pr[1,1] <- 0
colnames(GS10.pr) <- "Price Return"
for (i in 1:(NROW(GS10)-1)) {
  GS10.pr[i+1,1] <- FixedRateBondPriceByYield(yield=GS10[i+1,1]/100, issueDate=Sys.Date(), maturityDate=advance("UnitedStates/GovernmentBond", Sys.Date(), 10, 3), rates=GS10[i,1]/100,period=2)[1]/100-1
}
#total return will be the price return + yield/12 for one month
GS10.R <- GS10.pr + lag(GS10,k=1)/12/100
colnames(GS10.R)<-"GS10"

### Currencies
# Trade Weighted U.S. Dollar Index: Major Currencies - TWEXMMTH
getSymbols("TWEXMMTH", src="FRED") # index values
# Dates should be end of month, not beginning of the month as reported
index(TWEXMMTH) = as.Date(as.yearmon(index(TWEXMMTH)), frac=1)
USDI.R=ROC(TWEXMMTH)

### Credit Spread
# Yield spread of Merrill Lynch High-Yield Corporate Master II Index minus 10-year Treasury
getSymbols("BAMLH0A0HYM2EY",src="FRED")
CREDIT=BAMLH0A0HYM2EY-GS10

### Liquidity?
# getSymbols("TB3MS",src="FRED")
# index(TB3MS) = as.Date(as.yearmon(index(TB3MS)), frac=1)
# getSymbols("MED3",src="FRED")
# index(MED3) = as.Date(as.yearmon(index(MED3)), frac=1)
# TED=MED3-TB3MS

### Real estate
### Commodities
## What benchmarks?
### S&P 500 TR or MSCI World
### US 10y or MSCI index?
### USD index
### NAREIT
### DJUBS Commodities index

# Set up objectives as buoys
## Equal contribution to
### Weight
### Variance
### Risk (mETL)
## Reward to Risk
### Mean-Variance
### Mean-mETL
## Minimum 
### Variance
### Risk (mETL)

# Add constraints
## Box constraints - 2% to 20%?
## Rebalancing period - annual

# Set up a starting portfolio
## Could use the equal weight

# Forecast returns
## Start with pamean but don't use it in the presentation
## Apply multi-factor model
## Show fit
## ADD MORE HERE

# Forecast risk
## Historical realized
## GARCH(1,1) for vol? if daily data available...

# Run each of the objective portfolios as of a Date - Dec2010?
## Combined scatter with overlaid objectives, starting portfolio
### Mean-variance plot
### Mean-mETL plot?
### Comparison of portfolio weights plot

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation