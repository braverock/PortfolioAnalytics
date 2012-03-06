### For R/Finance workshop on Portfolio Analytics
# Chicago, 10 May 2012
# Peter Carl and Brian Peterson

### Load the necessary packages
# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(PerformanceAnalytics)
require(xts)
require(DEoptim)
require(doMC)
registerDoMC()
require(TTR)

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

## Distributions
## Risk
postscript(file="EDHEC-returns.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
# Generate charts of EDHEC index returns with ETL and VaR through time
charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="EDHEC Index Returns", clean='boudt', show.cleaned=TRUE, show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rep("black", NCOL(edhec.R)))

## Returns and Risk
## Autocorrelation

# Acquire data for factors
## What factor set?
### Equities
### Bonds
### Currencies
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