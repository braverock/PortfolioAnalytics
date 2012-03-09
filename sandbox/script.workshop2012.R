### For R/Finance workshop on Portfolio Analytics
# Chicago, 10 May 2012
# Peter Carl and Brian Peterson

### Load the necessary packages
# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(quantmod)
require(DEoptim)
require(doMC)
registerDoMC()
require(TTR)
# Available on r-forge
require(FactorAnalytics) # development version > build 

####### Script WBS
# Parse data from EDHEC or HFRI
## Just load the data from packages
### See script.buildEDHEC.R and script.buildFactors.R
data(edhec)
data(factors)


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
postscript(file="EDHEC-BarVaR.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
# Generate charts of EDHEC index returns with ETL and VaR through time
charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="EDHEC Index Returns", clean='boudt', show.cleaned=TRUE, show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rainbow8equal)
dev.off()

## Returns and Risk Scatter

## Autocorrelation



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
### Create a small weighted annualized trailing-period mean wrapper function
pamean <- function(n=12, R, weights, geometric=TRUE)
{ sum(Return.annualized(last(R,n), geometric=geometric)*weights) }

paEMA <- function(n=10, R, weights, ...)
{# call Exponential Moving Average from TTR, return the last observation
  sum((12*last(apply(R,2,FUN=TTR::EMA,n=n)))*weights)
}

pasd <- function(R, weights){
  as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
}
## Apply multi-factor model
## Show fit
## ADD MORE DETAIL HERE

# Forecast risk
## Historical realized
## GARCH(1,1) for vol? if daily data available...

# Run each of the objective portfolios as of a Date - Dec2010?
## Combined scatter with overlaid objectives, starting portfolio
### Mean-variance plot

# A set of box constraints used to initialize all the bouy portfolios
init.constr <- constraint(assets = colnames(edhec.R),
  min = .05, # minimum position weight
  max = .5, #1, # maximum position weight
  min_sum=0.99, # minimum sum of weights must be equal to 1-ish
  max_sum=1.01, # maximum sum must also be about 1
  weight_seq = generatesequence() 
  )

init.constr <- add.objective(constraints=init.constr,
  type="return", # the kind of objective this is
  name="pamean",
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments = list(n=36)
  )

init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="pasd", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list()
  )

rp = random_portfolios(rpconstraints=init.constr, permutations=1000)

### BUOY 1: Constrained Mean-StdDev Portfolio
MeanSD.constr <- init.constr
# Turn back on the return and sd objectives
MeanSD.constr$objectives[[1]]$multiplier = -1 # pamean
MeanSD.constr$objectives[[2]]$multiplier = 1 # pasd
# Evaluate the constraint object with Random Portfolios
MeanSD.RND<-optimize.portfolio(R=edhec.R,
  constraints=MeanSD.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 2: Constrained Mean-mETL Portfolio
MeanmETL.constr <- init.constr
# Turn on the return objective
MeanmETL.constr$objectives[[1]]$multiplier = -1 # pamean
# Add a mETL risk objective to the constraints
MeanmETL.constr <- add.objective(MeanmETL.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  arguments=list(p=(1-1/12), clean="boudt")
  )
# Evaluate the constraint object with Random Portfolios
MeanmETL.RND<-optimize.portfolio(R=edhec.R,
  constraints=MeanmETL.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 3: Constrained Minimum Variance Portfolio
MinSD.constr <- init.constr
# Turn back on the sd objectives
MinSD.constr$objectives[[2]]$multiplier = 1 # StdDev
# Evaluate the constraint object with Random Portfolios
MinSD.RND<-optimize.portfolio(R=edhec.R,
  constraints=MinSD.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 4: Constrained Minimum mETL Portfolio
MinmETL.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  arguments=list(p=(1-1/12), clean="boudt")
  )
# Evaluate the constraint object with Random Portfolios
MinmETL.RND<-optimize.portfolio(R=edhec.R,
  constraints=MinmETL.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 5: Constrained Equal Variance Contribution Portfolio
EqSD.constr <- add.objective(init.constr, type="risk_budget", name="StdDev",  enabled=TRUE, min_concentration=TRUE, arguments = list(clean='boudt', p=(1-1/12)))
# Evaluate the constraint object with Random Portfolios
EqSD.RND<-optimize.portfolio(R=edhec.R,
  constraints=EqSD.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 6: Constrained Equal mETL Contribution Portfolio
EqmETL.constr <- add.objective(init.constr, type="risk_budget", name="CVaR",  enabled=TRUE, min_concentration=TRUE, arguments = list(clean='boudt', p=(1-1/12)))
# Evaluate the constraint object with Random Portfolios
EqmETL.RND<-optimize.portfolio(R=edhec.R,
  constraints=EqmETL.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above

### BUOY 7: Equal Weight Portfolio
# There's only one:
# Rebalance an equal-weight portfolio annually
dates=c(as.Date("1999-12-31"),time(edhec.R[endpoints(edhec.R, on="years")]))
weights = xts(matrix(rep(1/NCOL(edhec.R),length(dates)*NCOL(edhec.R)), ncol=NCOL(edhec.R)), order.by=dates)
colnames(weights)= colnames(edhec.R)
EqWgt = Return.rebalancing(edhec.R,weights)

# Chart EqWgt Results
postscript(file="EqWgtPlot1.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
charts.PerformanceSummary(EqWgt, main="Eq Wgt Portfolio", methods=c("ModifiedVaR", "ModifiedES"), p=(1-1/12), clean='boudt', show.cleaned=TRUE, gap=36, colorset=black, lwd=3)
dev.off()

### Comparison of portfolio weights plot

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation