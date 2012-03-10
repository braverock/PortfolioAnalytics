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
{ as.vector(sum(Return.annualized(last(R,n), geometric=geometric)*weights)) }

paEMA <- function(n=10, R, weights, ...)
{# call Exponential Moving Average from TTR, return the last observation
  sum((12*last(apply(R,2,FUN=TTR::EMA,n=n)))*weights)
}

pasd <- function(R, weights, n){
  as.numeric(StdDev(R=last(R,n), weights=weights)*sqrt(12)) # hardcoded for monthly data
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
  max = .3, #1, # maximum position weight
  min_sum=0.99, # minimum sum of weights must be equal to 1-ish
  max_sum=1.01, # maximum sum must also be about 1
  weight_seq = generatesequence() 
  )

init.constr <- add.objective(constraints=init.constr,
  type="return", # the kind of objective this is
  name="pamean",
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments = list(n=60)
  )

init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="pasd", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list(n=60)
  )
# @TODO: add CVaR here to look at these in CVaR space

# Generate a single set of random portfolios to evaluate against all constraint sets
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
plot(MeanSD.RND, risk.col="pasd.pasd", return.col="mean")

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
plot(MeanmETL.RND, risk.col="pasd.pasd", return.col="mean")

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
plot(MinSD.RND, risk.col="pasd.pasd", return.col="mean")

### BUOY 4: Constrained Minimum mETL Portfolio
MinmETL.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  arguments=list(p=(1-1/12))
  )
# Evaluate the constraint object with Random Portfolios
MinmETL.RND<-optimize.portfolio(R=edhec.R,
  constraints=MinmETL.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above
plot(MinmETL.RND, risk.col="pasd.pasd", return.col="mean")

### BUOY 5: Constrained Equal Variance Contribution Portfolio
EqSD.constr <- add.objective(init.constr, type="risk_budget", name="StdDev",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12)))
# Evaluate the constraint object with Random Portfolios
EqSD.RND<-optimize.portfolio(R=edhec.R,
  constraints=EqSD.constr,
  optimize_method='random',
  search_size=1000, trace=TRUE, verbose=TRUE,
  rp=rp) # use the same random portfolios generated above
plot(EqSD.RND, risk.col="pasd.pasd", return.col="mean")


### BUOY 6: Constrained Equal mETL Contribution Portfolio
EqmETL.constr <- add.objective(init.constr, type="risk_budget", name="CVaR",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12)))
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
# > names(EqmETL.RND)
# [1] "random_portfolios"                  "random_portfolio_objective_results"
# [3] "weights"                            "objective_measures"                
# [5] "call"                               "constraints"                       
# [7] "data_summary"                       "elapsed_time"                      
# [9] "end_t"      
# Assemble the result data
results = c("MeanSD.RND", "MeanmETL.RND", "MinSD.RND", "MinmETL.RND", "EqSD.RND", "EqmETL.RND")
## Weights
RND.weights=MeanSD.RND$random_portfolio_objective_results[[1]]$weights #EqWgt
for(result in results){
  x=get(result)
  RND.weights = rbind(RND.weights,x$weights)
}
rownames(RND.weights)=c("EqWgt",results) # @TODO: add prettier labels

## Objective measures
RND.objectives=rbind(MeanSD.RND$random_portfolio_objective_results[[1]]$objective_measures[1:2]) #EqWgt
for(result in results){
  x=get(result)
  x.obj=rbind(x$objective_measures[1:2])
  RND.objectives = rbind(RND.objectives,x.obj)
}
rownames(RND.objectives)=c("EqWgt",results) # @TODO: add prettier labels

# Chart the results together
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2,3)),height=c(2,0.25,1.5),width=1)
par(mar=c(4,4,4,2)+.1, cex=1)
## Draw the Scatter chart of combined results
### Get the random portfolios from one of the result sets
xtract = extractStats(MeanSD.RND)
plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-Variance Space")
points(RND.objectives[,2],RND.objectives[,1], col=rainbow8equal, pch=16)
box(col = "darkgray")
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")

# Add legend to middle panel
par(mar=c(0,4,0,2)+.1, cex=0.7)
plot.new()
legend("bottom",legend=rownames(RND.weights), col=rainbow8equal, pch=16, lwd=2, ncol=4,  border.col="darkgray", y.intersp=1.2)

# Draw the Weights chart of the combined results
columnnames = colnames(RND.weights)
numassets = length(columnnames)
minmargin = 3
topmargin=1
# set the bottom border to accommodate labels
  bottommargin = max(c(minmargin, (strwidth(columnnames,units="in"))/par("cin")[1])) * 1 #cex.lab
  if(bottommargin > 10 ) {
    bottommargin<-10
    columnnames<-substr(columnnames,1,19)
    # par(srt=45) #TODO figure out how to use text() and srt to rotate long labels
  }
par(mar = c(bottommargin, 4, topmargin, 2) +.1, cex=1)
plot(RND.weights[1,], type="b", col=rainbow8equal[1],  ylim=c(0,max(EqSD.RND$constraints$max)), ylab="Weights", xlab="",axes=FALSE)
points(EqSD.RND$constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
points(EqSD.RND$constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
for(i in 1:NROW(RND.weights)) points(RND.weights[i,], type="b", col=rainbow8equal[i], lwd=2)
axis(2, cex.axis = .8, col = "darkgray")
axis(1, labels=columnnames, at=1:numassets, las=3, cex.axis = .8, col = "darkgray")
box(col = "darkgray")
par(op)
## Add a legend in a third panel
# Use colors to group measures weight=orange, ETL=blue, sd=green
# Use pch to group types min=triangle, equal=circle, returnrisk=square

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation