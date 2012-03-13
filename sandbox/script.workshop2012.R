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
## Box constraints - 5% to 30%?
## Rebalancing period - quarterly? annual?
## Turnover constraints

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

# pasd <- function(R, weights, n){
#   as.numeric(StdDev(R=last(R,n), weights=weights)*sqrt(12)) # hardcoded for monthly data
# }
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

# Construct objectives for buoy portfolios

# Select a rebalance period
rebalance_period = 'quarters' # uses endpoints identifiers from xts

# A set of box constraints used to initialize ALL the bouy portfolios
init.constr <- constraint(assets = colnames(edhec.R),
  min = .05, # minimum position weight
  max = .3, #1, # maximum position weight
  min_sum=0.99, # minimum sum of weights must be equal to 1-ish
  max_sum=1.01, # maximum sum must also be about 1
  weight_seq = generatesequence() 
  )
# Add measure 1, annualized return
init.constr <- add.objective(constraints=init.constr,
  type="return", # the kind of objective this is
  name="pamean",
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments = list(n=60)
  )
# Add measure 2, annualized standard deviation
init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="pasd", # the function to minimize
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list() # from inception 
  )
# Add measure 3, CVaR with p=(1-1/12)
init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=FALSE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list(p=(1-1/12)) #, clean="boudt"
  )

### Construct BUOY 1: Constrained Mean-StdDev Portfolio
MeanSD.constr <- init.constr
# Turn back on the return and sd objectives
MeanSD.constr$objectives[[1]]$multiplier = -1 # pamean
MeanSD.constr$objectives[[2]]$multiplier = 1 # pasd

### Construct BUOY 2: Constrained Mean-mETL Portfolio
MeanmETL.constr <- init.constr
# Turn on the return and mETL objectives
MeanmETL.constr$objectives[[1]]$multiplier = -1 # pamean
MeamETL.constr$objectives[[3]]$multiplier = 1 # mETL
MeamETL.constr$objectives[[3]]$enabled = TRUE # mETL

### Construct BUOY 3: Constrained Minimum Variance Portfolio
MinSD.constr <- init.constr
# Turn back on the sd objectives
MinSD.constr$objectives[[2]]$multiplier = 1 # StdDev

### Construct BUOY 4: Constrained Minimum mETL Portfolio
MinmETL.constr <- init.constr
# Turn back on the mETL objective
MinmETL.constr$objectives[[3]]$multiplier = 1 # mETL
MinmETL.constr$objectives[[3]]$enabled = TRUE # mETL

### Construct BUOY 5: Constrained Equal Variance Contribution Portfolio
EqSD.constr <- add.objective(init.constr, type="risk_budget", name="StdDev",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12)))
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
EqSD.constr$objectives[[2]]$multiplier = 1 # min paSD
EqSD.constr$objectives[[1]]$multiplier = -1 # max pamean

### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio
EqmETL.constr <- add.objective(init.constr, type="risk_budget", name="CVaR",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12)))
EqmETL.constr$objectives[[3]]$multiplier = 1 # min mETL
EqmETL.constr$objectives[[3]]$enabled = TRUE # min mETL
EqmETL.constr$objectives[[1]]$multiplier = -1 # max pamean

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so construct weights for it.  Rebalance the equal-weight portfolio at the same frequency as the others.
dates=index(edhec.R[endpoints(edhec.R, on=rebalance_period)])
weights = xts(matrix(rep(1/NCOL(edhec.R),length(dates)*NCOL(edhec.R)), ncol=NCOL(edhec.R)), order.by=dates)
colnames(weights)= colnames(edhec.R)


### Evaluate constraint objects
# Generate a single set of random portfolios to evaluate against all constraint sets
rp = random_portfolios(rpconstraints=init.constr, permutations=1000)

### Evaluate BUOY 1: Constrained Mean-StdDev Portfolio
# MeanSD.RND<-optimize.portfolio(R=edhec.R,
#   constraints=MeanSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MeanSD.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives through time 
### requires PortfolioAnalytics build >= 1864
MeanSD.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=MeanSD.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  #rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MeanSD.w = extractWeights.rebal(MeanSD.RND.t)
MeanSD=Return.rebalancing(edhec.R, MeanSD.w)
colnames(MeanSD) = "MeanSD"

### Evaluate BUOY 2: Constrained Mean-mETL Portfolio
# MeanmETL.RND<-optimize.portfolio(R=edhec.R,
#   constraints=MeanmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MeanmETL.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MeanmETL.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=MeanmETL.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MeanmETL.w = extractWeights.rebal(MeanmETL.RND.t)
MeanmETL=Return.rebalancing(edhec.R, MeanmETL.w)
colnames(MeanmETL) = "MeanmETL"

### Evaluate BUOY 3: Constrained Minimum Variance Portfolio
# MinSD.RND<-optimize.portfolio(R=edhec.R,
#   constraints=MinSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MinSD.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MinSD.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=MinSD.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MinSD.w = extractWeights.rebal(MinSD.RND.t)
MinSD=Return.rebalancing(edhec.R, MinSD.w)
colnames(MinSD) = "MinSD"

### Evaluate BUOY 4: Constrained Minimum mETL Portfolio
# MinmETL.RND<-optimize.portfolio(R=edhec.R,
#   constraints=MinmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MinmETL.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MinmETL.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=MinmETL.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MinmETL.w = extractWeights.rebal(MinmETL.RND.t)
MinmETL=Return.rebalancing(edhec.R, MinmETL.w)
colnames(MinmETL) = "MinmETL"

### Evaluate BUOY 5: Constrained Equal Variance Contribution Portfolio
# EqSD.RND<-optimize.portfolio(R=edhec.R,
#   constraints=EqSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(EqSD.RND, risk.col="pasd.pasd", return.col="mean")
EqSD.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=EqSD.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
EqSD.w = extractWeights.rebal(EqSD.RND.t)
EqSD=Return.rebalancing(edhec.R, EqSD.w)
colnames(EqSD) = "EqSD"

### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio
# EqmETL.RND<-optimize.portfolio(R=edhec.R,
#   constraints=EqmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
EqmETL.RND.t = optimize.portfolio.rebalancing(R=edhec.R,
  constraints=EqmETL.constr, 
  optimize_method='random', 
  search_size=1000, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
EqmETL.w = extractWeights.rebal(EqmETL.RND.t)
EqmETL=Return.rebalancing(edhec.R, EqmETL.w)
colnames(EqmETL) = "EqmETL"

### Evaluate BUOY 7: Equal Weight Portfolio
# There's only one, so calculate it.  Rebalance the equal-weight portfolio regularly, matching the periods above
EqWgt = Return.rebalancing(edhec.R,weights) # requires development build of PerfA >= 1863 or CRAN version 1.0.4 or higher
colnames(EqWgt)="EqWgt"
### Performance of Buy & Hold Random Portfolios
BHportfs = EqWgt
for(i in 2:NROW(rp)){ #@TODO: Use foreach in this loop instead
  weights_i = xts(matrix(rep(rp[i,],length(dates)), ncol=NCOL(rp)), order.by=dates)
  tmp = Return.rebalancing(edhec.R,weights_i)
  BHportfs = cbind(BHportfs,tmp)
}
# Chart EqWgt Results against BH RP portfolios
postscript(file="EqWgtBHPerfSumm.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
charts.PerformanceSummary(BHportfs, main="Equal Weight and Buy & Hold Random Portfolios", methods=c("ModifiedVaR", "ModifiedES"), p=(1-1/12), gap=36, colorset=c("orange",rep("darkgray",NCOL(BHportfs))), lwd=3, legend.loc=NA)
# use clean='boudt', show.cleaned=TRUE, in final version?
dev.off()


### Plot comparison of objectives and weights 
# > names(EqmETL.RND)
# [1] "random_portfolios"                  "random_portfolio_objective_results"
# [3] "weights"                            "objective_measures"                
# [5] "call"                               "constraints"                       
# [7] "data_summary"                       "elapsed_time"                      
# [9] "end_t"      
# Assemble the result data
results = c("MeanSD.RND.t", "MeanmETL.RND.t", "MinSD.RND.t", "MinmETL.RND.t", "EqSD.RND.t", "EqmETL.RND.t")
## Extract Weights
RND.weights=MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$weights #EqWgt
for(result in results){
  x=get(result)
  RND.weights = rbind(RND.weights,x[["2010-12-31"]]$weights)
}
rownames(RND.weights)=c("EqWgt",results) # @TODO: add prettier labels

## Extract Objective measures
RND.objectives=rbind(MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$objective_measures[1:3]) #EqWgt
for(result in results){
  x=get(result)
  x.obj=rbind(x[["2010-12-31"]]$objective_measures[1:3])
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
# This could easily be done in mean CVaR space as well
# plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="CVaR", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-mETL Space")
# points(RND.objectives[,3],RND.objectives[,1], col=rainbow8equal, pch=16)
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
# Use colors to group measures weight=orange, ETL=blue, sd=green
# Use pch to group types min=triangle, equal=circle, returnrisk=square

# Results through time
# @TODO: remove center panel
charts.PerformanceSummary(cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL))


# Ex-ante and Ex-post views of buoy portfolios at a date

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation