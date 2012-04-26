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
require(vcd) # for color palates

### Set up color palates
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
    axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
# Use dark8equal for now?

# Qualitative color scheme by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

####### Script WBS
# Parse data from EDHEC or HFRI
## Just load the data from packages
### See script.buildEDHEC.R and script.buildFactors.R
data(edhec)
# data(factors)


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
#postscript(file="EDHEC-Cumulative-Returns.pdf", height=6, width=10,  paper="special", horizontal=FALSE, onefile=FALSE)
png(filename="EDHEC-Cumulative-Returns.png", units="in", height=5.5, width=9, res=96) 
par(cex.lab=.8) # should set these parameters once at the top
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 4, 2))
chart.CumReturns(edhec.R, main = "EDHEC Index Returns", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= rainbow8equal, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(edhec.R, main = "", ylab = "Drawdown", colorset = rainbow8equal, cex.axis=.6, cex.lab=.7)
par(op)
dev.off()
## Distributions

## Risk
# postscript(file="EDHEC-BarVaR.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
png(filename="EDHEC-BarVaR.png", units="in", height=5.5, width=9, res=96) 
# Generate charts of EDHEC index returns with ETL and VaR through time
charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="EDHEC Index Returns", clean='boudt', show.cleaned=TRUE, show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rainbow8equal)
dev.off()

## Rolling Performance
png(filename="EDHEC-RollPerf.png", units="in", height=5.5, width=9, res=96) 
# Generate charts of EDHEC index returns with ETL and VaR through time
charts.RollingPerformance(edhec.R, width=36, main="EDHEC Index Rolling 36-Month Performance", colorset=rainbow8equal)
dev.off()

## Returns and Risk Scatter
png(filename="EDHEC-Scatter36m.png", units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(last(edhec.R,36), main="EDHEC Index Trailing 36-Month Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()
png(filename="EDHEC-ScatterSinceIncept.png", units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(edhec.R, main="EDHEC Index Since Inception Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()
## Distributions

## Autocorrelation

#########################################################################
# Optimization starts here
########################################################################

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

pameanLCL <- function(n=36, R, weights, scale=12)
{ as.vector(sum( scale*mean.LCL(last(R,n))*weights)) }

paEMA <- function(n=10, R, weights, ...)
{# call Exponential Moving Average from TTR, return the last observation
  sum((12*last(apply(R,2,FUN=TTR::EMA,n=n)))*weights)
}

# pasd <- function(R, weights, n){
#   as.numeric(StdDev(R=last(R,n), weights=weights)*sqrt(12)) # hardcoded for monthly data
# }
pasd <- function(R, weights){
#    as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
    as.numeric(StdDev(R=R, weights=weights)*sqrt(4)) # hardcoded for quarterly data
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
clean = "boudt" #"none"
permutations = 4000

# A set of box constraints used to initialize ALL the bouy portfolios
init.constr <- constraint(assets = colnames(edhec.R),
  min = .05, # minimum position weight
  max = .3, #1, # maximum position weight
  min_sum=0.99, # minimum sum of weights must be equal to 1-ish
  max_sum=1.01, # maximum sum must also be about 1
  weight_seq = generatesequence(by=.005) 
  )
# Add measure 1, annualized return
init.constr <- add.objective(constraints=init.constr,
  type="return", # the kind of objective this is
  #name="pamean",
  name="pameanLCL",
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  # arguments = list(n=60) # for monthly
  arguments = list(n=12) # for quarterly
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
p=1-1/12 # for monthly
p=.25 # for quarterly
init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=FALSE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list(p=p), clean=clean)
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
MeanmETL.constr$objectives[[3]]$multiplier = 1 # mETL
MeanmETL.constr$objectives[[3]]$enabled = TRUE # mETL

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
EqmETL.constr <- add.objective(init.constr, type="risk_budget", name="CVaR",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12), clean=clean))
EqmETL.constr$objectives[[3]]$multiplier = 1 # min mETL
EqmETL.constr$objectives[[3]]$enabled = TRUE # min mETL
EqmETL.constr$objectives[[1]]$multiplier = -1 # max pamean

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so construct weights for it.  Rebalance the equal-weight portfolio at the same frequency as the others.
dates=index(edhec.R[endpoints(edhec.R, on=rebalance_period)])
weights = xts(matrix(rep(1/NCOL(edhec.R),length(dates)*NCOL(edhec.R)), ncol=NCOL(edhec.R)), order.by=dates)
colnames(weights)= colnames(edhec.R)


### Evaluate constraint objects
# Generate a single set of random portfolios to evaluate against all constraint set
rp = random_portfolios(rpconstraints=init.constr, permutations=permutations)

start_time<-Sys.time()
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
  rp=rp, # all the same as prior
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
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
  search_size=permutations, trace=TRUE, verbose=TRUE, 
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
#BHportfs = EqWgt
#for(i in 2:NROW(rp)){ #@TODO: Use foreach in this loop instead
#  weights_i = xts(matrix(rep(rp[i,],length(dates)), ncol=NCOL(rp)), order.by=dates)
#  tmp = Return.rebalancing(edhec.R,weights_i)
#  BHportfs = cbind(BHportfs,tmp)
#}
BHportfs <- foreach(i=1:NROW(rp),.combine=cbind, .inorder=TRUE) %dopar% {
	weights_i = xts(matrix(rep(rp[i,],length(dates)), ncol=NCOL(rp)), order.by=dates)
	tmp = Return.rebalancing(edhec.R,weights_i)
}
# BHportfs <- cbind(EqWgt,BHportfs)

end_time<-Sys.time()
end_time-start_time

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
# Assemble the ex ante result data
results = c("MeanSD.RND.t", "MeanmETL.RND.t", "MinSD.RND.t", "MinmETL.RND.t", "EqSD.RND.t", "EqmETL.RND.t")
results.names= c("Eq Wgt", "Mean SD", "Mean mETL", "Min SD", "Min mETL", "Eq SD", "Eq mETL")
## Extract Weights
RND.weights = MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$weights #EqWgt
for(result in results){
  x=get(result)
  RND.weights = rbind(RND.weights,x[["2010-12-31"]]$weights)
}
rownames(RND.weights)=c(results.names) # @TODO: add prettier labels

## Extract Objective measures
RND.objectives=rbind(MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$objective_measures[1:3]) #EqWgt
for(result in results){
  x=get(result)
  x.obj=rbind(x[["2010-12-31"]]$objective_measures[1:3])
  RND.objectives = rbind(RND.objectives,x.obj)
}
rownames(RND.objectives)=c("EqWgt",results) # @TODO: add prettier labels

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ONLY Equal Weight portfolio
# --------------------------------------------------------------------
xtract = extractStats(MeanSD.RND.t[["2010-12-31"]])
png(filename="RP-EqW-ExAnte-2010-12-31.png", units="in", height=5.5, width=9, res=96) 
plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-Variance Space", cex=.7)
points(RND.objectives[1,2],RND.objectives[1,1], col=tol7qualitative, pch=16)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright",legend=results.names[1], col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
dev.off()

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ALL BUOY portfolios
# --------------------------------------------------------------------
png(filename="Buoy-ExAnte-2010-12-31.png", units="in", height=5.5, width=9, res=96) 
plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-Variance Space", cex=.7)
points(RND.objectives[,2],RND.objectives[,1], col=tol7qualitative, pch=16)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright", legend=results.names, col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
dev.off()

# --------------------------------------------------------------------
# Plot weights of Buoy portfolios
# --------------------------------------------------------------------
# @TODO: add \n to labels
png(filename="Weights-ExAnte-2010-12-31.png", units="in", height=5.5, width=9, res=96)
op <- par(no.readonly=TRUE)
# c(bottom, left, top, right)
par(oma = c(5,12,6,2), mar=c(0,0,0,1))

layout(matrix(c(1:7), nr = 1, byrow = TRUE))
for(i in 1:7){
  if(i==1){
    barplot(RND.weights[i,], col=rainbow8equal, horiz=TRUE, xlim=c(0,max(RND.weights)), axes=FALSE, names.arg=colnames(RND.weights), las=2)
    abline(v=0, col="darkgray")
    abline(v=1/7, col="darkgray", lty=2)
    axis(1, cex.axis = 1, col = "darkgray", las=1)
    mtext(rownames(RND.weights)[i], side= 3, cex=0.7, adj=0)
  } 
  else{
    barplot(RND.weights[i,], col=rainbow8equal, horiz=TRUE, xlim=c(0,max(RND.weights)), axes=FALSE, names.arg="", ylab=rownames(RND.weights)[i])
    abline(v=0, col="darkgray")
    abline(v=1/7, col="darkgray", lty=2)
    mtext(rownames(RND.weights)[i], side= 3, cex=0.7, adj=0)
  }
}
par(op)
title("Portfolio Weights by Objective", outer=TRUE)
dev.off()


# Plot Ex Ante scatter of buoy portfolios and weights
postscript(file="ExAnteScatterWeights20101231.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2,3)),height=c(2,0.25,1.5),width=1)
par(mar=c(4,4,4,2)+.1, cex=1)
## Draw the Scatter chart of combined results
### Get the random portfolios from one of the result sets
# xtract = extractStats(MeanSD.RND.t[["2010-12-31"]]) # did this above
plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-Variance Space", cex=.7)
points(RND.objectives[,2],RND.objectives[,1], col=tol7qualitative, pch=16)
# This could easily be done in mean CVaR space as well
# plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="CVaR", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-mETL Space")
# points(RND.objectives[,3],RND.objectives[,1], col=rainbow8equal, pch=16)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")

# Add legend to middle panel
par(mar=c(0,4,0,2)+.1, cex=0.7)
plot.new()
legend("bottom",legend=rownames(RND.weights), col=tol7qualitative, pch=16, lwd=2, ncol=4,  border.col="darkgray", y.intersp=1.2)

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
plot(RND.weights[1,], type="b", col=rainbow8equal[1],  ylim=c(0,max(EqSD.RND.t$constraints$max)), ylab="Weights", xlab="",axes=FALSE)
points(EqSD.RND.t$constraints$min, type="b", col="darkgray", lty="solid", lwd=2, pch=24)
points(EqSD.RND.t$constraints$max, type="b", col="darkgray", lty="solid", lwd=2, pch=25)
for(i in 1:NROW(RND.weights)) points(RND.weights[i,], type="b", col=tol7qualitative[i], lwd=2)
axis(2, cex.axis = .8, col = "darkgray")
axis(1, labels=columnnames, at=1:numassets, las=3, cex.axis = .8, col = "darkgray")
box(col = "darkgray")
par(op)
dev.off()
# Use colors to group measures weight=orange, ETL=blue, sd=green
# Use pch to group types min=triangle, equal=circle, returnrisk=square

# Calculate ex post results
x.ret2011=Return.cumulative(BHportfs["2011-01::2011-12"])
x.sd2011=StdDev.annualized(BHportfs["2011-01::2011-12"])

obj.real2011=NA
for(i in 1:NROW(RND.weights)){
  x = Return.portfolio(R=edhec.R["2011-01::2011-12"], weights=RND.weights[i,])
  y=c(Return.cumulative(x), StdDev.annualized(x))
  if(is.na(obj.real2011))
    obj.real2011=y
  else
    obj.real2011=rbind(obj.real2011,y)
}
rownames(obj.real2011)=rownames(RND.weights)
colnames(obj.real2011)=c("Realized Returns","Realized SD")
xmin=min(c(x.sd2011,xtract[,"pasd.pasd"]))
xmax=max(c(x.sd2011,xtract[,"pasd.pasd"]))
ymin=min(c(x.ret2011,xtract[,"mean"]))
ymax=max(c(x.ret2011,xtract[,"mean"]))

# Show Ex Ante and Ex Post results for 2010-12-31
# One more time, chart the ex ante results in mean-sd space
postscript(file="ExAnteExPost20101231.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2,3)),height=c(2,0.25,2),width=1)
par(mar=c(4,4,4,2)+.1, cex=1)
## Draw the Scatter chart of combined results
### Get the random portfolios from one of the result sets
xtract = extractStats(MeanSD.RND.t[["2010-12-31"]])
plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Ex Ante Results for 2010-12-31", cex=.5,  xlim=c(xmin,xmax), ylim=c(ymin,ymax))
grid(col = "darkgray")
points(RND.objectives[,2],RND.objectives[,1], col=tol7qualitative, pch=16)
abline(h = 0, col = "darkgray")
# This could easily be done in mean CVaR space as well
# plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="CVaR", ylab="Mean", col="darkgray", axes=FALSE, main="Objectives in Mean-mETL Space")
# points(RND.objectives[,3],RND.objectives[,1], col=rainbow8equal, pch=16)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")

# Add legend to middle panel
par(mar=c(0,4,0,2)+.1, cex=0.7)
plot.new()
legend("bottom",legend=rownames(RND.weights), col=tol7qualitative, pch=16, ncol=4,  border.col="darkgray", y.intersp=1.2)

# Plot the ex post results in mean-sd space
# Realized return versus predicted volatility?
par(mar=c(4,4,4,2)+.1, cex=1)
plot(x.sd2011,x.ret2011, xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="Ex Post Results for 2010-12-31", cex=.5,  xlim=c(xmin,xmax), ylim=c(ymin,ymax))
grid(col = "darkgray")
points(obj.real2011[,2],obj.real2011[,1], col=tol7qualitative, pch=16)
abline(h = 0, col = "darkgray")
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
par(op)
dev.off()

# Results through time
# @TODO: remove center panel
charts.PerformanceSummary(cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)["2009::2011"], colorset=tol7qualitative)
charts.PerformanceSummary(cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)["2000::2011"], colorset=tol7qualitative)

turnover = function(w1,w2) {sum(abs(w1-w2))/length(w1)}
# Calculate the turnover matrix for the random portfolio set:
to.matrix<-matrix(nrow=NROW(rp),ncol=NROW(rp))
for(x in 1:NROW(rp)){
  for(y in 1:NROW(rp)) {
    to.matrix[x,y]<-turnover(rp[x,],rp[y,])
  }
}

# Show turnover of the RP portfolios relative to the EqWgt portfolio
postscript(file="TurnoverOf20101231.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2)),height=c(4,1),width=1)
par(mar=c(4,4,4,2)+.1, cex=1)
  seq.col = heat.colors(11)
  ## Draw the Scatter chart of combined results
  ### Get the random portfolios from one of the result sets
  x=apply(rp, MARGIN=1,FUN=turnover,w2=rp[1,])
  plot(xtract[,"pasd.pasd"],xtract[,"mean"], xlab="StdDev", ylab="Mean", col=seq.col[ceiling(x*100)], axes=FALSE, main="Turnover of Random Portfolios from Equal-Weight Portfolio", cex=.7, pch=16)
  points(RND.objectives[1,2],RND.objectives[1,1], col="blue", pch=19, cex=1)
  axis(1, cex.axis = 0.8, col = "darkgray")
  axis(2, cex.axis = 0.8, col = "darkgray")
  box(col = "darkgray")

# Add legend to bottom panel
par(mar=c(5,5.5,2,3)+.1, cex=0.7)
## Create a histogramed legend for sequential colorsets
## this next bit of code is based on heatmap.2 in gplots package
x=ceiling(x*100)
  scale01 <- function(x, low = min(x), high = max(x)) {
    return((x - low)/(high - low))
  }
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = length(seq.col)+1)
  min.raw <- min(x, na.rm = TRUE)
  max.raw <- max(x, na.rm = TRUE)
  z <- seq(min.raw, max.raw, length = length(seq.col))
  image(z = matrix(z, ncol = 1), col = seq.col, breaks = breaks, xaxt = "n", yaxt = "n")
  par(usr = c(0, 1, 0, 1)) # needed to draw the histogram correctly
  lv <- pretty(breaks)
  xv <- scale01(as.numeric(lv), min.raw, max.raw)
  axis(1, at = xv, labels=sprintf("%s%%", pretty(lv)))
  h <- hist(x, plot = FALSE, breaks=breaks)
  hx <- scale01(breaks, min(x), max(x))
  hy <- c(h$counts, h$counts[length(h$counts)])
  lines(hx, hy/max(hy)*.95, lwd = 2, type = "s", col = "blue")
  axis(2, at = pretty(hy)/max(hy)*.95, pretty(hy))
  title(ylab="Count")
  title(xlab="Degree of Turnover from Equal Weight Portfolio")
par(op)
dev.off()
# Ex-ante and Ex-post views of buoy portfolios at a date

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation