### For R/Finance workshop on Portfolio Analytics
# Chicago, 10 May 2012
# Peter Carl and Brian Peterson

### Load the necessary packages
# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(quantmod)
require(DEoptim)
require(foreach)
require(doMC)
registerDoMC(3)
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

########################################################################
# Performance analysis of EDHEC hedge fund indexes
########################################################################
# --------------------------------------------------------------------
# EDHEC Indexes Returns through time
# --------------------------------------------------------------------
#postscript(file="EDHEC-Cumulative-Returns.pdf", height=6, width=10,  paper="special", horizontal=FALSE, onefile=FALSE)
png(filename="EDHEC-Cumulative-Returns.png", units="in", height=5.5, width=9, res=96) 
par(cex.lab=.8) # should set these parameters once at the top
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) #c(bottom, left, top, right)
chart.CumReturns(edhec.R, main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= rainbow8equal, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(edhec.R, main = "", ylab = "Drawdown", colorset = rainbow8equal, cex.axis=.6, cex.lab=.7)
par(op)
dev.off()

# --------------------------------------------------------------------
# EDHEC Indexes Risk
# --------------------------------------------------------------------
# postscript(file="EDHEC-BarVaR.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
png(filename="EDHEC-BarVaR.png", units="in", height=5.5, width=9, res=96) 
# Generate charts of EDHEC index returns with ETL and VaR through time
par(mar=c(3, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
# charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="", clean='boudt', show.cleaned=TRUE, show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rainbow8equal)

charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="", show.greenredbars=TRUE, 
              methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, 
              colorset=rep("Black",7), ylim=c(-.1,.15))

# charts.BarVaR(edhec.R, p=(1-1/12), gap=36, main="", show.greenredbars=TRUE, methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, colorset=rainbow8equal)
par(op)
dev.off()

# --------------------------------------------------------------------
# EDHEC Indexes Rolling Performance
# --------------------------------------------------------------------
png(filename="EDHEC-RollPerf.png", units="in", height=5.5, width=9, res=96) 
# Generate charts of EDHEC index returns with ETL and VaR through time
par(mar=c(5, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.RollingPerformance(edhec.R, width=36, main="", colorset=rainbow8equal, legend.loc="topleft")
par(op)
dev.off()

# --------------------------------------------------------------------
# EDHEC Indexes Returns and Risk Scatter
# --------------------------------------------------------------------
png(filename="EDHEC-Scatter36m.png", units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(last(edhec.R,36), main="EDHEC Index Trailing 36-Month Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()
png(filename="EDHEC-ScatterSinceIncept.png", units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(edhec.R, main="EDHEC Index Since Inception Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()

# --------------------------------------------------------------------
## EDHEC Indexes Table of Return and Risk Statistics
# --------------------------------------------------------------------
require(Hmisc)
incept.stats = t(table.RiskStats(R=edhec.R, p=p, Rf=.03/12))
write.csv(incept.stats, file="inception-stats.csv")
png(filename="EDHEC-InceptionStats.png", units="in", height=5.5, width=9, res=96) 
textplot(format.df(incept.stats, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1,3,1,3,3,1,3,3,1,1,3,3,1,0), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, mar = c(0,0,4,0)+0.1))
dev.off()
# --------------------------------------------------------------------
## EDHEC Indexes Distributions
# --------------------------------------------------------------------

png(filename="EDHEC-Distributions.png", units="in", height=5.5, width=9, res=96) 
op <- par(no.readonly = TRUE)
# c(bottom, left, top, right)
par(oma = c(5,0,2,1), mar=c(0,0,0,3))
layout(matrix(1:28, ncol=4, byrow=TRUE), widths=rep(c(.6,1,1,1),7))
# layout.show(n=21)
chart.mins=min(edhec.R)
chart.maxs=max(edhec.R)
row.names = sapply(colnames(RND.weights), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)
for(i in 1:7){
  if(i==7){
    plot.new()
    text(x=1, y=0.5, adj=c(1,0.5), labels=row.names[i], cex=1.1)
    chart.Histogram(edhec.R[,i], main="", xlim=c(chart.mins, chart.maxs), breaks=seq(-0.15,0.10, by=0.01), show.outliers=TRUE, methods=c("add.normal"))
    abline(v=0, col="darkgray", lty=2)
    chart.QQPlot(edhec.R[,i], main="", pch="*", envelope=0.95, col=c(1,"#005AFF"), ylim=c(chart.mins, chart.maxs))
    abline(v=0, col="darkgray", lty=2)
    chart.ECDF(edhec.R[,i], main="", xlim=c(chart.mins, chart.maxs), lwd=2)
    abline(v=0, col="darkgray", lty=2)
  }
  else{
    plot.new()
    text(x=1, y=0.5, adj=c(1,0.5), labels=row.names[i], cex=1.1)
    chart.Histogram(edhec.R[,i], main="", xlim=c(chart.mins, chart.maxs), breaks=seq(-0.15,0.10, by=0.01), xaxis=FALSE, yaxis=FALSE, show.outliers=TRUE, methods=c("add.normal"))
    abline(v=0, col="darkgray", lty=2)
    chart.QQPlot(edhec.R[,i], main="", xaxis=FALSE, yaxis=FALSE, pch="*", envelope=0.95, col=c(1,"#005AFF"), ylim=c(chart.mins, chart.maxs))
    abline(v=0, col="darkgray", lty=2)
    chart.ECDF(edhec.R[,i], main="", xlim=c(chart.mins, chart.maxs), xaxis=FALSE, yaxis=FALSE, lwd=2)
    abline(v=0, col="darkgray", lty=2)
  }
}
par(op)
dev.off()

# --------------------------------------------------------------------
# Correlation
# --------------------------------------------------------------------
require("corrplot")
col3 <- colorRampPalette(c("darkgreen", "white", "darkred"))
M <- cor(edhec.R)
colnames(M) = rownames(M) = row.names
order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="complete")
M.hc2 <- M[order.hc2,order.hc2]
png(filename="EDHEC-cor-inception.png", units="in", height=5.5, width=4.5, res=96) 
corrplot(M.hc2, tl.col="black", tl.cex=0.8, method="square", col=col3(8), cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25)
corrRect.hclust(M.hc2, k=3, method="complete", col="blue")
dev.off()

M36 <- cor(last(edhec.R,36))
colnames(M36) = rownames(M36) = row.names
order36.hc2 <- corrMatOrder(M36, order="hclust", hclust.method="complete")
M36.hc2 <- M36[order36.hc2,order36.hc2]
png(filename="EDHEC-cor-tr36m.png", units="in", height=5.5, width=4.5, res=96) 
corrplot(M36.hc2, tl.col="black", tl.cex=0.8, method="square", col=col3(8), cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25)
corrRect.hclust(M36.hc2, k=3, method="complete", col="blue")
dev.off()


# --------------------------------------------------------------------
## Autocorrelation
# --------------------------------------------------------------------
# @TODO: This is frosting, do it last

runname='garch.sigma.and.mu'

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

pasd <- function(R, weights){
    as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
#    as.numeric(StdDev(R=R, weights=weights)*sqrt(4)) # hardcoded for quarterly data
}

pasd.garch<- function(R,weights,garch.sigma,...) {
	#sigmas is an input of predicted sigmas on a date, 
	# presumably from a GARCH model
	as.numeric(sum((garch.sigma[last(index(R)),]*weights)*sqrt(12)))
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
  name="pamean",
  #name="pameanLCL",
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments = list(n=60) # for monthly
  # arguments = list(n=12) # for quarterly
)
# Add measure 2, annualized standard deviation
init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="pasd", # to minimize from the sample
  #name='pasd.garch', # to minimize from the predicted sigmas
  enabled=TRUE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list() # from inception for pasd 
  #arguments=list(sigmas=garch.sigmas) # from inception for pasd.garch 
)
# Add measure 3, CVaR with p=(1-1/12)

# set confidence for VaR/ES
p=1-1/12 # for monthly
#p=.25 # for quarterly

init.constr <- add.objective(init.constr,
  type="risk", # the kind of objective this is
  name="CVaR", # the function to minimize
  enabled=FALSE, # enable or disable the objective
  multiplier=0, # calculate it but don't use it in the objective
  arguments=list(p=p), 
  clean=clean
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
EqSD.constr$objectives[[1]]$multiplier = 0 # max pamean

### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio
EqmETL.constr <- add.objective(init.constr, type="risk_budget", name="CVaR",  enabled=TRUE, min_concentration=TRUE, arguments = list(p=(1-1/12), clean=clean))
EqmETL.constr$objectives[[3]]$multiplier = 1 # min mETL
EqmETL.constr$objectives[[3]]$enabled = TRUE # min mETL
EqmETL.constr$objectives[[1]]$multiplier = 0 # max pamean

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so construct weights for it.  Rebalance the equal-weight portfolio at the same frequency as the others.
dates=index(edhec.R[endpoints(edhec.R, on=rebalance_period)])
weights = xts(matrix(rep(1/NCOL(edhec.R),length(dates)*NCOL(edhec.R)), ncol=NCOL(edhec.R)), order.by=dates)
colnames(weights)= colnames(edhec.R)


### Evaluate constraint objects
# Generate a single set of random portfolios to evaluate against all constraint set
print(paste('constructing random portfolios at',Sys.time()))
rp = random_portfolios(rpconstraints=init.constr, permutations=permutations)
print(paste('done constructing random portfolios at',Sys.time()))

### Choose our 'R' variable
R=edhec.R # for monthlies
#R=edhec.R.quarterly #to use the quarterlies
#R=garch.mu # to use the monthly quarter-ahead predictions from the garch

start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))
### Evaluate BUOY 1: Constrained Mean-StdDev Portfolio
# MeanSD.RND<-optimize.portfolio(R=R,
#   constraints=MeanSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MeanSD.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives through time 
### requires PortfolioAnalytics build >= 1864
MeanSD.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(MeanSD.RND.t,MeanSD.w,MeanSD,file=paste('MeanSD',Sys.Date(),runname,'rda',sep='.'))

print(paste('Completed meanSD optimization at',Sys.time(),'moving on to meanmETL'))

### Evaluate BUOY 2: Constrained Mean-mETL Portfolio
# MeanmETL.RND<-optimize.portfolio(R=R,
#   constraints=MeanmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MeanmETL.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MeanmETL.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(MeanmETL.RND.t,MeanmETL.w,MeanmETL,file=paste('MeanmETL',Sys.Date(),runname,'rda',sep='.'))
print(paste('Completed meanmETL optimization at',Sys.time(),'moving on to MinSD'))

### Evaluate BUOY 3: Constrained Minimum Variance Portfolio
# MinSD.RND<-optimize.portfolio(R=R,
#   constraints=MinSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MinSD.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MinSD.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(MinSD.RND.t,MinSD.w,MinSD,file=paste('MinSD',Sys.Date(),runname,'rda',sep='.'))
print(paste('Completed MinSD optimization at',Sys.time(),'moving on to MinmETL'))

### Evaluate BUOY 4: Constrained Minimum mETL Portfolio
# MinmETL.RND<-optimize.portfolio(R=R,
#   constraints=MinmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(MinmETL.RND, risk.col="pasd.pasd", return.col="mean")
# Evaluate the objectives with RP through time 
MinmETL.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(MinmETL.RND.t,MinmETL.w,MinmETL,file=paste('MinmETL',Sys.Date(),runname,'rda',sep='.'))
print(paste('Completed MinmETL optimization at',Sys.time(),'moving on to EqSD'))

### Evaluate BUOY 5: Constrained Equal Variance Contribution Portfolio
# EqSD.RND<-optimize.portfolio(R=R,
#   constraints=EqSD.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
# plot(EqSD.RND, risk.col="pasd.pasd", return.col="mean")
EqSD.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(EqSD.RND.t,EqSD.w,EqSD,file=paste('EqSD',Sys.Date(),runname,'rda',sep='.'))
print(paste('Completed EqSD optimization at',Sys.time(),'moving on to EqmETL'))

### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio
# EqmETL.RND<-optimize.portfolio(R=R,
#   constraints=EqmETL.constr,
#   optimize_method='random',
#   search_size=1000, trace=TRUE, verbose=TRUE,
#   rp=rp) # use the same random portfolios generated above
EqmETL.RND.t = optimize.portfolio.rebalancing(R=R,
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
save(EqmETL.RND.t,EqmETL.w,EqmETL,file=paste('EqmETL',Sys.Date(),runname,'rda',sep='.'))
print(paste('Completed EqmETL optimization at',Sys.time(),'moving on to EqWgt'))

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
save(rp,BHportfs,EqWgt,file=paste('BHportfs',Sys.Date(),runname,'rda',sep='.'))

end_time<-Sys.time()
end_time-start_time

# Assemble the ex ante result data

results.obj = c("MeanSD.RND.t", "MeanmETL.RND.t", "MinSD.RND.t", "MinmETL.RND.t", "EqSD.RND.t", "EqmETL.RND.t")
results.names= c("Eq Wgt", "Mean SD", "Mean mETL", "Min SD", "Min mETL", "Eq SD", "Eq mETL")

results = list(EqWgt=EqWgt,
		BHportfs=BHportfs,
		MeanSD.RND.t=MeanSD.RND.t, 
		MeanmETL.RND.t=MeanmETL.RND.t, 
		MinSD.RND.t=MinSD.RND.t, 
		MinmETL.RND.t=MinmETL.RND.t, 
		EqSD.RND.t=EqSD.RND.t, 
		EqmETL.RND.t=EqmETL.RND.t)


# evalDate="2010-12-31"
## Extract Weights
evalDate="2008-06-30"
RND.weights = MeanSD.RND.t[[evalDate]]$random_portfolio_objective_results[[1]]$weights #EqWgt
for(result in results.obj){
    x=get(result)
    RND.weights = rbind(RND.weights,x[[evalDate]]$weights)
}
rownames(RND.weights)=results.names # @TODO: add prettier labels

#RND.weights = MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$weights #EqWgt
#for(result in results){
#	RND.weights = rbind(RND.weights,result[["2010-12-31"]]$weights)
#}
#results.names= c("Eq Wgt", "Mean SD", "Mean mETL", "Min SD", "Min mETL", "Eq SD", "Eq mETL")
#rownames(RND.weights)=c(results.names) # @TODO: add prettier labels
#
### Extract Objective measures
#RND.objectives=rbind(MeanSD.RND.t[["2010-12-31"]]$random_portfolio_objective_results[[1]]$objective_measures[1:3]) #EqWgt
#for(result in results){
#	RND.objectives = rbind(RND.objectives,rbind(result[["2010-12-31"]]$objective_measures[1:3]))
#}
#rownames(RND.objectives)=c("EqWgt",results) # @TODO: add prettier labels
save(results,file=paste(Sys.Date(),runname,'full-results','rda',sep='.'))

## Extract Objective measures
RND.objectives=rbind(MeanSD.RND.t[[evalDate]]$random_portfolio_objective_results[[1]]$objective_measures[1:3]) #EqWgt
x.obj<-NULL
for(result in names(results)[grep('.t',names(results),fixed=TRUE)]){
  print(result)
    x=get('results')[[result]]
    x.obj=rbind(x.obj, data.frame(mean=x[[evalDate]]$objective_measures[[1]],pasd=x[[evalDate]]$objective_measures[[2]],CVaR=as.numeric(x[[evalDate]]$objective_measures[[3]][1])))
}
print(x.obj)
rownames(x.obj)=names(results)[grep('.t',names(results),fixed=TRUE)] # @TODO: add prettier labels


#****************************************************************************
# END main optimization section
#****************************************************************************
op <- par(no.readonly=TRUE)

# --------------------------------------------------------------------
# NOT USED: Chart EqWgt Results against BH RP portfolios
# --------------------------------------------------------------------
postscript(file="EqWgtBHPerfSumm.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
charts.PerformanceSummary(BHportfs, main="Equal Weight and Buy & Hold Random Portfolios", methods=c("ModifiedVaR", "ModifiedES"), p=(1-1/12), gap=36, colorset=c("orange",rep("darkgray",NCOL(BHportfs))), lwd=3, legend.loc=NA)
# use clean='boudt', show.cleaned=TRUE, in final version?
dev.off()
# --------------------------------------------------------------------


### Plot comparison of objectives and weights 
# > names(EqmETL.RND)
# [1] "random_portfolios"                  "random_portfolio_objective_results"
# [3] "weights"                            "objective_measures"                
# [5] "call"                               "constraints"                       
# [7] "data_summary"                       "elapsed_time"                      
# [9] "end_t"      

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ONLY Equal Weight portfolio
# --------------------------------------------------------------------
xtract = extractStats(MeanSD.RND.t[[evalDate]])

png(filename="RP-EqW-ExAnte-2008-06-30.png", units="in", height=5.5, width=9, res=96) 
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
plot(xtract[,"pasd.pasd"],xtract[,"pamean.pamean"], xlab="Predicted StdDev", ylab="Predicted Mean", col="darkgray", axes=FALSE, main="", cex=.7)
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
points(RND.objectives[1,2],RND.objectives[1,1], col=tol7qualitative, pch=16, cex=1.5)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright",legend=results.names[1], col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ALL BUOY portfolios
# --------------------------------------------------------------------
png(filename="Buoy-ExAnte-2008-06-30.png", units="in", height=5.5, width=9, res=96) 
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
plot(xtract[,"pasd.garch.pasd.garch"],xtract[,"pamean.pamean"], xlab="Predicted StdDev", ylab="Predicted Mean", col="darkgray", axes=FALSE, main="", cex=.7)
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
points(x.obj[,2],x.obj[,1], col=tol7qualitative, pch=16, cex=1.5)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright", legend=results.names, col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot weights of Buoy portfolios as of 2008-06-30
# --------------------------------------------------------------------
png(filename="Weights-ExAnte-2008-06-30.png", units="in", height=5.5, width=9, res=96)
par(oma = c(4,8,2,1), mar=c(0,0,0,1)) # c(bottom, left, top, right)
layout(matrix(c(1:7), nr = 1, byrow = TRUE))
row.names = sapply(colnames(RND.weights), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)
for(i in 1:7){
  if(i==1){
    barplot(RND.weights[i,], col=rainbow8equal, horiz=TRUE, xlim=c(0,max(RND.weights)), axes=FALSE, names.arg=row.names, las=2, cex.names=1.1)
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
dev.off()

# --------------------------------------------------------------------
# NOT USED: Plot Ex Ante scatter of buoy portfolios and weights
# --------------------------------------------------------------------
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

# --------------------------------------------------------------------
# Plot Ex Post scatter of buoy portfolios
# --------------------------------------------------------------------
# Calculate ex post results
xpost.ret=Return.cumulative(BHportfs["2008-06::2008-09"])
xpost.sd=StdDev.annualized(BHportfs["2008-06::2008-09"])

xpost.obj=NA
for(i in 1:NROW(RND.weights)){
  x = Return.portfolio(R=edhec.R["2008-06::2008-09"], weights=RND.weights[i,])
  y=c(Return.cumulative(x), StdDev.annualized(x))
  if(is.na(xpost.obj))
    xpost.obj=y
  else
    xpost.obj=rbind(xpost.obj,y)
}
rownames(xpost.obj)=rownames(RND.weights)
colnames(xpost.obj)=c("Realized Returns","Realized SD")
xmin=min(c(xpost.sd,xtract[,"pasd.garch.pasd.garch"]))
xmax=max(c(xpost.sd,xtract[,"pasd.garch.pasd.garch"]))
ymin=min(c(xpost.ret,xtract[,"pamean.pamean"]))
ymax=max(c(xpost.ret,xtract[,"pamean.pamean"]))

png(filename="Scatter-ExPost-2008-06-30.png", units="in", height=5.5, width=9, res=96)
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
plot(xpost.sd,xpost.ret, xlab="Realized StdDev", ylab="Realized Mean", col="darkgray", axes=FALSE, main="", cex=.7)#,  xlim=c(xmin,xmax), ylim=c(ymin,ymax))
grid(col = "darkgray")
points(xpost.obj[,2],xpost.obj[,1], col=tol7qualitative, pch=16, cex=1.5)
abline(h = 0, col = "darkgray")
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomleft",legend=rownames(RND.weights), col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, inset=.02)
dev.off()

# --------------------------------------------------------------------
# NOT USED: Show Ex Ante AND Ex Post results for 2010-12-31
# --------------------------------------------------------------------
# One more time, chart the ex ante results in mean-sd space
# postscript(file="ExAnteExPost20070630.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
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
legend("bottomright",legend=rownames(RND.weights), col=tol7qualitative, pch=16, ncol=4,  border.col="darkgray", y.intersp=1.2)
par(op)
dev.off()

# --------------------------------------------------------------------
# Ex Post Results Through Time
# --------------------------------------------------------------------
# charts.PerformanceSummary(cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)["2009::2011"], colorset=tol7qualitative)
# charts.PerformanceSummary(cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)["2000::2011"], colorset=tol7qualitative)
buoys.R=cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)
png(filename="Buoy-Cumulative-Returns.png", units="in", height=5.5, width=9, res=96) 
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) # c(bottom, left, top, right)
chart.CumReturns(buoys.R["2000::",], main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= tol7qualitative, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(buoys.R["2000::",], main = "", ylab = "Drawdown", colorset = tol7qualitative, cex.axis=.6, cex.lab=.7)
par(op)
dev.off()


# --------------------------------------------------------------------
# Show turnover of the RP portfolios relative to the EqWgt portfolio
# --------------------------------------------------------------------
turnover = function(w1,w2) {sum(abs(w1-w2))/length(w1)}
# Calculate the turnover matrix for the random portfolio set:
to.matrix<-matrix(nrow=NROW(rp),ncol=NROW(rp))
for(x in 1:NROW(rp)){
  for(y in 1:NROW(rp)) {
    to.matrix[x,y]<-turnover(rp[x,],rp[y,])
  }
}

png(filename="Turnover-2010-12-31.png", units="in", height=5.5, width=9, res=96)
# postscript(file="TurnoverOf20101231.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2)),height=c(4,1.25),width=1)
par(mar=c(4,4,1,2)+.1, cex=1) # c(bottom, left, top, right)
  seq.col = heat.colors(11)
  ## Draw the Scatter chart of combined results
  ### Get the random portfolios from one of the result sets
  x=apply(rp, MARGIN=1,FUN=turnover,w2=rp[1,])
  plot(xtract[,"pasd.garch.pasd.garch"],xtract[,"pamean.pamean"], xlab="Predicted StdDev", ylab="Predicted Mean", col=seq.col[ceiling(x*100)], axes=FALSE, main="", cex=.7, pch=16)
  grid(col = "darkgray")
  points(RND.objectives[1,2],RND.objectives[1,1], col="blue", pch=19, cex=1.5)
  axis(1, cex.axis = 0.8, col = "darkgray")
  axis(2, cex.axis = 0.8, col = "darkgray")
  box(col = "darkgray")

# Add legend to bottom panel
par(mar=c(5,5.5,1,3)+.1, cex=0.7)
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

# --------------------------------------------------------------------
# RSGarch results against EqWgt returns
# --------------------------------------------------------------------

print(load("~/devel/R/RSGarch.rda"))

png(filename="RSGarch.png", units="in", height=5.5, width=9, res=96) 
layout(matrix(c(1,2)),height=c(2,1.5),width=1)
par(mar=c(1, 4, 4, 2) + 0.1, cex=0.8) #c(bottom, left, top, right)
chart.TimeSeries(zoo(f.MC.q,index(EqWgt)[-1]), main='Regime Switching Garch model for the Equal Weight Portfolio', ylab='Volatility Regime from 0(low) to 1(high)', xaxis=FALSE, colorset=rep("blue",2), lty=2, lwd=1)
par(mar=c(4,4,0,2)+.1, cex=0.8) #c(bottom, left, top, right)
chart.BarVaR(EqWgt, methods="StdDev", show.symmetric=TRUE, main="", ylab="EqWgt Return", width=12)
par(op)
dev.off()


# --------------------------------------------------------------------
# Other things we might do:

###############
#ARMA-GARCH(1,1,1) for mu, sigma, and skew estimates 3 months out
require(rugarch)


ctrl = list(rho = 1, delta = 1e-6, outer.iter = 100, tol = 1e-6,outer.iter=700,inner.iter=2000)
spec = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
        mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
        distribution.model = "ghyp")

dates<-seq.Date(from=as.Date('1975-03-01'), to=as.Date('1996-12-31'),by=1)
dates<-dates[endpoints(dates,on='months')]
taildates<-seq.Date(from=as.Date(as.Date(last(index(edhec.R)))+1), to=as.Date(as.Date(last(index(edhec.R)))+160),by=1)
taildates<-taildates[endpoints(taildates,on='months')]
tailxts<-xts(rep(0,length(taildates)),order.by=taildates)

garch.out <- foreach(i=1:ncol(edhec.R),.inorder=TRUE) %dopar% {

    oridata  <- edhec.R[,i]
    start <- first(index(oridata))
    end   <- last(index(oridata))
    
    #bootsstrap
    #we're going to do a simple sample with replacement here
    # if doing this 'for real', a more sophisticated factor model monte carlo 
    # or AR tsboot() approach would likely be preferred 
    data<-rbind(xts(sample(oridata,length(dates),replace=TRUE),order.by=dates),oridata,tailxts)
    colnames(data)<-colnames(oridata)
    #add some NA's on the end, hack for now
    
    #run the garch
    rm(bktest)
    #NOTE forecast.length needs to be evenly divisible by n.ahead and refit.every
    bktest = ugarchroll(spec, data = data, n.ahead = 3,
            forecast.length = 186, refit.every = 3, refit.window = "moving",
            solver = "solnp", fit.control = list(), solver.control = ctrl,
            calculate.VaR = FALSE, VaR.alpha = c(0.01, 0.025, 0.05))
    
    # the standardized density parameters (rho, zeta)
    f01density = bktest@roll$f01density[[3]]
    skew = apply(f01density[,5:6],1, FUN = function(x) dskewness("nig", skew=x[1], shape=x[2]))
    kurt = apply(f01density[,5:6],1, FUN = function(x) dkurtosis("nig", skew=x[1], shape=x[2])) #excess kurtosis
    
    # just sum the n.ahead predictions for now, could compound them, not sure 
    # what trouble that would cause
    mu3 = rowSums(cbind(as.data.frame(bktest, n.ahead=1, refit = "all", which = "series"), 
          as.data.frame(bktest, n.ahead=2, refit = "all", which = "series"),
          as.data.frame(bktest, n.ahead=3, refit = "all", which = "series")))

    sigma3 = rowMeans(cbind(as.data.frame(bktest, n.ahead=1, refit = "all", which = "sigma"), 
          as.data.frame(bktest, n.ahead=2, refit = "all", which = "sigma"),
          as.data.frame(bktest, n.ahead=3, refit = "all", which = "sigma")))
  
    garchmom<-cbind(
            as.data.frame(bktest, n.ahead=3, refit = "all", which = "series"), 
            as.data.frame(bktest, n.ahead=3, refit = "all", which = "sigma"),
            skew, kurt,mu3,sigma3)
    
    garchmom.xts<-xts(garchmom,order.by=as.Date(rownames(garchmom)))
    garchdata<-garchmom.xts[index(oridata)]
    colnames(garchdata)[1]<-'mu'
    colnames(garchdata)[6]<-'sigma3'
    
    out<-list(bktest=bktest,spec=spec,oridata=oridata,data=data,garchmom=garchmom.xts,garchdata=garchdata,start=start, end=end)
}
names(garch.out)<-colnames(edhec.R)
# OK, so now we've got a big unweildy GARCH list.  let's create garchmu and garchsigma
garch.mu<-foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$mu }
names(garch.mu)<-colnames(edhec.R)
garch.sigma<-foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$sigma }
names(garch.sigma)<-colnames(edhec.R)
garch.skew<-foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$skew }
names(garch.skew)<-colnames(edhec.R)
garch.kurtosis <-foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$kurt }
names(garch.kurtosis)<-colnames(edhec.R)
garch.mu3 <- foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$mu3 }
names(garch.mu3)<-colnames(edhec.R)
garch.sigma3 <- foreach(x=iter(garch.out),.combine=cbind)%do% { x$garchdata$sigma3 }
names(garch.sigma3)<-colnames(edhec.R)


#diagnose skew and kurtosis
last(garch.skew)
skewness(tail(edhec.R,36))
last(garch.kurtosis)
kurtosis(tail(edhec.R,36))

foreach(i=1:ncol(edhec.R)) %do% {
    dev.new()
    par(mfrow=c(3,1))
    plot(garch.out[[i]]$bktest, n.ahead=1, which = 3,main=paste(names(garch.out)[i],'n-ahead 1'))
    plot(garch.out[[i]]$bktest, n.ahead=2, which = 3,main=paste(names(garch.out)[i],'n-ahead 2'))
    plot(garch.out[[i]]$bktest, n.ahead=3, which = 3,main=paste(names(garch.out)[i],'n-ahead 3'))
}

save(garch.out,garch.mu,garch.mu3,garch.sigma,garch.sigma3,garch.skew,garch.kurtosis,file=paste(Sys.Date(),runname,'garch-results','rda',sep='.'))

#####
# you can examine the bktest slots using commands like:
#report(bktest, type="VaR", n.ahead = 1, VaR.alpha = 0.01, conf.level = 0.95)
#report(convert.arb.bktest, type="fpm")
#plot(bktest)

# fit=ugarchfit(garch.out$"Global Macro"$spec, garch.out$"Global Macro"$data)
# show(fit)


###########
# Multivariate Garch from rmgarch package

# set up the data
bs.data <- foreach(i=1:ncol(edhec.R),.inorder=TRUE, .combine=cbind) %dopar% {
    
    oridata  <- edhec.R[,i]
    start <- first(index(oridata))
    end   <- last(index(oridata))
    
    #bootsstrap
    #we're going to do a simple sample with replacement here
    # if doing this 'for real', a more sophisticated factor model monte carlo 
    # or AR tsboot() approach would likely be preferred 
    data<-rbind(xts(sample(oridata,length(dates),replace=TRUE),order.by=dates),oridata,tailxts)
    colnames(data)<-colnames(oridata)
    #add some NA's on the end, hack for now
    
    data
}

#try GO-GARCH
ggspec = gogarchspec(
        mean.model = list(model = c("constant", "AR", "VAR")[3], lag = 2), #external.regressors = ex), 
        variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), submodel = NULL, variance.targeting = FALSE), 
        distribution.model = c("mvnorm", "manig", "magh")[3], ica = c("fastica", "pearson", "jade", "radical")[1])
ggfit = gogarchfit(ggspec, data =  bs.data, out.sample = 0, 
        parallel = FALSE, parallel.control = parallel.control)

gportmoments(ggfit,weights=rep(1/ncol(bs.data),ncol(bs.data)))

rcov(ggfit)

ggroll = gogarchroll(ggspec, data = bs.data, n.ahead = 1,
        forecast.length = 186, refit.every = 3, refit.window = "moving",
        solver = "solnp", fit.control = list(), solver.control = ctrl)

# try DCC-GARCH
spec = spec # use the same spec we used for the univariate GARCH
spec1 = dccspec(uspec = multispec( replicate(7, spec) ), dccOrder = c(1,1), asymmetric = TRUE, distribution = "mvnorm")
dccfit1 = dccfit(spec1, data = bs.data, fit.control = list(eval.se=FALSE))


#dccroll1 = dccroll(spec1, data = bs.data, n.ahead = 1,
#        forecast.length = 186, refit.every = 3, refit.window = "moving",
#        solver = "solnp", fit.control = list(), solver.control = ctrl)

###
# garch moments
# mu
dccmu<-fitted(dccfit1)
colnames(dccmu)<-colnames(edhec.R)
rownames(dccmu)<-as.character(index(bs.data)[-1])
dccmu<-xts(dccmu,order.by=as.Date(rownames((dccmu))))
dccmu<-dccmu[index(edhec.R)]

# sigma
dccsigma<-sigma(dccfit1)
colnames(dccsigma)<-colnames(edhec.R)
rownames(dccsigma)<-as.character(index(bs.data)[-1])
dccsigma<-xts(dccsigma,order.by=as.Date(rownames((dccsigma))))
dccsigma<-dccsigma[index(edhec.R)]

# conditional covariance
dcccova<-rcov(dccfit1)
dcccovl<-list()
for(i in 1:dim(dcccova)[3]) { dcccovl[[i]]<- dcccova[,,i]; colnames(dcccovl[[i]])<-colnames(edhec.R); rownames(dcccovl[[i]])<-colnames(edhec.R)}
names(dcccovl)<-index(bs.data)[-length(index(bs.data))]
dcccovls<-dcccovl[1:444] #subset out only the real data
dcccovls<-dcccovls[263:444] # dump the zero junk from the end
all.equal(names(dcccovls),as.character(index(edhec.R)))

# conditional correlation
dcccora<-rcor(dccfit1)
dcccorl<-list()
for(i in 1:dim(dcccora)[3]) { dcccorl[[i]]<- dcccora[,,i]; colnames(dcccorl[[i]])<-colnames(edhec.R); rownames(dcccorl[[i]])<-colnames(edhec.R)}
names(dcccorl)<-index(bs.data)[-length(index(bs.data))]
dcccorls<-dcccorl[1:444] #subset out only the real data
dcccorls<-dcccorls[263:444] # dump the zero junk from the end
dcccorl<-dcccorls
rm(dcccorls,dcccora)
save(spec,dccfit1,dccmu,dccsigma,dcccovl,dcccorl,file=paste('MVDCCGarch',Sys.Date(),'rda',sep='.'))

# Historical performance of each buoy portfolio
## Same statistics as above
## Compare relative performance

# Condition factor model forecasts?
## On volatility
## On correlation