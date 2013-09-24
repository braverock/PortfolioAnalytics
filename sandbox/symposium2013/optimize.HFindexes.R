### For Presentation at FactSet's 2013 US Investment Process Symposium
# November 10 - 12 , 2013
# Peter Carl

### Load the necessary packages
# Include optimizer packages
require(PortfolioAnalytics)
require(quantmod)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
# ... and multi-core packages
require(foreach)
require(doMC)
registerDoMC(5)

# Available on r-forge
# require(FactorAnalytics) # development version > build 

### Set script constants
runname='historical.moments'

# Select a rebalance period
rebalance_period = 'quarters' # uses endpoints identifiers from xts; how to do semi-annual?
clean = "boudt" #"none"
permutations = 1000
p=1-1/12 # set confidence for VaR/mETL for monthly data

### Description

# Seven assets, in this case hedge fund indexes representing different styles and two 'types':
# Convertible Arbitrage   | Non-directional
# Equity Market Neutral   | Non-directional
# Fixed Income Arbitrage  | Non-directional
# Event Driven            | Non-directional
# CTA Global              | Directional
# Global Macro            | Directional
# Long/Short Equity       | Directional

# see analyze.HFindexes.R for more detail

# Set up seven objectives as buoy portfolios:
# - Equal contribution to...
#   1 Weight
#   2 Variance
#   3 Risk (mETL)
# - Reward to Risk ratio of...
#   4 Mean-Variance
#   5 Mean-mETL
# - Minimum...
#   6 Variance
#   7 Risk (mETL)

# Add constraints
# - Box constraints - 5% to 30%
# - Group constraints - Non-directional constrained to 20-70%; Directional between 10-50%
# - Rebalancing period - quarterly
# - Turnover constraints - TBD

#------------------------------------------------------------------------
# Set up an initial portfolio object with constraints and objectives using
# v2 specification

# Create initial portfolio object used to initialize ALL the bouy portfolios
init.portf <- portfolio.spec(assets=colnames(R), 
                             weight_seq=generatesequence(by=0.005)
)
# Add leverage constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="leverage", 
                             min_sum=1, 
                             max_sum=1
)
# Add box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0.05, 
                             max=0.3
)
# Add group constraint
init.portf <- add.constraint(portfolio=init.portf, type="group",
                  groups=list(c(1:4),
                              c(5:7)),
                  group_min=c(0.25,.05),
                  group_max=c(0.85,0.75)
)

# print(init.portf)
# summary(init.portf)

#------------------------------------------------------------------------
### Construct BUOY 1: Constrained Mean-StdDev Portfolio - using ROI
MeanSD.portf <- init.portf
# Add the return and sd objectives to the constraints created above
MeanSD.portf <- add.objective(portfolio=init.portf,
                              type="return", # the kind of objective this is
                              name="mean" # name of the function
                              )
MeanSD.portf <- add.objective(portfolio=MeanSD.portf,
                              type="risk", # the kind of objective this is
                              name="var" # name of the function
                              )

### Construct BUOY 2: Constrained Mean-mETL Portfolio - using ROI
#@ Cannot maximize mean return per unit ETL with ROI, consider using
#@ random portfolios or DEoptim. - RB
# Add the return and mETL objectives
MeanmETL.portf <- add.objective(portfolio=init.portf,
                                type="return", # the kind of objective this is
                                name="mean" # name of the function
                                )
MeanmETL.portf <- add.objective(portfolio=MeanmETL.portf,
                                type="risk", # the kind of objective this is
                                name="ES", # the function to minimize
                                arguments=list(p=p)
                                )

### Construct BUOY 3: Constrained Minimum Variance Portfolio - using ROI
# Add the variance objective
MinSD.portf <- add.objective(portfolio=init.portf,
                             type="risk", # the kind of objective this is
                             name="var", # name of the function
                             )

### Construct BUOY 4: Constrained Minimum mETL Portfolio - using ROI
# Add the mETL objective
MinmETL.portf <- add.objective(portfolio=init.portf,
                               type="risk", # the kind of objective this is
                               name="ES", # the function to minimize
                               arguments=list(p=p)
                               )

### Construct BUOY 5: Constrained Equal Variance Contribution Portfolio - using RP
#@ - Add the sub-objectives first. Adding these 3 objectives means that we are
#@ maximizing mean per unit StdDev with equal volatility contribution portfolios. - RB
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
EqSD.portf <- add.objective(portfolio=init.portf,
                            type="risk",
                            name="StdDev"
) # OR
EqSD.portf <- add.objective(portfolio=EqSD.portf,
                            type="return",
                            name="mean"
)
EqSD.portf <- add.objective(portfolio=EqSD.portf, 
                            type="risk_budget", 
                            name="StdDev",  
                            min_concentration=TRUE,
                            arguments = list(clean=clean)
                            )

EqSD.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
EqSD.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio - using RP
#@ Add the sub-objectives first. These should be added to the EqmETL portfolio.
#@ All objectives below mean that we are maximizing mean return per unit ES with
#@ equal ES contribution. - RB
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
EqmETL.portf <- add.objective(portfolio=init.portf,
                            type="risk",
                            name="ES"
) # OR
EqmETL.portf <- add.objective(portfolio=EqmETL.portf,
                            type="return",
                            name="mean"
)
EqmETL.portf <- add.objective(EqmETL.portf,
                              type="risk_budget",
                              name="ES",
                              min_concentration=TRUE,
                              arguments = list(p=(1-1/12), clean=clean)
)

EqmETL.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
EqmETL.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so construct weights for it.  Rebalance the equal-weight portfolio at the same frequency as the others.
# dates=index(R[endpoints(R, on=rebalance_period)])
# weights = xts(matrix(rep(1/NCOL(R),length(dates)*NCOL(R)), ncol=NCOL(R)), order.by=dates)
# colnames(weights)= colnames(R)

### Construct RISK BUDGET Portfolio
RiskBudget.portf <- portfolio.spec(assets=colnames(R), 
                             weight_seq=generatesequence(by=0.005)
)
# Add leverage constraint
RiskBudget.portf <- add.constraint(portfolio=RiskBudget.portf, 
                             type="leverage", 
                             min_sum=0.99, # set to speed up RP
                             max_sum=1.01
)
# Establish position bounds
RiskBudget.portf <- add.constraint(portfolio=RiskBudget.portf, 
                  type="box", 
                  min=0.05, 
                  max=1.0
)
# Maximize mean return
RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
                                  type="return",
                                  name="mean"
                                  )
# Add a risk measure
# Use ES to be consistent with risk measures in other BUOY portfolios
RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
                                  type="risk",
                                  name="ES",
                                  multiplier=1,
                                  arguments = list(p=(1-1/12), clean=clean)
                                  )

# Set risk budget limits
RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
                                  type="risk_budget",
                                  name="ES",
                                  max_prisk=0.4,
                                  arguments = list(p=(1-1/12), clean=clean)
                                  )
# Calculate portfolio variance, but don't use it in the objective; used only for plots
RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
                                  type="risk", # the kind of objective this is
                                  name="StdDev", # the function to minimize
                                  enabled=TRUE, # enable or disable the objective
                                  multiplier=0, # calculate it but don't use it in the objective
                                  arguments=list(clean=clean)
)

#------------------------------------------------------------------------
### Evaluate portfolio objective objects
# Generate a single set of random portfolios to evaluate against all constraint set
print(paste('constructing random portfolios at',Sys.time()))
rp = random_portfolios(portfolio=init.portf, permutations=permutations)
print(paste('done constructing random portfolios at',Sys.time()))

start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))

### Evaluate BUOY 1: Constrained Mean-StdDev Portfolio - with ROI
MeanSD.ROI<-optimize.portfolio(R=R,
  portfolio=MeanSD.portf,
  optimize_method='ROI',
  trace=TRUE
) 
plot(MeanSD.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Mean-Volatility Portfolio")
save(MeanSD.ROI,file=paste(resultsdir, 'MeanSD-', Sys.Date(), '-', runname, '.rda',sep='')) # Save the results
print(paste('Completed meanSD optimization at',Sys.time(),'moving on to meanmETL'))

### Evaluate BUOY 2: Constrained Mean-mETL Portfolio - with ROI
MeanmETL.ROI<-optimize.portfolio(R=R,
  portfolio=MeanmETL.portf,
  optimize_method='ROI',
  trace=TRUE, verbose=TRUE
  ) 
plot(MeanmETL.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Mean-mETL Portfolio")
plot(MeanmETL.ROI, risk.col="ES", return.col="mean", rp=permutations, chart.assets=TRUE, main="Mean-mETL Portfolio")
save(MeanmETL.ROI,file=paste(resultsdir, 'MeanETL-', Sys.Date(), '-', runname, '.rda',sep=''))
chart.EfficientFrontier(MeanmETL.RND)
print(paste('Completed meanmETL optimization at',Sys.time(),'moving on to MinSD'))

MeanmETL.RND<-optimize.portfolio(R=R,
                                 portfolio=MeanmETL.portf,
                                 optimize_method='random',
                                 search_size=10000,
                                 trace=TRUE
) 
plot(MeanmETL.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")
plot(MeanmETL.RND, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")

### Evaluate BUOY 3: Constrained Minimum Variance Portfolio - with ROI
MinSD.ROI<-optimize.portfolio(R=R,
  portfolio=MinSD.portf,
  optimize_method='ROI',
  trace=TRUE, verbose=TRUE
  ) # 
plot(MinSD.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum Volatility Portfolio")
save(MinSD.ROI,file=paste(resultsdir, 'MinSD-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed MinSD optimization at',Sys.time(),'moving on to MinmETL'))

### Evaluate BUOY 4: Constrained Minimum mETL Portfolio - with ROI
MinmETL.ROI<-optimize.portfolio(R=R,
  portfolio=MinmETL.portf,
  optimize_method='ROI',
  trace=TRUE, verbose=TRUE,
  ) 
plot(MinmETL.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum mETL Portfolio")
plot(MinmETL.ROI, risk.col="ES", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum mETL Portfolio")
save(MinmETL.ROI,file=paste(resultsdir, 'MinmETL-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed MinmETL optimization at',Sys.time(),'moving on to EqSD'))

### Evaluate BUOY 5: Constrained Equal Variance Contribution Portfolio - with RP
EqSD.RND<-optimize.portfolio(R=R,
  portfolio=EqSD.portf,
  optimize_method='random',
  search_size=1000, trace=TRUE
  ) 
plot(EqSD.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
chart.RiskBudget(EqSD.RND, risk.type="percentage", neighbors=25)
save(EqSD.RND,file=paste(resultsdir, 'EqSD-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed EqSD optimization at',Sys.time(),'moving on to EqmETL'))

EqSD.DE<-optimize.portfolio(R=R,
                             portfolio=EqSD.portf,
                             optimize_method='DEoptim',
                             search_size=1000, trace=TRUE, verbose=TRUE
) 
plot(EqSD.DE, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
chart.RiskBudget(EqSD.DE, risk.type="percentage")


### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio - with RP
EqmETL.RND<-optimize.portfolio(R=R,
  portfolio=EqmETL.portf,
  optimize_method='random',
  search_size=1000, trace=TRUE
  ) # 
plot(EqmETL.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
plot(EqmETL.RND, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
chart.RiskBudget(EqmETL.RND, neighbors=25)
save(EqmETL.RND,file=paste(resultsdir, 'EqmETL-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed EqmETL optimization at',Sys.time(),'moving on to RiskBudget'))

### Evaluate BUOY 7: Equal Weight Portfolio
# There's only one, so calculate it.
#@ Create a portfolio object with all the objectives we want calculated. - RB
EqWt.portf <- portfolio.spec(assets=colnames(R))
EqWt.portf <- add.constraint(portfolio=EqWt.portf, type="leverage", min_sum=0.99, max_sum=1.01)
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="return", name="mean")
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="risk_budget", name="ES", arguments=list(p=p, clean=clean))
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="risk_budget", name="StdDev", arguments=list(clean=clean))

#@ Calculate the objective measures for the equal weight portfolio - RB
EqWt.opt <- equal.weight(R=R, portfolio=EqWt.portf)


### Evaluate Risk Budget Portfolio - with DE
registerDoSEQ() # turn off parallelization to keep the trace data
RiskBudget.DE<-optimize.portfolio(R=R,
                               portfolio=RiskBudget.portf,
                               optimize_method='DEoptim',
                               search_size=1000, trace=TRUE
                               ) # use the same random portfolios generated above
plot(RiskBudget.DE, risk.col="StdDev", return.col="mean")
plot(RiskBudget.DE, risk.col="ES", return.col="mean") # several outlier portfolios
chart.RiskBudget(RiskBudget.DE)
chart.RiskBudget(RiskBudget.DE, risk.type="percentage")

save(RiskBudget.DE,file=paste(resultsdir, 'RiskBudget-', Sys.Date(), '-', runname, '.rda',sep=''))
print(RiskBudget.DE$elapsed_time)
print('Done with optimizations.')


### Combine optimization objects
buoys <- combine.optimizations(list(MeanSD=MeanSD.ROI, MeanmETL=MeanmETL.ROI, MinSD=MinSD.ROI, MinmETL=MinmETL.ROI, EqSD=EqSD.RND, EqmETL=EqmETL.RND, RB=RiskBudget.DE, EqWt=EqWt.opt))
# how to add an EqWgt to this list?
#@ The elements of this list need to be optimize.portfolio objects, so unfortunately we
#@ can't do this unless we created an optimize.portfolio object for an equal weight
#@ portfolio. I'll add this. - RB
chart.Weights(buoys, plot.type="bar", ylim=c(0,1))

#@ Chart the portfolios that have mean and ES as objective measures. - RB
chart.RiskReward(buoys, risk.col="ES")
#@ Chart the portfolios that have mean and StdDev as objective measures. - RB
chart.RiskReward(buoys, risk.col="StdDev")

#@ The EqmETL and RB optimizations would be good to compare because they are
#@ similar in that they both include component ES as an objective. - RB
buoyETL <- combine.optimizations(list(EqmETL=EqmETL.RND, RB=RiskBudget.DE, EqWt=EqWt.opt))
chart.RiskBudget(buoyETL, match.col="ES", risk.type="percentage", legend.loc="topright")

#@ Compare the equal weight portfolio and the equal SD contribution portfolio. - RB
buoyStdDev <- combine.optimizations(list(EqSD=EqSD.RND, EqWt=EqWt.opt))
chart.RiskBudget(buoyStdDev, match.col="StdDev", risk.type="absolute", legend.loc="topleft")

Wgts = extractWeights(buoys)

# Extract portfolio measures from each objective
## We can't just extract them, because they aren't all calculated
## so fill them in...
portfmeas=NULL
for(i in 1:NROW(Wgts)){
  mean = sum(colMeans(R)*Wgts[i,])
  sd = StdDev(R, weights=Wgts[i,])
  es = ES(R, weights=Wgts[i,], method="modified", portfolio_method="component", p=p)
  portfmeas=rbind(portfmeas, c(mean, sd[1], es[1]))
}
colnames(portfmeas)=c("Mean", "StdDev", "mETL")
rownames(portfmeas)=rownames(Wgts)

end_time<-Sys.time()
end_time-start_time


#########################################################################
# Optimization ends here
#########################################################################


