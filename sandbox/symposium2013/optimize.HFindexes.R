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
#rebalance_period = 'quarters' # uses endpoints identifiers from xts; how to do semi-annual?
clean = "none" #"boudt" # "none" 
permutations = 2000
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
                             type="full_investment"
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
# EqSD.portf <- add.objective(portfolio=init.portf,
#                             type="risk",
#                             name="StdDev"
# ) # OR
EqSD.portf <- add.objective(portfolio=init.portf,
                            type="return",
                            name="mean"
)
EqSD.portf <- add.objective(portfolio=EqSD.portf, 
                            type="risk_budget", 
                            name="StdDev",  
                            min_concentration=TRUE,
                            arguments = list(clean=clean)
                            )

# EqSD.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# EqSD.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio - using RP
#@ Add the sub-objectives first. These should be added to the EqmETL portfolio.
#@ All objectives below mean that we are maximizing mean return per unit ES with
#@ equal ES contribution. - RB
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
# EqmETL.portf <- add.objective(portfolio=init.portf,
#                             type="risk",
#                             name="ES"
# ) # OR
EqmETL.portf <- add.objective(portfolio=init.portf,
                            type="return",
                            name="mean"
)
EqmETL.portf <- add.objective(EqmETL.portf,
                              type="risk_budget",
                              name="ES",
                              min_concentration=TRUE,
                              arguments = list(p=p, clean=clean)
)
# Calculate portfolio variance, but don't use it in the objective; used only for plots
EqmETL.portf <- add.objective(portfolio=EqmETL.portf,
                                  type="risk", # the kind of objective this is
                                  name="StdDev", # the function to minimize
                                  enabled=TRUE, # enable or disable the objective
                                  multiplier=0, # calculate it but don't use it in the objective
                                  arguments=list(clean=clean)
)
# EqmETL.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# EqmETL.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so create a portfolio object with all the objectives we want calculated. 
EqWt.portf <- portfolio.spec(assets=colnames(R))
EqWt.portf <- add.constraint(portfolio=EqWt.portf, type="leverage", min_sum=0.99, max_sum=1.01)
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="return", name="mean")
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="risk_budget", name="ES", arguments=list(p=p, clean=clean))
EqWt.portf <- add.objective(portfolio=EqWt.portf, type="risk_budget", name="StdDev", arguments=list(clean=clean))

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
# RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
#                                   type="risk",
#                                   name="ES",
#                                   multiplier=1,
#                                   arguments = list(p=(1-1/12), clean=clean)
#                                   )

# Set risk budget limits
RiskBudget.portf <- add.objective(portfolio=RiskBudget.portf,
                                  type="risk_budget",
                                  name="ES",
                                  max_prisk=0.3,
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
# Generate a single set of random portfolios to evaluate against all RP constraint sets
print(paste('constructing random portfolios at',Sys.time()))

# Modify the init.portf specification to get RP running 
rp.portf <- init.portf
# rp.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# rp.portf$constraints[[1]]$max_sum = 1.01
# rp = random_portfolios(portfolio=rp.portf, permutations=10000, max_permutations=400)
# print(paste('done constructing random portfolios at',Sys.time()))
# save(rp,file=paste(resultsdir, 'random-portfolios-', Sys.Date(), '-', runname, '.rda',sep=''))
load(file=paste(resultsdir,'random-portfolios-2013-09-28.historical.moments.rda'))

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
# OR with random portfolios
# MeanmETL.RND<-optimize.portfolio(R=R,
#                                  portfolio=MeanmETL.portf,
#                                  optimize_method='random',
#                                  rp=rp,
#                                  trace=TRUE
# ) 
# plot(MeanmETL.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")
# plot(MeanmETL.RND, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")

### Evaluate BUOY 3: Constrained Minimum Variance Portfolio - with ROI
MinSD.ROI<-optimize.portfolio(R=R,
  portfolio=MinSD.portf,
  optimize_method='ROI',
  trace=TRUE, verbose=TRUE
  ) # 
plot(MinSD.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum Volatility Portfolio with ROI")
save(MinSD.ROI,file=paste(resultsdir, 'MinSD-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed MinSD optimization at',Sys.time(),'moving on to MinmETL'))
# OR with random portfolios
# MinSD.RND<-optimize.portfolio(R=R,
#   portfolio=MinSD.portf,
#   optimize_method='random',
#   rp=rp,
#   trace=TRUE, verbose=TRUE
#   ) # 
# plot(MinSD.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Minimum Volatility Portfolio with RP")

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
  rp=rp,
  trace=TRUE
  ) 
plot(EqSD.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
chart.RiskBudget(EqSD.RND, risk.type="percentage", neighbors=25)
save(EqSD.RND,file=paste(resultsdir, 'EqSD.RND-', Sys.Date(), '-', runname, '.rda',sep=''))


# or with DE
# EqSD.DE<-optimize.portfolio(R=R,
#   portfolio=EqSD.portf,
#   optimize_method='DEoptim',
#   search_size=1000, 
#   trace=TRUE, verbose=TRUE
# ) 
# plot(EqSD.DE, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
# chart.RiskBudget(EqSD.DE, risk.type="percentage")
# save(EqSD.DE,file=paste(resultsdir, 'EqSD.DE-', Sys.Date(), '-', runname, '.rda',sep=''))

print(paste('Completed EqSD optimization at',Sys.time(),'moving on to EqmETL'))

### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio - with RP
EqmETL.RND<-optimize.portfolio(R=R,
  portfolio=EqmETL.portf,
  optimize_method='random',
  rp=rp,
  trace=TRUE
  ) # 
plot(EqmETL.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
plot(EqmETL.RND, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
chart.RiskBudget(EqmETL.RND, neighbors=25)
save(EqmETL.RND,file=paste(resultsdir, 'EqmETL-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed EqmETL optimization at',Sys.time(),'moving on to RiskBudget'))

### Evaluate BUOY 7: Equal Weight Portfolio
# Calculate the objective measures for the equal weight portfolio
EqWt.opt <- equal.weight(R=R, portfolio=EqWt.portf)


### Evaluate Risk Budget Portfolio - with DE
# registerDoSEQ() # turn off parallelization to keep the trace data
RiskBudget.DE<-optimize.portfolio(R=R,
                               portfolio=RiskBudget.portf,
                               optimize_method='DEoptim',
                              search_size=1000, trace=TRUE
                               ) 
plot(RiskBudget.DE, risk.col="StdDev", return.col="mean")
plot(RiskBudget.DE, risk.col="ES", return.col="mean") # several outlier portfolios
chart.RiskBudget(RiskBudget.DE)
chart.RiskBudget(RiskBudget.DE, risk.type="percentage")

save(RiskBudget.DE,file=paste(resultsdir, 'RiskBudget-', Sys.Date(), '-', runname, '.rda',sep=''))
print(RiskBudget.DE$elapsed_time)
print('Done with optimizations.')

#------------------------------------------------------------------------
### Extract data from optimizations for analysis

# Combine optimization objects
buoys <- combine.optimizations(list(MeanSD=MeanSD.ROI, MeanmETL=MeanmETL.ROI, MinSD=MinSD.ROI, MinmETL=MinmETL.ROI, EqSD=EqSD.RND, EqmETL=EqmETL.RND, RB=RiskBudget.DE, EqWt=EqWt.opt))
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

### Extract portfolio measures from each objective
# We can't just extract them, because they aren't all calculated
# so fill them in...
buoys.portfmeas =  buoys.contrib.sd = buoys.contrib.es = buoys.perc.sd = buoys.perc.es = NULL
for(i in 1:NROW(Wgts)){
  mean = sum(colMeans(R)*Wgts[i,])
  sd = StdDev(R, weights=Wgts[i,], portfolio_method="component")
  es = ES(R, weights=Wgts[i,], method="modified", portfolio_method="component", p=p)
  buoys.portfmeas=rbind(buoys.portfmeas, c(mean, sd[[1]][1], es[[1]][1]))
  buoys.contrib.sd= rbind(buoys.contrib.sd,sd[[2]])
  buoys.contrib.es= rbind(buoys.contrib.es,es[[2]])
  buoys.perc.sd = rbind(buoys.perc.sd,sd[[3]])
  buoys.perc.es = rbind(buoys.perc.es,es[[3]])
}
colnames(buoys.portfmeas)=c("Mean", "StdDev", "mETL")
rownames(buoys.portfmeas)=
rownames(buoys.contrib.sd)= 
rownames(buoys.contrib.es)= 
rownames(buoys.perc.sd) = 
rownames(buoys.perc.es) = rownames(Wgts)
colnames(buoys.contrib.sd)= 
colnames(buoys.contrib.es)= 
colnames(buoys.perc.sd) = 
colnames(buoys.perc.es) = colnames(Wgts)

# get the RP portfolios with risk and return pre-calculated
xtract = extractStats(EqmETL.RND) 
save(xtract,file=paste(resultsdir, 'xtract-RPs-', Sys.Date(), '-', runname, '.rda',sep=''))
# columnnames = colnames(xtract)
results.names=rownames(buoys.portfmeas)

# by Asset metrics
assets.portfmeas=as.matrix(scatterFUN(R, FUN="mean"))
assets.portfmeas=cbind(assets.portfmeas, scatterFUN(R, FUN="StdDev"))
assets.portfmeas=cbind(assets.portfmeas, scatterFUN(R, FUN="ES"))
colnames(assets.portfmeas)=c("Mean", "StdDev", "mETL")
rownames(assets.portfmeas)=colnames(Wgts)

end_time<-Sys.time()
end_time-start_time


#########################################################################
# Optimization ends here
#########################################################################


