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
registerDoMC(6)

# Available on r-forge
# require(FactorAnalytics) # development version > build 

### Set script constants
runname='historical.moments'
rebalance_period = 'years' #'quarters' # uses endpoints identifiers from xts; how to do semi-annual?
clean = "boudt" # "none" 
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
  type="leverage"
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
  name="var", # name of the function
  arguments=list(clean=clean)
)

### Construct BUOY 2: Constrained Mean-mETL Portfolio - using ROI
#@ Cannot maximize mean return per unit ETL with ROI, consider using
#@ random portfolios or DEoptim. - RB
# Add the return and mETL objectives
MeanmETL.portf <- add.objective(portfolio=init.portf,
  type="return", # the kind of objective this is
  name="mean" # name of the function
  , multiplier=-12
  )
MeanmETL.portf <- add.objective(portfolio=MeanmETL.portf,
  type="risk", # the kind of objective this is
  name="ES", # the function to minimize
  arguments=list(p=p, clean=clean)
  )

### Construct BUOY 3: Constrained Minimum Variance Portfolio - using ROI
# Add the variance objective
MinSD.portf <- add.objective(portfolio=init.portf,
  type="risk", # the kind of objective this is
  name="var", # name of the function
  arguments=list(p=p, clean=clean)
)

### Construct BUOY 4: Constrained Minimum mETL Portfolio - using ROI
# Add the mETL objective
MinmETL.portf <- add.objective(portfolio=init.portf,
  type="risk", # the kind of objective this is
  name="ES", # the function to minimize
  arguments=list(p=p, clean=clean)
)

### Construct BUOY 5: Constrained Equal Variance Contribution Portfolio - using RP
#@ - Add the sub-objectives first. Adding these 3 objectives means that we are
#@ maximizing mean per unit StdDev with equal volatility contribution portfolios. - RB
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
# MRCSD.portf <- add.objective(portfolio=init.portf,
#                             type="risk",
#                             name="StdDev"
# ) # OR
# MRCSD.portf <- add.objective(portfolio=init.portf,
#   type="return",
#   name="mean"
# )
MRCSD.portf <- add.objective(portfolio=init.portf, 
  type="risk_budget", 
  name="StdDev",  
  min_concentration=TRUE,
  arguments = list(clean=clean)
)

# MRCSD.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# MRCSD.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio - using RP
#@ Add the sub-objectives first. These should be added to the MRCmETL portfolio.
#@ All objectives below mean that we are maximizing mean return per unit ES with
#@ equal ES contribution. - RB
# Without a sub-objective, we get a somewhat undefined result, since there are (potentially) many Equal SD contribution portfolios.
# MRCmETL.portf <- add.objective(portfolio=init.portf,
#                             type="risk",
#                             name="ES"
# ) # OR
MRCmETL.portf <- add.objective(portfolio=init.portf,
                            type="return",
                            name="mean"
)
MRCmETL.portf <- add.objective(MRCmETL.portf,
                              type="risk_budget",
                              name="ES",
                              min_concentration=TRUE,
                              arguments = list(p=p, clean=clean)
)
# Calculate portfolio variance, but don't use it in the objective; used only for plots
MRCmETL.portf <- add.objective(portfolio=MRCmETL.portf,
                                  type="risk", # the kind of objective this is
                                  name="StdDev", # the function to minimize
                                  enabled=TRUE, # enable or disable the objective
                                  multiplier=0, # calculate it but don't use it in the objective
                                  arguments=list(clean=clean)
)
# MRCmETL.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# MRCmETL.portf$constraints[[1]]$max_sum = 1.01

### Construct BUOY 7: Equal Weight Portfolio
# There's only one, so create a portfolio object with all the objectives we want calculated. 
EqWgt.portf <- portfolio.spec(assets=colnames(R))
EqWgt.portf <- add.constraint(portfolio=EqWgt.portf, type="leverage", min_sum=1, max_sum=1)
EqWgt.portf <- add.objective(portfolio=EqWgt.portf, type="return", name="mean")
EqWgt.portf <- add.objective(portfolio=EqWgt.portf, type="risk_budget", name="ES", arguments=list(p=p, clean=clean))
EqWgt.portf <- add.objective(portfolio=EqWgt.portf, type="risk_budget", name="StdDev", arguments=list(clean=clean))

### Construct BUOY 8: Inverse Volatility Portfolio
# There's only one, so create a portfolio object with all the objectives we want calculated. 
VolWgt.portf <- portfolio.spec(assets=colnames(R))
VolWgt.portf <- add.constraint(portfolio=VolWgt.portf, type="leverage", min_sum=0.99, max_sum=1.01)
VolWgt.portf <- add.objective(portfolio=VolWgt.portf, type="return", name="mean")
VolWgt.portf <- add.objective(portfolio=VolWgt.portf, type="risk_budget", name="ES", arguments=list(p=p, clean=clean))
VolWgt.portf <- add.objective(portfolio=VolWgt.portf, type="risk_budget", name="StdDev", arguments=list(clean=clean))

# REMOVED - to much to show already
# ### Construct RISK BUDGET Portfolio
# ConstrConcmETL.portf <- portfolio.spec(assets=colnames(R), 
#   weight_seq=generatesequence(by=0.005)
# )
# # Add leverage constraint
# ConstrConcmETL.portf <- add.constraint(portfolio=RiskBudget.portf, 
#   type="leverage", 
#   min_sum=0.99, # set to speed up RP, DE
#   max_sum=1.01
# )
# # Establish position bounds
# ConstrConcmETL.portf <- add.constraint(portfolio=ConstrConcmETL.portf, 
#   type="box", 
#   min=0.01, # leave relatively unconstrained
#   max=1.0
# )
# # Maximize mean return
# ConstrConcmETL.portf <- add.objective(portfolio=ConstrConcmETL.portf,
#   type="return", # maximize return
#   name="mean",
#   multiplier=12
# )
# # Add a risk measure
# # Use ES to be consistent with risk measures in other BUOY portfolios
# ConstrConcmETL.portf <- add.objective(portfolio=ConstrConcmETL.portf,
#   type="risk",
#   name="ETL", # using a different name to avoid clobbering slot below, workaround for bug
#   multiplier=1,
#   arguments = list(p=p, clean=clean)
# )
# 
# # Set contribution limits
# ConstrConcmETL.portf <- add.objective(portfolio=ConstrConcmETL.portf,
#   type="risk_budget",
#   name="ES",
#   max_prisk=0.3, # Sets the maximum percentage contribution to risk
#   arguments = list(p=p, clean=clean)
# )
# Calculate portfolio variance, but don't use it in the objective; used only for plots
# ConstrConcmETL.portf <- add.objective(portfolio=ConstrConcmETL.portf,
#   type="risk", # the kind of objective this is
#   name="StdDev", # the function to minimize
#   enabled=TRUE, # enable or disable the objective
#   multiplier=0, # calculate it but don't use it in the objective
#   arguments=list(clean=clean)
# )

#------------------------------------------------------------------------
### Evaluate portfolio objective objects
#------------------------------------------------------------------------
# Generate a single set of random portfolios to evaluate against all RP constraint sets
print(paste('constructing random portfolios at',Sys.time()))

# Modify the init.portf specification to get RP running 
rp.portf <- init.portf
# rp.portf$constraints[[1]]$min_sum = 0.99 # set to speed up RP
# rp.portf$constraints[[1]]$max_sum = 1.01
rp.portf$constraints[[1]]$min_sum = 1.00 # for more accuracy
rp.portf$constraints[[1]]$max_sum = 1.00
# rp = random_portfolios(portfolio=rp.portf, permutations=30000, max_permutations=400) # will get fewer with less accuracy
load(file=paste(resultsdir,'random-portfolios-2013-10-05.historical.moments.rda'))
rp.mean = apply(rp, 1, function(w) mean(R %*% w))
rp.sd = apply(rp, 1, function(x) StdDev(R=R, weights=x, p=p, clean=clean))
plot(rp.sd, rp.mean, col="darkgray", cex=0.5)

# REMOVED: This was fruitless
# rp1 = random_portfolios(portfolio=rp.portf, permutations=10000, max_permutations=400, rp_method="sample")
# rp1.mean = apply(rp1, 1, function(w) mean(R %*% w))
# rp1.sd = apply(rp1, 1, function(x) StdDev(R=R, weights=x, p=p))
# rp1.etl=NULL; for(i in 1:NROW(rp1)) {rp1.etl[i]=ETL(R=R, weights=as.vector(rp1[i,]), p=p, portfolio_method="component")[[1]]}
# plot(rp1.sd, rp1.mean, col="gray", cex=0.5)
# 
# rp2 = random_portfolios(portfolio=rp.portf, permutations=10000, max_permutations=400, rp_method="simplex", fev=2)
# rp2.mean = apply(rp2, 1, function(w) mean(R %*% w))
# rp2.sd = apply(rp2, 1, function(x) StdDev(R=R, weights=x, p=p))
# points(rp2.sd,rp2.mean, col="blue", cex=0.5)
# 
# rp3 = random_portfolios(portfolio=rp.portf, permutations=10000, max_permutations=400, rp_method="grid")
# rp3.mean = apply(rp3, 1, function(w) mean(R %*% w))
# rp3.sd = apply(rp3, 1, function(x) StdDev(R=R, weights=x, p=p))
# points(rp3.sd,rp3.mean, col="green", cex=0.5)

# print(paste('done constructing random portfolios at',Sys.time()))
# save(rp,file=paste(resultsdir, 'random-portfolios-', Sys.Date(), '-', runname, '.rda',sep=''))


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

### Evaluate BUOY 2: Constrained Mean-mETL Portfolio - with DE or RND
# Not possible with ROI - RB
# MeanmETL.ROI<-optimize.portfolio(R=R,
#   portfolio=MeanmETL.portf,
#   optimize_method='ROI',
#   trace=TRUE, verbose=TRUE
#   ) 
# 

# So use random portfolios instead
MeanmETL.RND<-optimize.portfolio(R=R,
                                 portfolio=MeanmETL.portf,
                                 optimize_method='random',
                                 rp=rp,
                                 trace=TRUE
) 
plot(MeanmETL.RND, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Mean-mETL Portfolio")
plot(MeanmETL.RND, risk.col="ES", return.col="mean", rp=permutations, chart.assets=TRUE, main="Mean-mETL Portfolio")
save(MeanmETL.RND,file=paste(resultsdir, 'MeanETL-RP-', Sys.Date(), '-', runname, '.rda',sep=''))
chart.EfficientFrontier(MeanmETL.RND)

# OR with DE optim
MeanmETL.DE<-optimize.portfolio(R=R,
  portfolio=MeanmETL.portf,
  optimize_method='DEoptim',
  search_size=20000,
  initialpop=rp[1:50,] # seed with a starting population that we know fits the constraint space
) 
plot(MeanmETL.DE, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")
plot(MeanmETL.DE, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Mean-mETL Portfolio")
save(MeanmETL.DE,file=paste(resultsdir, 'MeanETL-DE-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed meanmETL optimization at',Sys.time(),'moving on to MinSD'))


### Evaluate BUOY 3: Constrained Minimum Variance Portfolio - with ROI
MinSD.ROI<-optimize.portfolio(R=R,
  portfolio=MinSD.portf,
  optimize_method='ROI', 
  trace=TRUE
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
  trace=TRUE, verbose=TRUE
  ) 
plot(MinmETL.ROI, risk.col="StdDev", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum mETL Portfolio")
plot(MinmETL.ROI, risk.col="ES", return.col="mean", rp=permutations, chart.assets=TRUE, main="Minimum mETL Portfolio")
save(MinmETL.ROI,file=paste(resultsdir, 'MinmETL-', Sys.Date(), '-', runname, '.rda',sep=''))
print(paste('Completed MinmETL optimization at',Sys.time(),'moving on to MRCSD'))

### Evaluate BUOY 5: Constrained Equal Variance Contribution Portfolio - with RP
# MRCSD.RND<-optimize.portfolio(R=R,
#   portfolio=MRCSD.portf,
#   optimize_method='random',
#   rp=rp,
#   trace=TRUE
#   ) 
# plot(MRCSD.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
# chart.RiskBudget(MRCSD.RND, risk.type="percentage", neighbors=25)
# save(MRCSD.RND,file=paste(resultsdir, 'MRCSD.RND-', Sys.Date(), '-', runname, '.rda',sep=''))
# ... not a very satisfying solution

# OR DE optim - this gets very close (a nice, straight line), so use it
MRCSD.DE<-optimize.portfolio(R=R,
  portfolio=MRCSD.portf,
  optimize_method='DEoptim',
  search_size=20000,
  itermax=400,
  initialpop=rp[1:50,], # seed with a starting population that we know fits the constraint space
  trace=FALSE
  ) 
plot(MRCSD.DE, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
chart.RiskBudget(MRCSD.DE, risk.type="percentage", neighbors=25)
save(MRCSD.DE,file=paste(resultsdir, 'MRCSD.DE-', Sys.Date(), '-', runname, '.rda',sep=''))

print(paste('Completed MRCSD optimization at',Sys.time(),'moving on to MRCmETL'))

### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio - with RP
MRCmETL.RND<-optimize.portfolio(R=R,
  portfolio=MRCmETL.portf,
  optimize_method='random',
  rp=rp,
  trace=TRUE
  ) # 
plot(MRCmETL.RND, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
plot(MRCmETL.RND, risk.col="ES", return.col="mean", chart.assets=TRUE, main="Equal mETL Contribution Portfolio")
chart.RiskBudget(MRCmETL.RND, neighbors=25)
save(MRCmETL.RND,file=paste(resultsdir, 'MRCmETL-', Sys.Date(), '-', runname, '.rda',sep=''))

# OR DE optim - 
MRCmETL.DE<-optimize.portfolio(R=R,
  portfolio=MRCmETL.portf,
  optimize_method='DEoptim',
  search_size=20000,
  NP=200,
  initialpop=rp[1:50,], # seed with a starting population that we know fits the constraint space
  trace=FALSE
  ) 
plot(MRCmETL.DE, risk.col="StdDev", return.col="mean", chart.assets=TRUE, main="Equal Volatility Contribution Portfolio")
chart.RiskBudget(MRCmETL.DE, risk.type="percentage", neighbors=25)
save(MRCmETL.DE,file=paste(resultsdir, 'MRCmETL.DE-', Sys.Date(), '-', runname, '.rda',sep=''))

# # test it unconstrained:
# unconstr.portf <- portfolio.spec(assets=colnames(R), 
#                              weight_seq=generatesequence(by=0.005)
# )
# unconstr.portf <- add.constraint(portfolio=unconstr.portf, 
#                              type="leverage", 
#                              min_sum=0.99, # set to speed up RP
#                              max_sum=1.01
# )
# # Establish position bounds
# unconstr.portf <- add.constraint(portfolio=unconstr.portf, 
#                   type="box", 
#                   min=0.01, 
#                   max=1.0
# )
# MRCmETLun.portf <- add.objective(portfolio=unconstr.portf,
#                             type="return",
#                             name="mean"
# )
# MRCmETLun.portf <- add.objective(MRCmETL.portf,
#                               type="risk_budget",
#                               name="ES",
#                               min_concentration=TRUE,
#                               arguments = list(p=p, clean=clean)
# )
# 
# # ...in DE optim - 
# MRCmETLun.DE<-optimize.portfolio(R=R,
#   portfolio=MRCmETLun.portf,
#   optimize_method='DEoptim',
#   search_size=20000,
#   NP=200,
#   initialpop=rp[1:50,], # seed with a starting population that we know fits the constraint space
#   trace=FALSE
#   ) 

print(paste('Completed MRCmETL optimization at',Sys.time(),'moving on to RiskBudget'))

### Evaluate BUOY 7: Equal Weight Portfolio
# Calculate the objective measures for the equal weight portfolio
EqWgt.opt <- equal.weight(R=R, portfolio=EqWgt.portf)

### Evaluate BUOY 8: Inverse Volatility Portfolio
volatility.weight <- function (R, portfolio, ...) 
{
  if (!is.portfolio(portfolio)) 
      stop("portfolio object passed in must be of class 'portfolio'")
  assets <- portfolio$assets
  nassets <- length(assets)
  weights <- as.vector((1/StdDev(R))/sum(1/StdDev(R)))
  names(weights) <- names(assets)
  if (ncol(R) != nassets) {
      if (ncol(R) > nassets) {
          R <- R[, 1:nassets]
          warning("number of assets is less than number of columns in returns object, subsetting returns object.")
      }
      else {
          stop("number of assets is greater than number of columns in returns object")
      }
  }
  out <- constrained_objective(w = weights, R = R, portfolio = portfolio, 
      trace = TRUE, ...)$objective_measures
  return(structure(list(R = R, weights = weights, objective_measures = out, 
      call = match.call(), portfolio = portfolio), class = c("optimize.portfolio.invol", 
      "optimize.portfolio")))
}
# Calculate the objective measures for the vol weight portfolio
VolWgt.opt <- volatility.weight(R=R, portfolio=VolWgt.portf)

# REMOVED
# ### Evaluate Constrained Concentration to mETL Portfolio - with DE
# # registerDoSEQ() # turn off parallelization to keep the trace data
# ConstrConcmETL.DE<-optimize.portfolio(R=R,
#   portfolio=ConstrConcmETL.portf,
#   optimize_method='DEoptim',
#   search_size=40000,
#   NP=4000,
#   itermax=400,
#   trace=FALSE
# ) 
# #   list(c=0.25, # speed of crossover adaption (0,1]
# #   CR=0.75) # crossover probability [0,1]
# plot(ConstrConcmETL.DE, risk.col="StdDev", return.col="mean")
# plot(ConstrConcmETL.DE, risk.col="ES", return.col="mean") # several outlier portfolios
# chart.RiskBudget(ConstrConcmETL.DE)
# chart.RiskBudget(ConstrConcmETL.DE, risk.type="percentage")
# 
# save(ConstrConcmETL.DE,file=paste(resultsdir, 'ConstrConcmETL-', Sys.Date(), '-', runname, '.rda',sep=''))
# print(ConstrConcmETL.DE$elapsed_time)
print('Done with optimizations.')

#------------------------------------------------------------------------
### Extract data from optimizations for analysis
#------------------------------------------------------------------------
# Combine optimization objects
buoys <- combine.optimizations(list(MeanSD=MeanSD.ROI, MeanmETL=MeanmETL.RND, MinSD=MinSD.ROI, MinmETL=MinmETL.ROI, MRCSD=MRCSD.DE, MRCmETL=MRCmETL.DE, VolWgt=VolWgt.opt, EqWgt=EqWgt.opt))
# chart.Weights(buoys, plot.type="bar", ylim=c(0,1))
# 
# #@ Chart the portfolios that have mean and ES as objective measures. - RB
# chart.RiskReward(buoys, risk.col="ES")
# #@ Chart the portfolios that have mean and StdDev as objective measures. - RB
# chart.RiskReward(buoys, risk.col="StdDev")
# 
# #@ The MRCmETL and RB optimizations would be good to compare because they are
# #@ similar in that they both include component ES as an objective. - RB
# buoyETL <- combine.optimizations(list(MRCmETL=MRCmETL.RND, RB=RiskBudget.DE, EqWgt=EqWgt.opt))
# chart.RiskBudget(buoyETL, match.col="ES", risk.type="percentage", legend.loc="topright")
# 
# #@ Compare the equal weight portfolio and the equal SD contribution portfolio. - RB
# buoyStdDev <- combine.optimizations(list(MRCSD=MRCSD.RND, EqWgt=EqWgt.opt))
# chart.RiskBudget(buoyStdDev, match.col="StdDev", risk.type="absolute", legend.loc="topleft")

Wgts = extractWeights(buoys)

### Extract portfolio measures from each objective
# We can't just extract them, because they aren't all calculated
# so fill them in...
buoys.portfmeas =  buoys.contrib.sd = buoys.contrib.es = buoys.perc.sd = buoys.perc.es = NULL
for(i in 1:NROW(Wgts)){
  mean = sum(colMeans(R)*Wgts[i,])
  sd = StdDev(R, weights=Wgts[i,], portfolio_method="component", clean=clean)
  es = ES(R, weights=Wgts[i,], method="modified", portfolio_method="component", p=p, clean=clean)
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
xtract = extractStats(MRCmETL.RND) 
save(xtract,file=paste(resultsdir, 'xtract-RPs-', Sys.Date(), '-', runname, '.rda',sep=''))
# columnnames = colnames(xtract)
results.names=rownames(buoys.portfmeas)

# by Asset metrics
assets.portfmeas=as.matrix(scatterFUN(R, FUN="mean"))
assets.portfmeas=cbind(assets.portfmeas, scatterFUN(R, FUN="StdDev", clean=clean))
assets.portfmeas=cbind(assets.portfmeas, scatterFUN(R, FUN="ES", clean=clean))
colnames(assets.portfmeas)=c("Mean", "StdDev", "mETL")
rownames(assets.portfmeas)=colnames(Wgts)



#------------------------------------------------------------------------
# Run select buoy optimizations through time
#------------------------------------------------------------------------
# 

# Equal Weight
dates=index(R[endpoints(R, on=rebalance_period)])
EqWgt.w = xts(matrix(rep(1/NCOL(R),length(dates)*NCOL(R)), ncol=NCOL(R)), order.by=dates)
colnames(EqWgt.w)= colnames(R)
EqWgt.R=Return.rebalancing(R, EqWgt.w)
chart.StackedBar(EqWgt.w, colorset=wb13color, gap=0)

# Equal SD
MRCSD.DE.t = optimize.portfolio.rebalancing(R=R,
  portfolio=MRCSD.portf, 
  optimize_method='DEoptim',
  search_size=20000,
  NP=200,
  initialpop=rp[1:50,], # seed with a starting population that we know fits the constraint space
  trace=FALSE,
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MRCSD.w = extractWeights(MRCSD.DE.t)
MRCSD.gw = extractGroups(MRCSD.DE.t)
save(MRCSD.DE.t,file=paste(resultsdir, 'MRCSD.DE.t-', Sys.Date(), '-', runname, '.rda',sep=''))
chart.UnStackedBar(MRCSD.w, rotate="horizontal", colorset=wb13color, space=0, las=2)
MRCSD.R=Return.rebalancing(edhec.R, MRCSD.w)
colnames(MRCSD) = "MRCSD"

# Extract perc contrib of mES from results object
x=NULL
for(i in 1:length(names(MRCSD.DE.t)))  {
  x = rbind(x,MRCSD.DE.t[[i]][["objective_measures"]]$StdDev$pct_contrib_StdDev)
}
MRCSD.DE.pct_contrib_StdDev.t = as.xts(x, order.by=as.POSIXct(names(MRCSD.DE.t)))
chart.UnStackedBar(x.xts, rotate="horizontal", colorset=wb13color, space=0, las=2)


# MRC mETL
MRCmETL.DE.t = optimize.portfolio.rebalancing(R=R,
  portfolio=MRCmETL.portf, 
  optimize_method='DEoptim',
  search_size=20000,
  NP=200,
  initialpop=rp[1:50,], # seed with a starting population that we know fits the constraint space
  trace=FALSE,
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MRCmETL.w = extractWeights(MRCmETL.DE.t)
MRCmETL.gw = extractGroups(MRCmETL.DE.t)
save(MRCmETL.DE.t,file=paste(resultsdir, 'MRCmETL.DE.t-', Sys.Date(), '-', runname, '.rda',sep=''))
chart.UnStackedBar(MRCmETL.w, rotate="horizontal", colorset=wb13color, space=0, las=2)
MRCmETL=Return.rebalancing(edhec.R, MRCmETL.w)
colnames(MRCmETL) = "MRCmETL"

MRCmETL.RND.t = optimize.portfolio.rebalancing(R=R,
  portfolio=MRCmETL.portf, 
  optimize_method='random',
  rp=rp,
  trace=TRUE,
  rebalance_on=rebalance_period, # uses xts 'endpoints'
  trailing_periods=NULL, # calculates from inception
  training_period=36) # starts 3 years in to the data history
MRCmETL.RND.w = extractWeights(MRCmETL.RND.t)
MRCmETL.gw = extractGroups(MRCmETL.RND.t)
save(MRCmETL.DE.t,file=paste(resultsdir, 'MRCmETL.DE.t-', Sys.Date(), '-', runname, '.rda',sep=''))
chart.UnStackedBar(MRCmETL.w, rotate="horizontal", colorset=wb13color, space=0, las=2)
MRCmETL=Return.rebalancing(edhec.R, MRCmETL.w)
colnames(MRCmETL) = "MRCmETL"
end_time<-Sys.time()
end_time-start_time


#########################################################################
# Optimization ends here
#########################################################################


