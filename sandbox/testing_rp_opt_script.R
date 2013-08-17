# script to run examples from script.workshop2012.R using the v2 specification

# The following optimization problems will be run
# mean-StdDev
# - maximize mean-to-volatility (i.e. reward-to-risk)
# - BUOY 1
# minmETL
# - minimize modified Expected Tail Loss
# - BUOY 4
# eqmETL
# - equal risk modified Expected Tail Loss
# - BUOY 6

# Note: The script.workshop2012.R examples use pamean and pasd, I will simply
# use mean and StdDev.

# The script is organized in a way such that the examples from the 
# script.workshop2012.R (modified to work with the current code base) are shown
# first and then implemented using the v2 specification

##### script.workshop2012.R: #####
# v1 code from workshop

##### v2: #####
# v2 code

# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(quantmod)
require(DEoptim)
require(foreach)

# The multicore package, and therefore registerDoMC, should not be used in a
# GUI environment, because multiple processes then share the same GUI. Only use
# when running from the command line
require(doMC)
registerDoMC(3)

data(edhec)

# Drop some indexes and reorder
edhec.R = edhec[,c("Convertible Arbitrage", "Equity Market Neutral","Fixed Income Arbitrage", "Event Driven", "CTA Global", "Global Macro", "Long/Short Equity")]

# Define pamean function
# pamean <- function(n=12, R, weights, geometric=TRUE){
#   as.vector(sum(Return.annualized(last(R,n), geometric=geometric)*weights))
# }

# Define pasd function
# pasd <- function(R, weights){
#   as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
# }

# Set some parameters
rebalance_period = 'quarters' # uses endpoints identifiers from xts
clean = "none" #"boudt"
permutations = 4000


###### script.workshop2012: Initial constraint object #####
## Set up the initial constraints object with constraints and objectives using
## the v1 specification

# A set of box constraints used to initialize ALL the bouy portfolios
# init.constr <- constraint(assets = colnames(edhec.R),
#                           min = .05, # minimum position weight
#                           max = .3, #1, # maximum position weight
#                           min_sum=0.99, # minimum sum of weights must be equal to 1-ish
#                           max_sum=1.01, # maximum sum must also be about 1
#                           weight_seq = generatesequence(by=.005) 
# )
# Add measure 1, annualized return
# init.constr <- add.objective_v1(constraints=init.constr,
#                                 type="return", # the kind of objective this is
#                                 name="mean",
#                                 enabled=TRUE, # enable or disable the objective
#                                 multiplier=0, # calculate it but don't use it in the objective
# )
# Add measure 2, annualized standard deviation
# init.constr <- add.objective_v1(init.constr,
#                                 type="risk", # the kind of objective this is
#                                 name="StdDev", # to minimize from the sample
#                                 enabled=TRUE, # enable or disable the objective
#                                 multiplier=0, # calculate it but don't use it in the objective
# )
# Add measure 3, CVaR with p=(1-1/12)
# set confidence for VaR/ES
# p=1-1/12 # for monthly
#p=.25 # for quarterly
# init.constr <- add.objective_v1(init.constr,
#                                 type="risk", # the kind of objective this is
#                                 name="CVaR", # the function to minimize
#                                 enabled=FALSE, # enable or disable the objective
#                                 multiplier=0, # calculate it but don't use it in the objective
#                                 arguments=list(p=p), 
#                                 clean=clean
# )

##### v2: Initial Portfolio Object #####
## Set up an initial portfolio object with constraints and objectives using
## v2 specification

# Create initial portfolio object used to initialize ALL the bouy portfolios
init.portf <- portfolio.spec(assets=colnames(edhec.R), 
                             weight_seq=generatesequence(by=0.005))
# Add leverage constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="leverage", 
                             min_sum=0.99, 
                             max_sum=1.01)
# Add box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0.05, 
                             max=0.3)

#Add measure 1, annualized return
init.portf <- add.objective(portfolio=init.portf,
                            type="return", # the kind of objective this is
                            name="mean", # name of the function
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

# Add measure 2, annualized standard deviation
init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="StdDev", # to minimize from the sample
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

# Add measure 3, ES with p=(1-1/12)
# set confidence for ES
p=1-1/12 # for monthly

init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="ES", # the function to minimize
                            enabled=FALSE, # enable or disable the objective
                            multiplier=0, # calculate it but don't use it in the objective
                            arguments=list(p=p)
)
# print(init.portf)
# summary(init.portf)

##### script.workshop2012: BUOY 1 #####
### Construct BUOY 1: Constrained Mean-StdDev Portfolio #####
# MeanSD.constr <- init.constr
# Turn back on the return and sd objectives
# MeanSD.constr$objectives[[1]]$multiplier = -1 # mean
# MeanSD.constr$objectives[[2]]$multiplier = 1 # StdDev

##### v2: BUOY 1 #####
### Construct BUOY 1: Constrained Mean-StdDev Portfolio
MeanSD.portf <- init.portf
# Turn back on the return and sd objectives
MeanSD.portf$objectives[[1]]$multiplier = -1 # pamean
MeanSD.portf$objectives[[2]]$multiplier = 1 # pasd
# print(MeanSD.portf)
# summary(MeanSD.portf)

##### script.workshop2012: BUOY 4 #####
### Construct BUOY 4: Constrained Minimum mETL Portfolio
# MinmETL.constr <- init.constr
# Turn back on the mETL objective
# MinmETL.constr$objectives[[3]]$multiplier = 1 # mETL
# MinmETL.constr$objectives[[3]]$enabled = TRUE # mETL

##### v2: BUOY 4 #####
### Construct BUOY 4: Constrained Minimum mETL Portfolio
MinmETL.portf <- init.portf
# Turn back on the mETL objective
MinmETL.portf$objectives[[3]]$multiplier = 1 # mETL
MinmETL.portf$objectives[[3]]$enabled = TRUE # mETL
# print(MinmETL.portf)
# summary(MinmETL.portf)

##### script.workshop2012: BUOY 6 #####
### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio
# EqmETL.constr <- add.objective_v1(init.constr, 
#                                   type="risk_budget", 
#                                   name="CVaR", 
#                                   enabled=TRUE, 
#                                   min_concentration=TRUE, 
#                                   arguments = list(p=(1-1/12), clean=clean))
# EqmETL.constr$objectives[[3]]$multiplier = 1 # min mETL
# EqmETL.constr$objectives[[3]]$enabled = TRUE # min mETL

##### v2: BUOY 6 #####
### Construct BUOY 6: Constrained Equal mETL Contribution Portfolio
EqmETL.portf <- add.objective(init.portf,
                              type="risk_budget",
                              name="ES",
                              enabled=TRUE,
                              min_concentration=TRUE,
                              arguments = list(p=(1-1/12), clean=clean)
)
EqmETL.portf$objectives[[3]]$multiplier = 1 # min mETL
EqmETL.portf$objectives[[3]]$enabled = TRUE # min mETL
# print(EqmETL.portf)
# summary(EqmETL.portf)

### Choose our 'R' variable
R=edhec.R # for monthlies

# Generate a single set of random portfolios to evaluate against all constraint set
print(paste('constructing random portfolios at',Sys.time()))
rp = random_portfolios(portfolio=init.portf, permutations=permutations)
print(paste('done constructing random portfolios at',Sys.time()))

start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))

##### script.workshop2012.R: Evaluate BUOY 1 #####
### Evaluate BUOY 1: Constrained Mean-StdDev Portfolio
# MeanSD.RND<-optimize.portfolio_v1(R=R,
#                                constraints=MeanSD.constr,
#                                optimize_method='random',
#                                search_size=1000, trace=TRUE, verbose=TRUE,
#                                rp=rp) # use the same random portfolios generated above
# plot(MeanSD.RND, risk.col="StdDev", return.col="mean")
# Evaluate the objectives through time 
### requires PortfolioAnalytics build >= 1864
# MeanSD.RND.t = optimize.portfolio.rebalancing_v1(R=R,
#                                               constraints=MeanSD.constr, 
#                                               optimize_method='random', 
#                                               search_size=permutations, trace=TRUE, verbose=TRUE, 
#                                               rp=rp, # all the same as prior
#                                               rebalance_on=rebalance_period, # uses xts 'endpoints'
#                                               trailing_periods=NULL, # calculates from inception
#                                               training_period=36) # starts 3 years in to the data history
# MeanSD.w = extractWeights.rebal(MeanSD.RND.t)
# MeanSD=Return.rebalancing(edhec.R, MeanSD.w)
# colnames(MeanSD) = "MeanSD"

##### v2: Evaluate BUOY 1 #####
MeanSD.RND <- optimize.portfolio(R=R,
                                 portfolio=MeanSD.portf,
                                 optimize_method="random",
                                 trace=TRUE,
                                 rp=rp)
print(MeanSD.RND)
print(MeanSD.RND$elapsed_time)

# Evaluate the objectives with RP through time
# MeanSD.RND.t <- optimize.portfolio.rebalancing(R=R,
#                                                portfolio=MeanSD.portf,
#                                                optimize_method="random",
#                                                trace=TRUE,
#                                                rp=rp,
#                                                rebalance_on=rebalance_period,
#                                                training_period=36)
# MeanSD.w = extractWeights.rebal(MeanSD.RND.t)
# MeanSD=Return.rebalancing(edhec.R, MeanSD.w)
# colnames(MeanSD) = "MeanSD"
# save(MeanSD.RND, MeanSD.RND.t, MeanSD.w, MeanSD, file=paste('MeanSD',Sys.Date(),'rda',sep='.'))

print(paste('Completed meanSD optimization at',Sys.time(),'moving on to MinmETL'))

##### script.workshop2012.R: Evaluate BUOY 4 #####
### Evaluate BUOY 4: Constrained Minimum mETL Portfolio
# MinmETL.RND<-optimize.portfolio_v1(R=R,
#                                    constraints=MinmETL.constr,
#                                    optimize_method='random',
#                                    search_size=1000, trace=TRUE, verbose=TRUE,
#                                    rp=rp) # use the same random portfolios generated above
# plot(MinmETL.RND, risk.col="StdDev", return.col="mean")
# Evaluate the objectives with RP through time 
# MinmETL.RND.t = optimize.portfolio.rebalancing_v1(R=R,
#                                                   constraints=MinmETL.constr, 
#                                                   optimize_method='random', 
#                                                   search_size=permutations, trace=TRUE, verbose=TRUE, 
#                                                   rp=rp, # all the same as prior
#                                                   rebalance_on=rebalance_period, # uses xts 'endpoints'
#                                                   trailing_periods=NULL, # calculates from inception
#                                                   training_period=36) # starts 3 years in to the data history
# MinmETL.w = extractWeights.rebal(MinmETL.RND.t)
# MinmETL=Return.rebalancing(edhec.R, MinmETL.w)
# colnames(MinmETL) = "MinmETL"

##### v2: Evaluate BUOY 4 #####
MinmETL.RND <- optimize.portfolio(R=R,
                                  portfolio=MinmETL.portf,
                                  optimize_method="random",
                                  trace=TRUE,
                                  rp=rp)
print(MinmETL.RND)
print(MinmETL.RND$elapsed_time)

# Evaluate the objectives with RP through time
# MinmETL.RND.t <- optimize.portfolio.rebalancing(R=R,
#                                                 portfolio=MinmETL.portf,
#                                                 optimize_method="random",
#                                                 trace=TRUE,
#                                                 rp=rp,
#                                                 rebalance_on=rebalance_period,
#                                                 training_period=36)
# MinmETL.w = extractWeights.rebal(MinmETL.RND.t)
# MinmETL=Return.rebalancing(edhec.R, MinmETL.w)
# colnames(MinmETL) = "MinmETL"
# save(MinmETL.RND, MinmETL.RND.t, MinmETL.w, MinmETL,file=paste('MinmETL',Sys.Date(),'rda',sep='.'))

print(paste('Completed MinmETL optimization at',Sys.time(),'moving on to EqmETL'))

##### script.workshop2012.R: Evaluate BUOY 6 #####
### Evaluate BUOY 6: Constrained Equal mETL Contribution Portfolio
# EqmETL.RND<-optimize.portfolio_v1(R=R,
#                                   constraints=EqmETL.constr,
#                                   optimize_method='random',
#                                   search_size=1000, trace=TRUE, verbose=TRUE,
#                                   rp=rp) # use the same random portfolios generated above
# EqmETL.RND.t = optimize.portfolio.rebalancing_v1(R=R,
#                                                  constraints=EqmETL.constr, 
#                                                  optimize_method='random', 
#                                                  search_size=permutations, trace=TRUE, verbose=TRUE, 
#                                                  rp=rp, # all the same as prior
#                                                  rebalance_on=rebalance_period, # uses xts 'endpoints'
#                                                  trailing_periods=NULL, # calculates from inception
#                                                  training_period=36) # starts 3 years in to the data history
# EqmETL.w = extractWeights.rebal(EqmETL.RND.t)
# EqmETL=Return.rebalancing(edhec.R, EqmETL.w)
# colnames(EqmETL) = "EqmETL"

##### v2: Evaluate BUOY 6 #####
EqmETL.RND <- optimize.portfolio(R=R,
                                 portfolio=EqmETL.portf,
                                 optimize_method="random",
                                 trace=TRUE,
                                 rp=rp)
print(EqmETL.RND)
print(EqmETL.RND$elapsed_time)

# Evaluate the objectives with RP through time
# EqmETL.RND.t <- optimize.portfolio.rebalancing(R=R,
#                                                portfolio=EqmETL.portf,
#                                                optimize_method="random",
#                                                trace=TRUE,
#                                                rp=rp,
#                                                rebalance_on=rebalance_period,
#                                                training_period=36)
# EqmETL.w = extractWeights.rebal(EqmETL.RND.t)
# EqmETL=Return.rebalancing(edhec.R, EqmETL.w)
# colnames(EqmETL) = "EqmETL"
# save(EqmETL.RND, EqmETL.RND.t, EqmETL.w, EqmETL, file=paste('EqmETL',Sys.Date(),'rda',sep='.'))

end_time<-Sys.time()
print("Optimization Complete")
print(end_time-start_time)
