# Demonstrate examples from script.workshop2012.R using the v2 specification

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

# Implement BUOY 1, BUOY 4, and BUOY 6 from script.workshop2012.R

# Note: The script.workshop2012.R examples use pamean and pasd, I will simply
# use mean and StdDev.

# Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(quantmod)
require(foreach)

# The multicore package, and therefore registerDoMC, should not be used in a
# GUI environment, because multiple processes then share the same GUI. Only use
# when running from the command line
# require(doMC)
# registerDoMC(3)

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
print(init.portf)
summary(init.portf)

##### v2: BUOY 1 #####
### Construct BUOY 1: Constrained Mean-StdDev Portfolio
MeanSD.portf <- init.portf
# Turn back on the return and sd objectives
MeanSD.portf$objectives[[1]]$multiplier = -1 # pamean
MeanSD.portf$objectives[[2]]$multiplier = 1 # pasd
print(MeanSD.portf)
summary(MeanSD.portf)


##### v2: BUOY 4 #####
### Construct BUOY 4: Constrained Minimum mETL Portfolio
MinmETL.portf <- init.portf
# Turn back on the mETL objective
MinmETL.portf$objectives[[3]]$multiplier = 1 # mETL
MinmETL.portf$objectives[[3]]$enabled = TRUE # mETL
print(MinmETL.portf)
summary(MinmETL.portf)

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
print(EqmETL.portf)
summary(EqmETL.portf)

### Choose our 'R' variable
R=edhec.R # for monthlies

# Generate a single set of random portfolios to evaluate against all constraint set
print(paste('constructing random portfolios at',Sys.time()))
rp = random_portfolios(portfolio=init.portf, permutations=permutations)
print(paste('done constructing random portfolios at',Sys.time()))

start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))

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

print(paste('Completed meanSD optimization at',Sys.time(),'moving on to MinmETL'))

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

print(paste('Completed MinmETL optimization at',Sys.time(),'moving on to EqmETL'))

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

end_time<-Sys.time()
print("Optimization Complete")
print(end_time-start_time)
