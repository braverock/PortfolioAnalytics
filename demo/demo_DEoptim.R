#' ---
#' title: "Differential Evolution Optimization Demo"
#' date: "7/17/2014"
#' ---

#' This script demonstrates several optimization problems using Differential
#' Evolution as the optimization engine. This script is based heavily on
#' http://www.rinfinance.com/agenda/2012/workshop/Carl+Peterson.pdf.

#' The following optimization problems will be run
#' * mean-mETL: maximize mean-to-ETL (i.e. reward-to-risk)
#' * MinSD: minimize annualized standard deviation
#' * eqStdDev: equal risk (volatility)
#' * MeanRL: maximize mean with mETL risk limits

#' Include optimizer and multi-core packages
library(PortfolioAnalytics)
require(DEoptim)
require(foreach)

#' The multicore package, and therefore registerDoMC, should not be used in a
#' GUI environment, because multiple processes then share the same GUI. Only use
#' when running from the command line.
# require(doMC)
# registerDoMC(3)

#' Load the data
data(edhec)
edhec.R <- edhec[,c("Convertible Arbitrage", "Equity Market Neutral", 
                    "Fixed Income Arbitrage", "Event Driven", "CTA Global", 
                    "Global Macro", "Long/Short Equity")]

#' Define function to compute annualized standard deviation
pasd <- function(R, weights){
  as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
  # as.numeric(StdDev(R=R, weights=weights)*sqrt(4)) # hardcoded for quarterly data
}

#' Set some parameters
rebalance_period = 'quarters' # uses endpoints identifiers from xts
clean = "none" #"boudt"
permutations = 4000

#' Create initial portfolio object used to initialize ALL the bouy portfolios
init.portf <- portfolio.spec(assets=colnames(edhec.R), 
                             weight_seq=generatesequence(by=0.005))
#' Add leverage constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="leverage", 
                             min_sum=0.99, 
                             max_sum=1.01)
#' Add box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0.05, 
                             max=0.3)

#' Add measure 1, mean return
init.portf <- add.objective(portfolio=init.portf,
                            type="return", # the kind of objective this is
                            name="mean", # name of the function
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

#' Add measure 2, annualized standard deviation
init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="pasd", # to minimize from the sample
                            enabled=TRUE, # enable or disable the objective
                            multiplier=0 # calculate it but don't use it in the objective
)

#' Add measure 3, ES with confidence level p=(1-1/12)
p <- 1-1/12 # for monthly

init.portf <- add.objective(portfolio=init.portf,
                            type="risk", # the kind of objective this is
                            name="ES", # the function to minimize
                            enabled=FALSE, # enable or disable the objective
                            multiplier=0, # calculate it but don't use it in the objective
                            arguments=list(p=p)
)

#' Set up portfolio for Mean-mETL
MeanmETL.portf <- init.portf
MeanmETL.portf$objectives[[1]]$multiplier=-1 # mean
MeanmETL.portf$objectives[[3]]$enabled=TRUE # mETL
MeanmETL.portf$objectives[[3]]$multiplier=1 # mETL

#' Set up portfolio for min pasd
MinSD.portf <- init.portf
MinSD.portf$objectives[[2]]$multiplier=1

#' Set up portfolio for eqStdDev
EqSD.portf <- add.objective(portfolio=init.portf,
                                type="risk_budget",
                                name="StdDev",
                                min_concentration=TRUE,
                                arguments = list(p=(1-1/12)))
#' Without a sub-objective, we get a somewhat undefined result, since 
#' there are (potentially) many Equal SD contribution portfolios.
EqSD.portf$objectives[[2]]$multiplier=1 # min pasd

#' Set up portfolio to maximize mean with mETL risk limit
MeanRL.portf <- add.objective(portfolio=init.portf, 
                              type='risk_budget', 
                              name="ES", 
                              min_prisk=-Inf, 
                              max_prisk=0.4, 
                              arguments=list(method="modified", p=p))
MeanRL.portf$objectives[[1]]$multiplier=-1 # mean
#' Change box constraints max to vector of 1s
MeanRL.portf$constraints[[2]]$max=rep(1, 7)

#' Set the 'R' variable
R <- edhec.R

#' Start the optimizations
start_time<-Sys.time()
print(paste('Starting optimization at',Sys.time()))

#' Run the optimization
##### mean-mETL #####
MeanmETL.DE <- optimize.portfolio(R=R,
                                  portfolio=MeanmETL.portf,
                                  optimize_method="DEoptim",
                                  trace=TRUE,
                                  search_size=2000,
                                  traceDE=5)
print(MeanmETL.DE)
print(MeanmETL.DE$elapsed_time)
chart.Weights(object=MeanmETL.DE, main="Mean-mETL Weights")
chart.RiskReward(object=MeanmETL.DE, return.col="mean", risk.col="ES")
# save(MeanmETL.DE, file=paste('MeanmETL',Sys.Date(),'rda',sep='.'))

# Evaluate the objectives with DE through time
# MeanmETL.DE.t <- optimize.portfolio.rebalancing(R=R,
#                                                 portfolio=MeanSD.portf,
#                                                 optimize_method="random",
#                                                 trace=TRUE,
#                                                 search_size=2000,
#                                                 rebalance_on=rebalance_period,
#                                                 training_period=36)
# MeanmETL.w = extractWeights.rebal(MeanmETL.DE.t)
# MeanmETL=Return.rebalancing(edhec.R, MeanmETL)
# colnames(MeanmETL) = "MeanmETL"
# save(MeanmETL.DE, MeanmETL.DE.t, MeanmETL.w, MeanmETL, file=paste('MeanmETL',Sys.Date(),'rda',sep='.'))

print(paste('Completed MeanmETL optimization at',Sys.time(),'moving on to MinSD'))

#' Run the optimization
##### min pasd #####
MinSD.DE <- optimize.portfolio(R=R,
                               portfolio=MinSD.portf,
                               optimize_method="DEoptim",
                               trace=TRUE,
                               search_size=2000,
                               traceDE=5)
print(MinSD.DE)
print(MinSD.DE$elapsed_time)
chart.Weights(object=MinSD.DE, plot.type="barplot", legend.loc=NULL)
chart.RiskReward(object=MinSD.DE, return.col="mean", risk.col="pasd")
# save(MinSD.DE, file=paste('MinSD',Sys.Date(),'rda',sep='.'))

print(paste('Completed MinSD optimization at',Sys.time(),'moving on to EqSD'))

#' Run the optimization
##### EqSD #####
EqSD.DE <- optimize.portfolio(R=R,
                              portfolio=EqSD.portf,
                              optimize_method="DEoptim",
                              trace=TRUE,
                              search_size=2000,
                              traceDE=5)
print(EqSD.DE)
print(EqSD.DE$elapsed_time)
# save(EqSD.DE, file=paste('EqSD',Sys.Date(),'rda',sep='.'))

chart.Weights(object=EqSD.DE)
chart.RiskReward(object=EqSD.DE, return.col="mean", risk.col="StdDev")
chart.RiskBudget(object=EqSD.DE, risk.type="absolute")
chart.RiskBudget(object=EqSD.DE, risk.type="pct_contrib")

print(paste('Completed EqSD optimization at',Sys.time(),'moving on to MeanRL'))

#' Run the optimization
##### MeanRL.DE #####
MeanRL.DE <- optimize.portfolio(R=R,
                              portfolio=MeanRL.portf,
                              optimize_method="DEoptim",
                              trace=TRUE,
                              search_size=2000,
                              traceDE=5)
print(MeanRL.DE)
print(MeanRL.DE$elapsed_time)
# save(MeanRL.DE, file=paste('MeanRL',Sys.Date(),'rda',sep='.'))

chart.Weights(object=MeanRL.DE)
chart.RiskBudget(object=MeanRL.DE, risk.type="pct_contrib", neighbors=25)

end_time<-Sys.time()
print("Optimization Complete")
print(end_time-start_time)
