### Load the necessary packages
library(PortfolioAnalytics)

data(edhec)

# Drop some indexes and reorder
edhec.R = edhec[,c("Convertible Arbitrage", "Equity Market Neutral","Fixed Income Arbitrage", "Event Driven", "CTA Global", "Global Macro", "Long/Short Equity")]

# Define pamean function
pamean <- function(n=12, R, weights, geometric=TRUE){
  as.vector(sum(Return.annualized(last(R,n), geometric=geometric)*weights))
}

# Define pasd function
pasd <- function(R, weights){
  as.numeric(StdDev(R=R, weights=weights)*sqrt(12)) # hardcoded for monthly data
}

# Create initial portfolio object used to initialize ALL the bouy portfolios
init.portf <- portfolio.spec(assets=colnames(edhec.R), weight_seq=generatesequence(by=0.005))
# Add leverage constraint
init.portf <- add.constraint(portfolio=init.portf, type="leverage", min_sum=0.99, max_sum=1.01)
# Add box constraint
init.portf <- add.constraint(portfolio=init.portf, type="box", min=0.05, max=0.3)


mean.portf <- add.objective(portfolio=init.portf,
                            type="return", # the kind of objective this is
                            name="mean", # name of the function
                            enabled=TRUE, # enable or disable the objective
                            multiplier=-1 # calculate it but don't use it in the objective
)

pamean.portf <- add.objective(portfolio=init.portf,
                            type="return", # the kind of objective this is
                            name="pamean", # name of the function
                            enabled=TRUE, # enable or disable the objective
                            multiplier=-1, # calculate it but don't use it in the objective
                            arguments=list(n=60)
)

pasd.portf <- add.objective(portfolio=init.portf,
                              type="risk", # the kind of objective this is
                              name="pasd", # name of the function
                              enabled=TRUE, # enable or disable the objective
                              multiplier=1 # calculate it but don't use it in the objective
)

permutations = 500
rp = random_portfolios(portfolio=init.portf, permutations=permutations)

# Takes about 1.4 seconds
mean_time <- system.time({ opt_mean <- optimize.portfolio(R=edhec.R, portfolio=mean.portf, optimize_method="random", rp=rp) })

# Takes nearly 71 seconds!!!!!
pamean_time <- system.time({ opt_pamean <- optimize.portfolio(R=edhec.R, portfolio=pamean.portf, optimize_method="random", rp=rp) })

pamean_time / mean_time

# takes about 1.5 seconds
pasd_time <- system.time({ opt_pasd <- optimize.portfolio(R=edhec.R, portfolio=pasd.portf, optimize_method="random", rp=rp) })
pasd_time
