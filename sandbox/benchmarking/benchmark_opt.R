
# The purpose of this script is to set a baseline for performance of optimize.portfolio

library(PortfolioAnalytics)
library(microbenchmark)

data(edhec)
returns <- edhec[,1:10]
funds <- colnames(returns)

# Add basic constraints and objectives
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(portfolio=init.portf, type="box", min=0, max=1)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES")

# Generate N random portfolios. Random portfolios should be generated outside
# of optimize.portfolio so that the time to generate random portfolios is not
# included in the timing
n_portfolios <- 5000
rp <- random_portfolios(portfolio=init.portf, 
                        permutations=n_portfolios, 
                        rp_method="sample", 
                        eliminate=FALSE)

opt_rp <- function(){
  optimize.portfolio(R=returns,
                     portfolio=init.portf,
                     optimize_method="random",
                     rp=rp, 
                     trace=TRUE)
}

opt_de <- function(){
  optimize.portfolio(R=returns,
                     portfolio=init.portf,
                     optimize_method="DEoptim",
                     search_size=n_portfolios,
                     rp=rp,
                     traceDE=0,
                     trace=TRUE)
}

opt_benchmark <- microbenchmark(opt_rp(), opt_de(), times=10)
comment_string <- "ES optimization benchmark with random portfolios and DEoptim"

zz <- file(description="sandbox/benchmarking/benchmark_output.txt", open="at")
sink(zz, append=TRUE)
cat("******\n")
Sys.time()
cat(comment_string, "\n")
opt_benchmark
cat("******\n")
sink()
close(zz)

# Rprof runs
# Rprof(filename="rp_profile_reuse.txt")
# optimize.portfolio(R=returns, portfolio=init.portf, optimize_method="random", rp=rp, trace=TRUE)
# Rprof(NULL)

# out_reuse <- summaryRprof("rp_profile_reuse.txt")
# out_no_reuse <- summaryRprof("rp_profile_no_reuse.txt")

