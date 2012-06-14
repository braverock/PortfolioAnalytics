## logLinking.r
##
## purpose: Aggregates performance attribution effects over time to produce a multiperiod summary.
##
## authors: Eric Zivot
## created: June 5, 2012




#' Logarithmic Linking
#' 
#' Aggregates performance attribution effects over time to produce a
#' multiperiod summary.
#' 
#' Transforms to a multiperiod arithmetic decomposition by distributing the
#' residual proportionately. Creates linking coefficients \code{k_t / k} and
#' applies to given \code{component} to combine arithmetic attribution effects
#' over time. If \code{weighted.benchmark.returns} is zero for each \code{p}
#' date, then the function returns absolute arithmetic attribution effect,
#' e.g., factor returns or idiosyncratic returns from factor model.
#' 
#' @param weighted.portfolio.returns \code{n x p} data frame containing asset
#' ID (as rownames) and weighted portfolio returns for \code{p} dates
#' @param weighted.benchmark.returns \code{m x p} containing benchmark ID (as
#' rownames) and weighted benchmark returns for \code{p} dates
#' @param component data frame containing ID and attribution component for
#' \code{p} dates (e.g., \code{n x p} for manager selection or \code{m x p}
#' strategy allocation)
#' @return Returns a list of two objects of class "\code{numeric}" after
#' applying the linking coefficient from the logarithmic method: \item{linked}{
#' combined \code{component} over time } \item{linked.total}{ sum of combined
#' \code{component} over time }
#' @author Eric Zivot
#' @seealso \code{\link{relativeAttribution}}
#' @references Christopherson, J., Carino, D., and Ferson, W. (2009)
#' \emph{Portfolio Performance Measurement and Benchmarking}, McGrall-Hill.
logLinking <- function(weighted.portfolio.returns, weighted.benchmark.returns, component){
## inputs
## weighted.portfolio.returns	n x p data frame containing asset ID (as rownames) and weighted portfolio returns for p dates
## weighted.benchmark.returns	m x p data frame containing benchmark ID (as rownames) and weighted benchmark returns for p dates
## component data frame containing ID and attribution component for p dates (e.g., manager selection or strategy allocation)
## outputs
## list of combined components over time after applying the linking coefficient from the logarithmic method

require(PerformanceAnalytics)

logCoeff = data.frame((log(1+colSums(weighted.portfolio.returns))
      - log(1+colSums(weighted.benchmark.returns)))
      /(colSums(weighted.portfolio.returns) - colSums(weighted.benchmark.returns)),
      (log(1+Return.cumulative(colSums(weighted.portfolio.returns)))
      - log(1+Return.cumulative(colSums(weighted.benchmark.returns))))
      /(Return.cumulative(colSums(weighted.portfolio.returns)) - Return.cumulative(colSums(weighted.benchmark.returns))))
colnames(logCoeff) = c("k_t","K")
logCoeff = data.frame(logCoeff, logCoeff$k_t/logCoeff$K)
colnames(logCoeff) = c("k_t","K","B")

linked = data.frame(component)
for(i in 1:nrow(logCoeff)){
      linked[,i] = as.matrix(component[,i])%*%logCoeff$B[i]
}

ans = list("linked" = rowSums(linked),
            "linked.total" = sum(rowSums(linked)))
return(ans)
}
