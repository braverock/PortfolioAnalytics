## relativeAttributionWithoutFactors.r
##
## purpose: Decompose relative portfolio returns into manager selection and
##          strategy allocation components
##
## authors: Eric Zivot
## created: June 5, 2012
##


#' Relative Performance Attribution
#' 
#' Decompose relative returns into manager selection and strategy allocation
#' components.
#' 
#' The Brinson model reflects a sector-based investment process and attributes
#' portfolio return to manager selection and strategy allocation effects.
#' Positive contributions are earned by overweighting outperforming
#' managers/sectors and underweighting underperformers.
#' 
#' @param portfolio.df \code{n x 3} data frame containing asset ID (as
#' rownames), Strategy \code{n x 1}, Weight \code{n x 1}, Return \code{n x 1},
#' @param benchmark.df \code{m x 3} data frame containing benchmark ID (as
#' rownames), Strategy \code{n x 1}, Weights \code{n x 1}, Returns \code{n x
#' 1},
#' @return Returns a list with the following components: \item{portfolio.ret}{
#' \code{1 x 1} matrix containing portfolio return} \item{benchmark.ret}{
#' \code{1 x 1} matrix containing benchmark return} \item{relative.ret}{
#' \code{1 x 1} matrix containing relative return} \item{manager}{ \code{n x 1}
#' data frame containing asset ID (as rownames) and \code{ManagerSelection} }
#' \item{manager.total}{ numeric value of total \code{ManagerSelection}
#' component} \item{strategy}{ \code{n x 2} data frame containing benchmark ID
#' (as rownames), \code{Strategy}, and \code{StrategyAllocation} }
#' \item{strategy.total}{ numeric value of total StrategyAllocation component}
#' @author Eric Zivot
#' @seealso \code{\link{logLinking}}
#' @references Davis, B. and Menchero, J. (2009) \emph{Beyond Brinson:
#' Establishing the Link Between Sector and Factor Models}, MSCI Barra Research
#' Insights.
#' 
relativeAttributionWithoutFactors <- function(portfolio.df, benchmark.df){
## 
## inputs:
## portfolio.df	n x 3 data frame containing asset ID (as rownames), strategy, weights, returns
## benchmark.df	m x 3 data frame containing benchmark ID (as rownames), strategy, weights, returns
## 
## outputs:
## list with the following components:
##  manager, dataframe giving active returns by manager selection
##  strategy, dataframe giving active returns by strategy allocation
##
require(plyr)

## x is portfolio.df, y is benchmark.df
temp = join(portfolio.df, benchmark.df, by = "Strategy")
colnames(temp) = c("Strategy", "Weight.x", "Return.x", "Weight.y", "Return.y")

## Active returns by manager selection
manager = data.frame(temp$Weight.x * (temp$Return.x  - temp$Return.y), row.names = rownames(temp))
colnames(manager) = c("ManagerSelection")

## Active returns by strategy allocation
strategy = data.frame(benchmark.df$Strategy, row.names = rownames(benchmark.df))
colnames(strategy) = c("Strategy")
for(i in 1:nrow(strategy)){
      strategy$ActiveWeight[i] = sum(portfolio.df$Weight[portfolio.df$Strategy == strategy$Strategy[i]]) - benchmark.df$Weight[i]
}
strategy = data.frame(strategy, strategy$ActiveWeight * (benchmark.df$Return - benchmark.df$Weight%*%benchmark.df$Return))
strategy$ActiveWeight = NULL
colnames(strategy) = c("Strategy", "StrategyAllocation")

# compute benchmark, portfolio and returns
portfolio.ret = crossprod(portfolio.df$Weight, portfolio.df$Return)
benchmark.ret = crossprod(benchmark.df$Weight, benchmark.df$Return)
relative.ret = portfolio.ret - benchmark.ret

ans = list("manager" = manager, 
           "strategy" = strategy,
           "manager.total" = sum(manager$ManagerSelection),
           "strategy.total" = sum(strategy$StrategyAllocation),
           "portfolio.ret" = portfolio.ret,
           "benchmark.ret" = benchmark.ret,
           "relative.ret" = relative.ret)
return(ans)
}
