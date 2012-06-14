## relativeAttribution.r
##
## purpose: Decompose relative portfolio returns into manager selection and
##          strategy allocation components incorporating factor contributions
##
## authors: Eric Zivot
## created: June 5, 2012
##


#' Relative Performance Attribution
#' 
#' Decompose relative returns into manager selection and strategy allocation
#' components, and then into factor and idiosyncratic return contributions.
#' 
#' The Brinson model reflects a sector-based investment process and attributes
#' portfolio return to manager selection and strategy allocation effects.
#' Positive contributions are earned by overweighting outperforming
#' managers/sectors and underweighting underperformers. In addition, the
#' manager selection and strategy allocation effects are decomposed into factor
#' and idiosyncratic return contributions as defined by a factor model.
#' 
#' @param portfolio.df \code{n x (3 + f)} data frame containing asset ID (as
#' rownames), Strategy \code{n x 1}, Weight \code{n x 1}, Return \code{n x 1},
#' exposure-weighted factor returns \code{n x f} whose column names are the
#' factor IDs
#' @param benchmark.df \code{m x (3 + f)} data frame containing benchmark ID
#' (as rownames), Strategy \code{n x 1}, Weights \code{n x 1}, Returns \code{n
#' x 1}, exposure-weighted factor returns \code{n x f} whose column names are
#' factor IDs.
#' @return Returns a list with the following components: \item{portfolio.ret}{
#' \code{1 x 1} matrix containing portfolio return} \item{benchmark.ret}{
#' \code{1 x 1} matrix containing benchmark return} \item{relative.ret}{
#' \code{1 x 1} matrix containing relative return} \item{manager}{ \code{n x 1}
#' data frame containing asset ID (as rownames) and \code{ManagerSelection} }
#' \item{manager.total}{ numeric value of total \code{ManagerSelection}
#' component} \item{manager.factor}{ \code{n x (1 + f)} data frame containing
#' asset ID (as rownames) and ManagerSelection by \code{Idiosyncratic} \code{n
#' x 1} and factor IDs \code{n x f}} \item{manager.factor.total}{ \code{n x 1}
#' numeric object with asset ID (as rownames) and ManagerSelection component
#' summed over all factor IDs} \item{manager.idiosyncratic.total}{ numeric
#' value of ManagerSelection component summed over all Idiosyncratic values}
#' \item{strategy}{ \code{n x 2} data frame containing benchmark ID (as
#' rownames), \code{Strategy}, and \code{StrategyAllocation} }
#' \item{strategy.total}{ numeric value of total StrategyAllocation component}
#' \item{strategy.factor}{ \code{m x (1 + f)} data frame containing benchmark
#' ID (as rownames) and StrategyAllocation by \code{Idiosyncratic} \code{m x 1}
#' and factor IDs \code{m x f}} \item{strategy.factor.total}{ \code{m x 1}
#' numeric object with benchmark ID (as rownames) and StrategyAllocation
#' component summed over all factor IDs} \item{strategy.idiosyncratic.total}{
#' numeric value of StrategyAllocation component summed over all Idiosyncratic
#' values}
#' @author Eric Zivot
#' @seealso \code{\link{logLinking}}
#' @references Davis, B. and Menchero, J. (2009) \emph{Beyond Brinson:
#' Establishing the Link Between Sector and Factor Models}, MSCI Barra Research
#' Insights.
relativeAttribution <- function(portfolio.df, benchmark.df){
## 
## inputs:
## portfolio.df	n x (3 + f) data frame containing asset ID (as rownames),
##                          Strategy (n x 1), Weight (n x 1), Return (n x 1),
##                          exposure-weighted factor returns (n x f) whose column
##                          names are the factor IDs
## benchmark.df	m x (3 + f) data frame containing benchmark ID (as rownames),
##                          Strategy (n x 1), Weights (n x 1), Return (n x 1),
##                          exposure-weighted factor returns (n x f) whose column
##                          names are factor IDs.
## 
## outputs:
## list with the following components:
##  
## portfolio.ret, matrix containing portfolio return
## benchmark.ret, matrix containing benchmark return
## relative.ret, matrix containing relative return
## manager, data frame containing class ID (as rownames) and ManagerSelection
## manager.total, numeric value of total ManagerSelection component
## manager.factor, data frame containing class ID (as rownames) and ManagerSelection by Idiosyncratic and factor IDs
## manager.factor.total, numeric object with class ID (as rownames) and ManagerSelection component summed over all factor IDs
## manager.idiosyncratic.total, numeric value of ManagerSelection component summed over all Idiosyncratic values  
## strategy, data frame containing benchmark ID (as rownames), Strategy, and StrategyAllocation
## strategy.total, numeric value of total StrategyAllocation component
## strategy.factor, data frame containing benchmark ID (as rownames) and StrategyAllocation by Idiosyncratic and factor IDs
## strategy.factor.total, numeric object with benchmark ID (as rownames) and StrategyAllocation component summed over all factors IDs
## strategy.idiosyncratic.total, numeric value of StrategyAllocation component summed over all Idiosyncratic values
##
require(plyr)

## Compute benchmark, portfolio, and returns
portfolio.ret = crossprod(portfolio.df$Weight, portfolio.df$Return)
benchmark.ret = crossprod(benchmark.df$Weight, benchmark.df$Return)
relative.ret = portfolio.ret - benchmark.ret

## x is portfolio.df, y is benchmark.df
colnames(benchmark.df) = paste(colnames(benchmark.df), ".y", sep="")
colnames(benchmark.df)[1] = "Strategy"
temp = join(portfolio.df,  benchmark.df, by = "Strategy")

## Active returns by manager selection
manager = data.frame(temp$Weight * (temp$Return  - temp$Return.y), row.names = rownames(temp))
colnames(manager) = c("ManagerSelection")

## Store factor names
factor.names = colnames(portfolio.df[4:ncol(portfolio.df)])
factor.names.y = paste(factor.names, ".y", sep="")

## Manager selection by factor and idiosyncratic returns
manager.factor = data.frame((temp$Return-rowSums(temp[,factor.names]) - (temp$Return.y-rowSums(temp[,factor.names.y])))*temp$Weight,
                    (temp[,factor.names] - temp[,factor.names.y]) * temp$Weight)
colnames(manager.factor) = c("Idiosyncratic", factor.names)

## Active returns by strategy allocation
strategy = data.frame(benchmark.df$Strategy, row.names = rownames(benchmark.df))
colnames(strategy) = c("Strategy")
for(i in 1:nrow(strategy)){
      strategy$ActiveWeight[i] = sum(portfolio.df$Weight[portfolio.df$Strategy == strategy$Strategy[i]]) - benchmark.df$Weight[i]
}
strategy = data.frame(strategy, strategy$ActiveWeight * (benchmark.df$Return - benchmark.df$Weight%*%benchmark.df$Return))

## Strategy allocation by factor and idiosyncratic returns
tmp.mat = matrix(0, nrow(strategy), ncol(benchmark.df)-1)
colnames(tmp.mat) = c("Strategy", "Idiosyncratic", factor.names)
rownames(tmp.mat) = rownames(benchmark.df)
strategy.factor = as.data.frame(tmp.mat)
strategy.factor$Strategy = benchmark.df$Strategy
strategy.factor$Idiosyncratic = (benchmark.df$Return - rowSums(benchmark.df[,factor.names.y]) - sum((benchmark.df$Return - rowSums(benchmark.df[,factor.names.y])) * benchmark.df$Weight)) * strategy$ActiveWeight
for(i in 1:nrow(strategy)){
      strategy.factor[i,factor.names] = colSums(benchmark.df[,factor.names.y] * benchmark.df$Weight)
}
strategy.factor[,factor.names] = (benchmark.df[,factor.names.y] - strategy.factor[,factor.names]) * strategy$ActiveWeight
colnames(strategy) = c("Strategy", "ActiveWeight", "StrategyAllocation")

ans = list("manager" = manager, 
           "strategy" = strategy[,c("Strategy","StrategyAllocation")],
           "strategy.active.weight" = strategy[,c("Strategy","ActiveWeight")],
           "manager.factor" = manager.factor,
           "strategy.factor" = strategy.factor,
           "manager.factor.total" = rowSums(manager.factor[,factor.names]),
           "manager.idiosyncratic.total" = sum(manager.factor$Idiosyncratic),
           "strategy.factor.total" = rowSums(strategy.factor[,factor.names]),
           "strategy.idiosyncratic.total" = sum(strategy.factor$Idiosyncratic),
           "manager.total" = sum(manager$ManagerSelection),
           "strategy.total" = sum(strategy$StrategyAllocation),
           "portfolio.ret" = portfolio.ret,
           "benchmark.ret" = benchmark.ret,
           "relative.ret" = relative.ret)
return(ans)
}
