
setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Optimized portfolio you want to analyse out-of-sample performance through (Component) Sharpe ratios

datacase = "equitybondscommodity" # "intequitybonds" or "equitybondscommodity"
 
estyears = 4;
percriskcontribcriterion = "mES"
RBcon = 0.4# 
frequency = "quarterly" ;yearly = F;
alpha = 0.05; # probability level for which CVaR is computed

# Load additional programs to interpret the data

library(zoo); library("PerformanceAnalytics"); source("R_interpretation/pfolioreturn.R"); 

# Choose data
datacase = "equitybondscommodity" # "equitybondscommodity" or "equitybondscommodity"

# Define optimization criteria

optimcrit = "mES." 
mainname =  paste("weights/" , percriskcontribcriterion,"/", datacase , "/" , frequency , "/" , sep="")
criterion = paste(mainname,optimcrit,estyears,"yr",sep="")
criteria = c( paste(mainname,"EW",sep="")  , 
              paste(mainname,"unconstrained/", optimcrit , "4yr-InfInf" , sep="")  , 
              paste(  criterion , "-InfInf" , sep="") , paste( criterion , "-Inf" , 0.4 , sep="")  , 
              paste( mainname,"unconstrained/", optimcrit , "4yr0.220.28" , sep="") ); 

source("R_Allocation/allocation_functions_monthly.R"); source("R_Allocation/estimators.R"); 
library(zoo);  library("PerformanceAnalytics"); 
memory.limit(2048)

#############################################################################
#   Load the data
#############################################################################

firstyear = 1976 ; firstquarter = 1; lastyear = 2009; lastquarter = 2; 

data = read.table( file= paste("data/",datacase,"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
monthlyR = zoo( data[,2:ncol(data)] , order.by = date )

# "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 

cAssets = 4; 
head(monthlyR); 

apply( monthlyR , 2 , 'mean' )*100
apply( monthlyR , 2 , 'sd' )*100
apply( monthlyR , 2 , 'skew' )
apply( monthlyR , 2 , 'exkur' )
cor(monthlyR)
monthlyRTBill = monthlyR[,cAssets+1]

# Define the out-of-sample periods 

from=to=c(); 
if( frequency == "yearly" ){
   for( year in (firstyear):(lastyear) ){ 
      from = c( from , paste( year,"-01-01",sep="" ) ); to  = c( to  ,  paste( (year),"-12-31",sep="" ) ) }
}else{
   for( year in (firstyear+estyears):(lastyear) ){ 
      from = c( from , paste( year,"-01-01",sep="" ) , paste( year,"-04-01",sep="" ) , paste( year,"-07-01",sep="" ) , paste( year,"-10-01",sep="" ) );
      to   = c( to , paste( year,"-03-31",sep="" ) , paste( year,"-06-30",sep="" ) , paste( year,"-09-30",sep="" ) , paste( year,"-12-31",sep="" ) )}
   from = from[firstquarter:(length(from)-4+lastquarter)]; to = to[firstquarter:(length(to)-4+lastquarter)] 
}

# Compute daily out of sample returns, accounting for compounding effects

Returns.rebalancing( R = monthlyR , criteria = criteria, from = from, to = to , folder=paste("/oosreturns/",datacase,"/", frequency ,"/", sep="") ) 
oosdates = time( window (monthlyR , start = from[1] , end = to[ length(to) ] ) ) 

load(paste(getwd(),"/","/oosreturns/",datacase,"/", frequency , "/simplereturns.Rdata" ,sep="") ) 

colnames(simplereturns) = c( "EW" , "UnconstrainedMinCVaR", "MaxWeightConstrained" , "MaxRiskbudgetconstrained" , "EqualRisk" )
date = time(simplereturns)

# Assess out of sample performance of the minimum StdDev portfolio through the rolling centered 3-year rolling standard deviation

sd_EW                   = rollapply(data=simplereturns[,1],width=36, FUN=sd)
sd_MinCVaR              = rollapply(data=simplereturns[,2],width=36, FUN=sd)
sd_weightconstrained    = rollapply(data=simplereturns[,3],width=36, FUN=sd)
sd_RCconstrained        = rollapply(data=simplereturns[,4],width=36, FUN=sd)
sd_equalrisk            = rollapply(data=simplereturns[,5],width=36, FUN=sd)

# Assess out of sample performance of the maximum Sharpe ratio through relative cumulative performance chart

cumperf_EW                   = cumprod(1 + na.omit(simplereturns[,1]))
cumperf_MinCVaR              = cumprod(1 + na.omit(simplereturns[,2]))
cumperf_weightconstrained    = cumprod(1 + na.omit(simplereturns[,3]))
cumperf_RCconstrained        = cumprod(1 + na.omit(simplereturns[,4]))
cumperf_equalrisk            = cumprod(1 + na.omit(simplereturns[,5]))


postscript( file=paste("fig/",percriskcontribcriterion,"/",datacase,"/",frequency,"/relativeperformance.eps",sep="") )

   par(mfrow=c(4,1), mar = c(2,2,3,2) , cex.main = 1.1 , cex.axis=0.8); 

   plot( sd_RCconstrained /sd_EW , 
   main="Relative rolling 3 month standard deviation w.r.t. equal-weighted",   type = "l" )

   plot( sd_RCconstrained /sd_MinCVaR , 
   main="Relative rolling 3 month standard deviation w.r.t. min CVaR",   type = "l" )

   plot( sd_RCconstrained /sd_weightconstrained , 
   main="Relative rolling 3 month standard deviation w.r.t. max 40 % weight constrained",   type = "l" )

   plot( sd_RCconstrained /sd_equalrisk , 
   main="Relative rolling 3 month standard deviation w.r.t. equal-risk",   type = "l" )


   par(mfrow=c(4,1), mar = c(2,2,3,2) , cex.main = 1.1 , cex.axis=0.8); 

   plot( cumperf_equalrisk /cumperf_EW , main="Relative cumulative performance w.r.t. equal-weighted",  type = "l" )

   plot( cumperf_equalrisk /cumperf_MinCVaR , main="Relative cumulative performance w.r.t. unconstrained Min CVaR",  type = "l" )

   plot( cumperf_equalrisk  /cumperf_weightconstrained , main="Relative cumulative performance w.r.t. max 40 % weight constrained",  type = "l" )

   plot( cumperf_equalrisk  /cumperf_RCconstrained  , main="Relative cumulative performance w.r.t. equal-risk",  type = "l" )


dev.off()

library(PerformanceAnalytics) 
library(zoo)
oosreturns = zoo(simplereturns[,1:5],order.by = seq.Date(as.Date(from[1])+31, as.Date(tail(to,1)) + 1, by ="month") - 1)
charts.PerformanceSummary(oosreturns)
table.Drawdowns(oosreturns[,1],top=10)
table.Drawdowns(oosreturns[,2],top=10)
table.Drawdowns(oosreturns[,3],top=10)
table.Drawdowns(oosreturns[,4],top=10)
table.Drawdowns(oosreturns[,5],top=10)

# Portfolio turnover per strategy:

# compute endweights:
(1+simplereturns)/rowSums(simplereturns)

# frequency of rebalancing: yearly of quarterly
estyears = 4;
percriskcontribcriterion = "mES"
RBconstraint = 0.4# 
frequency = "quarterly" ;
crit1="mES";
# Load portfolio weights:

Z                   = read.csv( file = paste(criteria[1],".csv" , sep="")  );
wstart_EW           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$TBill)
Z                   = read.csv( file = paste(criteria[2],".csv" , sep="")  );
wstart_MinCVaR      = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$TBill)
Z                   = read.csv( file = paste(criteria[3],".csv" , sep="")  );
wstart_weightconst  = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$TBill);
Z                   = read.csv( file = paste(criteria[4],".csv" , sep="")  );
wstart_RCconst      = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$TBill)
Z                   = read.csv( file = paste(criteria[5],".csv" , sep="")  );
wstart_equalrisk    = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$TBill)


cRebalancing = length(from)

cumR = c()
oosR = window (monthlyR , start = from[1] , end = to[ length(to) ] )

for( i in 1:cRebalancing ){
   sel = seq( (i-1)*3+1 , i*3 )
   cumR = rbind( cumR , apply((1+oosR[sel,]),2,'cumprod')[3,] )
}

wend_EW          = (wstart_EW[1:cRebalancing]*cumR)/rowSums( wstart_EW[1:cRebalancing]*cumR ) 
wend_MinCVaR     = (wstart_MinCVaR[1:cRebalancing]*cumR)/rowSums( wstart_MinCVaR[1:cRebalancing]*cumR )
wend_weightconst = (wstart_weightconst[1:cRebalancing]*cumR)/rowSums( wstart_weightconst[1:cRebalancing]*cumR )
wend_RCconst     = (wstart_RCconst[1:cRebalancing]*cumR)/rowSums( wstart_RCconst[1:cRebalancing]*cumR )
wend_equalrisk   = (wstart_equalrisk[1:cRebalancing]*cumR)/rowSums( wstart_equalrisk[1:cRebalancing]*cumR ) 

turnover_EW          = mean( abs(wend_EW[1:(cRebalancing-1)]          -    wstart_EW[2:cRebalancing]         ) )
turnover_MinCVaR     = mean( abs(wend_MinCVaR[1:(cRebalancing-1)]     -    wstart_MinCVaR[2:cRebalancing]    ) )
turnover_weightconst = mean( abs(wend_weightconst[1:(cRebalancing-1)] -    wstart_weightconst[2:cRebalancing]) )
turnover_RCconst     = mean( abs(wend_RCconst[1:(cRebalancing-1)]     -    wstart_RCconst[2:cRebalancing]    ) )
turnover_equalrisk   = mean( abs(wend_equalrisk[1:(cRebalancing-1)]   -    wstart_equalrisk[2:cRebalancing]  ) ) 

