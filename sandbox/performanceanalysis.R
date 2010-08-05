
# ----------------------------------------------------------------------------------
# In this script the out-of-sample returns of the optimized portfolios is analyzed
# 
# ----------------------------------------------------------------------------------

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Optimized portfolio you want to analyse out-of-sample performance through (Component) Sharpe ratios

estyears = 8;
percriskcontribcriterion = "mES"
frequency = "quarterly" ;yearly = F;

# Load additional programs to interpret the data

library(zoo); library("PerformanceAnalytics"); source("R_interpretation/pfolioreturn.R"); 
source("R_Allocation/estimators.R"); library(zoo);  
histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }

# Define optimization criteria

names = c(  "EqualWeight" , "MinRisk" , "MinRisk_PositionLimit" , "MinRisk_RiskLimit" ,
             "MinRiskConc" , "MinRiskConc_PositionLimit", "MinRisk_ReturnTarget", "MinRiskConc_ReturnTarget")

namelabels = c( "Equal Weight" , "Min CVaR" , "Min CVaR + Position Limit" , "Min CVaR + CVaR Alloc Limit" ,  
       "Min CVaR Conc" , "Min CVaR Conc + Position Limit", "Min CVaR + Return Target" , "Min CVaR Conc + Return Target" )

criteria =  paste( rep("weights/",8) , names , sep="")

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste("data/","data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

nominalreturns = T;
if(nominalreturns){ monthlyR = zoo( data[,2:ncol(data)] , order.by = date ) }else{
   monthlyR = zoo( data[,2:(ncol(data)-1)] , order.by = date ) -  zoo( data[,ncol(data)] , order.by = date )
}
monthlyR = monthlyR[,1:4]

summarystats_assets = FALSE;
if(summarystats_assets){
   apply( monthlyR , 2 , 'mean' )*100 ;    apply( monthlyR , 2 , 'sd' )*100
   apply( monthlyR , 2 , 'skew' )     ;    apply( monthlyR , 2 , 'exkur' )
   apply( monthlyR , 2 , 'histCVaR' ) ;    cor(monthlyR)
}


# Define the out-of-sample periods 

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[(1+estyears*4):(length(ep)-1)]+1
from = time(monthlyR)[ep.start]
from = seq( as.Date("1984-01-01"), as.Date("2010-04-01"), by="3 month") 
ep.end = ep[(1+estyears*4):(length(ep)-1)]+3
to = time(monthlyR)[ep.end]



# Compute daily out of sample returns, accounting for compounding effects

Returns.rebalancing( R = monthlyR , criteria = criteria, from = from, to = to , folder="/oosreturns/" ) 
oosdates = time( window (monthlyR , start = from[1] , end = to[ length(to) ] ) ) 

load(paste(getwd(),"/","/oosreturns/", "simplereturns.Rdata" ,sep="") ) 

colnames(simplereturns) = names
date = time(simplereturns)


# Bear periods
sp500 = window (monthlyR , start = from[1] , end = to[ length(to) ] )[,2]
bear = c(1:length(sp500))[sp500<mean(sp500)]
bear = c(1:length(sp500))[sp500<(-0.12)]
m.bear.dates = list();
i=1;
for( b in bear){
 m.bear.dates[[i]] = c( b-0.5, b+0.5)
 i = i + 1;  
}

# http://www.aheadofthecurve-thebook.com/charts.html
# Vertical yellow bars in most charts denote bear markets (declines in the S&P 500 Index of 12% or more). 
# IMPORTANT: The leading edge (left side) of the vertical yellow bars are thus stock market peaks, 
# and the trailing edge (right side) are stock market troughs.

out = table.Drawdowns(sp500,top=10) 
start.bear = out$From[out$Depth<(-0.12)]
end.bear = out$Trough[out$Depth<(-0.12)]
start.bear.index = c(1:length(sp500))[ time(sp500) ]
m.bear.dates = list()
v.bear.dates = c()
for( i in 1:length(start.bear) ){
   m.bear.dates[[i]] = c( as.yearmon(start.bear[i]) , as.yearmon(end.bear[i]) )
   v.bear.dates = c( v.bear.dates , seq(start.bear[i],end.bear[i],"days") )
}
v.bear.dates = as.Date( v.bear.dates )


# Chart of relative performance strategies vs Equal-Weight 

postscript( file="RelPerf_EW.eps" , horizontal = FALSE)
# zelf opslaan anders worden de cijfers niet gedraaid in de y-as
par( mfrow = c(2,1) , mar =c(2,2,2,2), cex.axis = 0.7 , cex.main=0.7 )
# EqualWeight, MinCVaR, MinCVaRConcentration
chart.RelativePerformance( simplereturns[,c(2,3,4)] , simplereturns[,c(1)] , 
   main = "" , lty=c("solid","solid","solid") , ylab="", xlab="",
   col=c("black","darkgray","darkgray") , las=1, lwd=c(2,2,5) , 
   auto.grid = TRUE, minor.ticks = FALSE ,ylim=c(0.7,1.65),
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y") 
legend("topleft", legend = c("Min CVaR","Min CVaR + Position Limit", "Min CVaR + Risk Allocation Limit"), 
   col=c("black","darkgray","darkgray"), lty=c("solid","solid","solid"), lwd=c(2,2,5)  ,cex=0.7) 

chart.RelativePerformance( simplereturns[,c(5,6)] , simplereturns[,c(1)] , 
   main = "" , lty=c("solid","solid","solid") , ylab="",
   col=c("black","darkgray") , lwd=c(2,2), las=1 , 
   auto.grid = TRUE, minor.ticks = FALSE , ylim=c(0.7,1.65), 
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y")  

legend("topleft", legend = c("Min CVaR Concentration","Min CVaR Concentration + Position Limit"), lty=c("solid","solid") ,
   col=c("black","darkgray"), lwd=c(2,2) ,cex=0.7 ) 
dev.off()


# Table of summary statistics on out-of-sample returns 

library(PerformanceAnalytics) 
library(zoo)
oosreturns = zoo(simplereturns[,1:6],order.by = seq.Date(as.Date(from[1])+31, as.Date(tail(to,1)) + 1, by ="month") - 1)

v.nobear.dates = as.Date(setdiff( time(oosreturns) , v.bear.dates ))

# Mean, Standard Deviation, CVaR 
histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }

print("Full period") #median, skew; exkur; histVaR
apply( oosreturns , 2 , 'mean' )*100*12  ; 
apply( oosreturns , 2 , 'sd' )*100
100*apply( oosreturns , 2 , 'histCVaR')

oosreturns_bear = oosreturns[ v.bear.dates ]
oosreturns_bull = oosreturns[ v.nobear.dates ]

print("Bear market")
apply( oosreturns_bear , 2 , 'mean' )*100*12; 
apply( oosreturns_bear , 2 , 'sd' )*100
100*apply( oosreturns_bear , 2 , 'histCVaR')

print("Bull market")
apply( oosreturns_bull , 2 , 'mean' )*100*12    ;
apply( oosreturns_bull , 2 , 'sd' )*100
100*apply( oosreturns_bull , 2 , 'histCVaR')

for( i in 1:6 ){ # Print the drawdowns
   print( namelabels[i] )
   print( table.Drawdowns(oosreturns[,i],top=10) )
}

# Herfindahl Index of Concentration

HI = c()

for( strat in 1:6 ){
    criterion = criteria[strat];
    weightedR = c(); portfolioVaR = c();
    weights = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")

    # Step 1: compute for each optimal weight the corresponding historical quantile

    for (row in 1:length(from)){
          # For the determination of the historical quantile all returns preceding the rebalancing period are taken
          previousR = window(monthlyR, start = time(monthlyR)[1] , end = as.Date(from[row]-1)) ;
          pfoosR = rowSums( matrix( rep( as.numeric(weights[row,]),nrow(previousR)) , nrow = nrow(previousR) )*previousR )
          # The weighted returns need the returns of the rebalancing period
          Rrebalperiod = window(monthlyR, start = as.Date(from[row]) , end = as.Date(to[row])) ;
          weightedR = rbind( weightedR , matrix( rep( as.numeric(weights[row,]),nrow(Rrebalperiod)) , nrow = nrow(Rrebalperiod) )*Rrebalperiod   );  
          portfolioVaR = c( portfolioVaR , histVaR( pfoosR ) ) ;
    }

    # Step 2: compute the mean squared weighted return over months with beyond VaR losses

    series = rowSums(weightedR) ; 
    out = mean(weightedR[series<(-portfolioVaR),]^2); 
    #out = mean(apply(weightedR[series<(-portfolioVaR),],1,'max')); 
    HI = c( HI , out )
}
rbind( namelabels , HI*100 )

# Portfolio turnover per strategy:

turnover = c();

# Compute for each rebalancing period, the cumulative return:

cumR = c()
oosR = window (monthlyR , start = from[1] , end = to[ length(to) ] )
cRebalancing = length(from)

for( i in 1:cRebalancing ){
   sel = seq( (i-1)*3+1 , i*3 )
   cumR = rbind( cumR , apply((1+oosR[sel,]),2,'cumprod')[3,] )
}

# Load portfolio weights:

for( strat in 1:6 ){
    criterion = criteria[strat];
    wstart = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
    wend   = (wstart[1:cRebalancing,]*cumR)/rowSums( wstart[1:cRebalancing,]*cumR )  
    out  = mean(  abs(wend[1:(cRebalancing-1),] -    wstart[2:cRebalancing,]         ))
    turnover  = c( turnover , mean(out) )
}

print( rbind( namelabels[1:6] , turnover*100) );
