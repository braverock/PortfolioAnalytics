
# ----------------------------------------------------------------------------------
# In this script the out-of-sample returns of the optimized portfolios is analyzed
# 
# ----------------------------------------------------------------------------------

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Optimized portfolio you want to analyse out-of-sample performance through (Component) Sharpe ratios

riskcrit = mincriterion = percriskcontribcriterion ="mES"; # "mES"  "StdDev" "GES"
estyears = 8;
frequency = "quarterly" ;yearly = F;
CC = TRUE
# Load additional programs to interpret the data

library(zoo); library("PerformanceAnalytics"); source("R_interpretation/pfolioreturn.R"); 
source("R_Allocation/estimators.R"); library(zoo);  
library(reldist) ; library(sandwich); library(zoo);  

histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }

# Define optimization criteria

names = c(  "EqualWeight" , "MinRisk" , "MinRisk_PositionLimit" , "MinRisk_RiskLimit" ,
            "MinRiskConc" , "MinRiskConc_PositionLimit", "EqualRisk" ,
            "MinRisk_ReturnTarget", "MinRiskConc_ReturnTarget" )
if(CC){ names= paste( names, "_CC", sep="") }

namelabels = c( "Equal Weight" , "Min CVaR" , "Min CVaR + Position Limit" , "Min CVaR + CVaR Alloc Limit" ,  
       "Min CVaR Conc" , "Min CVaR Conc + 40% Position Limit", "Min CVaR + ERC constraint" , "Min CVaR + Return Target" , "Min CVaR Conc + Return Target" )

if(mincriterion=="mES"){ sel = c(1,2:4,7,5:6) }else{ sel = c(1,2:4,7,5:6) }

#sel = c(1,2,5)
names = names[sel]; namelabels = namelabels[sel]; 

criteria =  paste( rep("weights/",length(names) ) , rep(mincriterion,length(names) ) , "/", names , sep="")
criteria[ criteria == "weights/StdDev/EqualWeight" ] = "weights/mES/EqualWeight"

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste("data/","data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

nominalreturns = T;
if(nominalreturns){ monthlyR = zoo( data[,2:ncol(data)] , order.by = date ) }else{
   monthlyR = zoo( data[,2:(ncol(data)-1)] , order.by = date ) -  zoo( data[,ncol(data)] , order.by = date )
}
monthlyR = monthlyR[,1:4]

# estimation periods
ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[1:(length(ep)-estyears*4)]+1
from = time(monthlyR)[ep.start]
from.estimation = seq( as.Date(paste(firstyear,"-01-01",sep="")), as.Date(paste(lastyear-estyears,"-07-01",sep="")), by="3 month") 
ep.end = ep[(1+estyears*4):length(ep)]
to.estimation = time(monthlyR)[ep.end]
cPeriods = length(from);

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[(1+estyears*4):(length(ep)-1)]+1
from = time(monthlyR)[ep.start]
from = seq( as.Date( paste( as.character(firstyear+estyears),"-01-01",sep="")), as.Date("2010-04-01"), by="3 month") 
ep.end = ep[(1+estyears*4):(length(ep)-1)]+3
to = time(monthlyR)[ep.end]

oosdates = time( window (monthlyR , start = from[1] , end = to[ length(to) ] ) )

makeTacticalWeights = TRUE

if(makeTacticalWeights){

  # Compute daily out of sample returns, accounting for compounding effects
  Returns.rebalancing( R = monthlyR , criteria = criteria, from = from, to = to , folder="/oosreturns/" ) 
   
  load(paste(getwd(),"/","/oosreturns/", "simplereturns.Rdata" ,sep="") ) 

  colnames(simplereturns) = names;  date = time(simplereturns)

  library("TTR")
  weights_MinRisk     = read.csv( file = paste("weights/", riskcrit , "/",  "MinRisk_CC", ".csv" , sep="")  );

  names_alternatives = c(  "EqualWeight" , "MinRisk_PositionLimit" , "MinRisk_RiskLimit" ,
            "EqualRisk" , "MinRiskConc" , "MinRiskConc_PositionLimit")
  if(CC){ names_alternatives = paste( names_alternatives, "_CC", sep="") }

  for(name in names_alternatives){
   weights_alternative = read.csv( file = paste("weights/", riskcrit , "/", name , ".csv" , sep="")  ); 
   weights_tact = weights_alternative
   z = simplereturns[,name]
   cz = cumprod( 1+z ) ; smaz = SMA(cz , n = 10)
   risky = 1*(cz > smaz ) ; risky[is.na(risky)] = 0;
   # end of rebal period value
   risky = risky[to]
   # we shift since for first rebal period no obs
   risky = as.numeric(c( 0 , risky[-1] ))
   weights_tact[ risky==0 , ] = weights_MinRisk[ risky==0 , ]
   write.table( weights_tact , file = paste("weights/", riskcrit , "/",  "tact_" , name , ".csv" , sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
 }

}

newnames = paste( rep( "tact_" , length(names_alternatives) ) , names_alternatives, sep="" )
tact_criteria =  paste( rep("weights/",length(newnames) ) , rep(mincriterion,length(newnames) ) , "/", newnames , sep="")

Returns.rebalancing( R = monthlyR , criteria = tact_criteria, from = from, to = to , folder="/oosreturns/" ) 
load(paste(getwd(),"/","/oosreturns/", "simplereturns.Rdata" ,sep="") ) 
colnames(simplereturns) = names_alternatives;  date = time(simplereturns)



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

#source( paste(getwd(),"/R_interpretation/findDrawdowns.R",sep="") )
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

# Table of summary statistics on out-of-sample returns 

library(PerformanceAnalytics) 
library(zoo)
oosreturns = simplereturns;
#zoo(simplereturns[,c(1:length(sel))],order.by = seq.Date(as.Date(from[1])+31, as.Date(tail(to,1)) + 1, by ="month") - 1)

v.nobear.dates = as.Date(setdiff( time(oosreturns) , v.bear.dates ))

# Mean, Standard Deviation, CVaR 
histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }


########################################################################


oosreturns = window(oosreturns , start=as.Date("1984-01-01") , end=tail(time(oosreturns),1) )
Tstart = 1+(8-estyears)*4
#Tstart = 1

out_full = out_bear = out_bull = c()

print("Full period") #median, skew; exkur; histVaR
out_full = rbind( out_full , apply( oosreturns , 2 , 'mean' )*100*12  )
out_full = rbind( out_full , apply( oosreturns , 2 , 'sd' )*100*sqrt(12))
out_full = rbind( out_full , apply( oosreturns , 2 , 'skewness' ))
out_full = rbind( out_full , apply( oosreturns , 2 , 'kurtosis' ))
out_full = rbind( out_full  , 100*apply( oosreturns , 2 , 'histCVaR'))
out_full = rbind( out_full , -100*apply( oosreturns , 2 , 'ES') )


oosreturns_bear = oosreturns[ v.bear.dates ]
oosreturns_bull = oosreturns[ v.nobear.dates ]

print("Bear market")
out_bear = rbind( out_bear , apply( oosreturns_bear , 2 , 'mean' )*100*12); 
out_bear = rbind( out_bear , apply( oosreturns_bear , 2 , 'sd' )*100*sqrt(12))
out_bear = rbind( out_bear , apply( oosreturns_bear , 2 , 'skewness' ))
out_bear = rbind( out_bear , apply( oosreturns_bear , 2 , 'kurtosis' ))
out_bear = rbind( out_bear , 100*apply( oosreturns_bear , 2 , 'histCVaR'))
out_bear = rbind( out_bear , -100*apply( oosreturns_bear , 2 , 'ES'))

print("Bull market")
out_bull = rbind( out_bull , apply( oosreturns_bull , 2 , 'mean' )*100*12  )  ;
out_bull = rbind( out_bull , apply( oosreturns_bull , 2 , 'sd' )*100*sqrt(12))
out_bull = rbind( out_bull , apply( oosreturns_bull , 2 , 'skewness' ))
out_bull = rbind( out_bull , apply( oosreturns_bull , 2 , 'kurtosis' ))
out_bull = rbind( out_bull , 100*apply( oosreturns_bull , 2 , 'histCVaR'))
out_bull = rbind( out_bull , -100*apply( oosreturns_bull , 2 , 'ES'))




# Portfolio turnover per strategy:

pfgini = c();
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

for( strat in 1:length(tact_criteria) ){
    criterion = tact_criteria[strat];
    wstart = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
    wend   = (wstart[Tstart:cRebalancing,]*cumR)/rowSums( wstart[Tstart:cRebalancing,]*cumR )  
    out  = rowSums(  abs( wstart[(Tstart+1):cRebalancing,]-wend[Tstart:(cRebalancing-1),]     ))
    turnover  = cbind( turnover , out )
    pfgini = c( pfgini , mean(apply( wstart, 1 , 'gini' )) )
}

pfturnover = colMeans( turnover )
out_full = rbind( out_full , pfgini ,  pfturnover*100  )

#out_full = rbind( out_full ,  turnover*100 )

library(xtable)

xtable(out_full)
xtable(out_bear)
xtable(out_bull)

# DRAWDOWNS

for( i in 1:length(tact_criteria) ){ # Print the drawdowns
   print( tact_criteria[i] )
   out =  table.Drawdowns(oosreturns[,i],top=10)
   out = out[out$Depth<=(-0.1),]
   print( out )
}

# Risk concentration

outloss = outriskconc = c();

for( strat in 1:length(tact_criteria) ){
    criterion = tact_criteria[strat];
    #print( criterion );
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
    #out = mean(weightedR[series<(-portfolioVaR),]^2); 
    downsidelosses = weightedR[series<(-portfolioVaR),]
    #downsidelosses = weightedR[series<=-0.10,]
    vES = rowSums(downsidelosses)

    #print("Total portfolio loss")
    out = apply(   -downsidelosses , 1 , 'sum') 
    outloss = cbind( outloss ,  c( median(out) , max(out) ) )
    #print("Max percentage loss")
    out = apply(   downsidelosses/ apply(   downsidelosses , 1 , 'sum') , 1 , 'max') 
    outriskconc = cbind( outriskconc , c( mean(out) , max(out) ) )
}

xtable(outloss)
xtable(outriskconc)
