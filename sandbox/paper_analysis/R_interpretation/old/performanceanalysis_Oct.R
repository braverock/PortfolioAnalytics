
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

names = c(  "EqualWeight" , 
             "MinRisk" , "MinRisk_PositionLimit" , "MinRisk_RiskLimit" , "MinRisk_ReturnTarget",
             "MinRiskConc" , "MinRiskConc_ReturnTarget", "MinRiskConc_PositionLimit")
#             "EqualRisk"  , "MinRiskConc_RiskLimit"  )

namelabels = c( "Equal Weight" , 
                 "Min CVaR" , "Min CVaR + Position Limit" , "Min CVaR + CVaR Alloc Limit" , "Min CVaR + Return Target" , 
                 "Min CVaR Conc" , "Min CVaR Conc + Return Target" , "Min Risk Conc + Position Limit")
#                 "Equal Risk"  , "Min Risk Conc + Risk Limit"  ) 

criteria =  paste( rep("weights/",8) , names , sep="")

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste("data/","data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

nominalreturns = T;
if(nominalreturns){ MonthlyR = zoo( data[,2:ncol(data)] , order.by = date ) }else{
   monthlyR = zoo( data[,2:(ncol(data)-1)] , order.by = date ) -  zoo( data[,ncol(data)] , order.by = date )
}


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
ep.end = ep[(1+estyears*4):(length(ep)-1)]+3
to = time(monthlyR)[ep.end]

# Compute daily out of sample returns, accounting for compounding effects

Returns.rebalancing( R = monthlyR , criteria = criteria, from = from, to = to , folder="/oosreturns/" ) 
oosdates = time( window (monthlyR , start = from[1] , end = to[ length(to) ] ) ) 

load(paste(getwd(),"/","/oosreturns/", "simplereturns.Rdata" ,sep="") ) 

colnames(simplereturns) = names
date = time(simplereturns)

# Benchmark strategies relative to bond index and equity index

sp500 = window (monthlyR , start = from[1] , end = to[ length(to) ] )[,2]
bear = c(1:length(sp500))[sp500<mean(sp500)]
#bear = rep(bear,each=2)
#bear = bear + rep( c(-0.5,0.5) , length(bear)/2 )

postscript( file="RelativePerformance_benchmarks.eps" , horizontal = FALSE ) 
par( mfrow = c(2,1) , mar = c(2,2.5,2,2) , cex.axis = 0.7 , cex.main=0.7 )
# EqualWeight, MinCVaR, MinCVaRConcentration
chart.RelativePerformance( simplereturns[,c(1,2,6)] , monthlyR[,c(1)] , 
   main = "Relative performance vs US bond index" , lty=c("solid","solid","solid") ,ylab="",
   col=c("black","black","darkgray"), lwd=c(1,3,3) , las=1) 

# for( i in bear ){ abline( v=time(simplereturns)[i]) } # doesn't plot

legend("bottomleft", legend = c("Equal-Weight", "Min CVaR", "Min CVaR Concentration"), lty=c("solid","solid","solid") ,
   col=c("black","black","darkgray"), lwd=c(1,3,3),cex=0.7 ) 

chart.RelativePerformance( simplereturns[,c(1,2,6)] , monthlyR[,c(2)] , 
   main = "Relative performance vs S&P 500 index" , lty=c("solid","solid","solid") ,ylab="",
   col=c("black","black","darkgray"), lwd=c(1,3,3) , las=1 ) 

dev.off()

postscript( file="RelativePerformance_alternatives.eps" , horizontal = FALSE)
# zelf opslaan anders worden de cijfers niet gedraaid in de y-as
par( mfrow = c(2,1) , mar =c(2,2.5,2,2), cex.axis = 0.7 , cex.main=0.7 )
# EqualWeight, MinCVaR, MinCVaRConcentration
chart.RelativePerformance( simplereturns[,c(3,4,5)] , simplereturns[,c(2)] , 
   main = "Relative performance vs Minimum CVaR" , lty=c("solid","solid","solid") , ylab="",
   col=c("black","black","darkgray") , las=1, lwd=c(1,3,3)) 
legend("bottomleft", legend = c("Min CVaR + Position Limit", "Min CVaR + Risk Allocation Limit", "Min CVaR + Return Target"), lty=c("solid","solid","solid") ,
   col=c("black","black","darkgray"), lwd=c(1,3,3) ,cex=0.7 ) 

chart.RelativePerformance( simplereturns[,c(9,7)] , simplereturns[,c(6)] , 
   main = "Relative performance vs Minimum CVaR Concentration" , lty=c("solid","solid") , ylab="",
   col=c("black","darkgray") , las=1 ) 
legend("bottomleft", legend = c("Min CVaR Concentration + Position Limit", "Min CVaR Concentration + Return Target"), lty=c("solid","solid") ,
   col=c("black","darkgray"), lwd=c(3,3) ,cex=0.7 ) 
dev.off()

# colnames(simplereturns)
# "EqualWeight"    "MinRisk"   "MinRisk_PositionLimit"     "MinRisk_RiskLimit"  "MinRisk_ReturnTarget" 
# "MinRiskConc"    "MinRiskConc_ReturnTarget"  "EqualRisk"  "MinRiskConc_PositionLimit" "MinRiskConc_RiskLimit"    

postscript( file="RelPerf_EW.eps" , horizontal = FALSE)
# zelf opslaan anders worden de cijfers niet gedraaid in de y-as
par( mfrow = c(2,1) , mar =c(2,2,2,2), cex.axis = 0.7 , cex.main=0.7 )
# EqualWeight, MinCVaR, MinCVaRConcentration
chart.RelativePerformance( simplereturns[,c(2,3,4,5)] , simplereturns[,c(1)] , 
   main = "" , lty=c("solid","solid","solid","solid") , ylab="", xlab="",
   col=c("black","black","darkgray","darkgray") , las=1, lwd=c(2,5,2,5) , ylim=c(0.75,1.65)) 
legend("topleft", legend = c("Min CVaR","Min CVaR + Position Limit", "Min CVaR + Risk Allocation Limit", "Min CVaR + Return Target"), 
   col=c("black","black","darkgray","darkgray"), lty=c("solid","solid","solid","solid"), lwd=c(2,5,2,5)  ,cex=0.7 ) 

chart.RelativePerformance( simplereturns[,c(6,8,7)] , simplereturns[,c(1)] , 
   main = "" , lty=c("solid","solid","solid") , ylab="",
   col=c("black","black","darkgray") , lwd=c(2,5,5), las=1 , ylim=c(0.75,1.65)) 
legend("topleft", legend = c("Min CVaR Concentration","Min CVaR Concentration + Position Limit", "Min CVaR Concentration + Return Target"), lty=c("solid","solid") ,
   col=c("black","black","darkgray"), lwd=c(2,5,5) ,cex=0.7 ) 
dev.off()


lty=c("solid","solid","solid") ,

# Assess out of sample performance of the minimum StdDev portfolio through the rolling centered 3-year rolling standard deviation

sd_S1  = rollapply(data=simplereturns[,1],width=36, FUN=sd); sd_S2 = rollapply(data=simplereturns[,2],width=36, FUN=sd)
sd_S3  = rollapply(data=simplereturns[,3],width=36, FUN=sd); sd_S4 = rollapply(data=simplereturns[,4],width=36, FUN=sd)
sd_S5  = rollapply(data=simplereturns[,5],width=36, FUN=sd); sd_S6 = rollapply(data=simplereturns[,6],width=36, FUN=sd)
sd_S7  = rollapply(data=simplereturns[,7],width=36, FUN=sd); sd_S8 = rollapply(data=simplereturns[,8],width=36, FUN=sd)
sd_S9  = rollapply(data=simplereturns[,9],width=36, FUN=sd); sd_S10 = rollapply(data=simplereturns[,10],width=36, FUN=sd)

# Assess out of sample performance of the maximum Sharpe ratio through relative cumulative performance chart

cumperf_S1 = cumprod(1 + na.omit(simplereturns[,1])) ; cumperf_S2 = cumprod(1 + na.omit(simplereturns[,2]))
cumperf_S3 = cumprod(1 + na.omit(simplereturns[,3])) ; cumperf_S4 = cumprod(1 + na.omit(simplereturns[,4]))
cumperf_S5 = cumprod(1 + na.omit(simplereturns[,5])) ; cumperf_S6 = cumprod(1 + na.omit(simplereturns[,6]))
cumperf_S7 = cumprod(1 + na.omit(simplereturns[,7])) ; cumperf_S8 = cumprod(1 + na.omit(simplereturns[,8]))
cumperf_S9 = cumprod(1 + na.omit(simplereturns[,9])) ; cumperf_S10= cumprod(1 + na.omit(simplereturns[,10]))

postscript( file=paste("fig/","relativeSD.eps",sep="") )

   par(mfrow=c(3,3), mar = c(2.2,3,3,1) , cex.main = 1.1 , cex.axis=1 , las=1); 

   plot( sd_S1 /sd_S2  , main=paste(namelabels[1],"vs",namelabels[2]) ,  ylab="", type = "l" )   ;    abline( h = 1 , lty = 3 )
   plot( sd_S3 /sd_S2  , main=paste(namelabels[3],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S4 /sd_S2  , main=paste(namelabels[4],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S5 /sd_S2  , main=paste(namelabels[5],"vs",namelabels[2]) ,  ylab="",   type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S6 /sd_S2  , main=paste(namelabels[6],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S7 /sd_S2  , main=paste(namelabels[7],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S8 /sd_S2  , main=paste(namelabels[8],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S9 /sd_S2  , main=paste(namelabels[9],"vs",namelabels[2]) ,  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )
   plot( sd_S10 /sd_S4 , main=paste(namelabels[10],"vs",namelabels[2]),  ylab="",  type = "l" ) ;    abline( h = 1 , lty = 3 )

dev.off()

postscript( file=paste("fig/","relativeperformance.eps",sep="") )

   par(mfrow=c(2,3), mar = c(2.2,3,3,1) , cex.main = 1.3 , cex.axis=1.2 , las=1); 
   ylim = c(0.5,1.3)

   plot( cumperf_S1 /cumperf_S2 , main=paste(namelabels[1]), ylab="", type = "l" , ylim = ylim )
   abline( h = 1 , lty = 3 )
   plot( cumperf_S3 /cumperf_S2 , main=paste(namelabels[3]), ylab="",   type = "l"  , ylim = ylim )
   abline( h = 1 , lty = 3 )
   plot( cumperf_S4 /cumperf_S2 , main=paste(namelabels[4]),  ylab="",  type = "l" , ylim = ylim  )
   abline( h = 1 , lty = 3 )
   plot( cumperf_S5 /cumperf_S2 , main=paste(namelabels[5]) ,  ylab="",  type = "l" , ylim = ylim  )
   abline( h = 1 , lty = 3 )
   plot( cumperf_S6 /cumperf_S2 , main=paste(namelabels[6]),  ylab="",  type = "l" , ylim = ylim  )
   abline( h = 1 , lty = 3 )
   plot( cumperf_S7 /cumperf_S2 , main=paste(namelabels[7]),  ylab="",  type = "l" , ylim = ylim  )
   abline( h = 1 , lty = 3 )


dev.off()

library(PerformanceAnalytics) 
library(zoo)
oosreturns = zoo(simplereturns[,1:10],order.by = seq.Date(as.Date(from[1])+31, as.Date(tail(to,1)) + 1, by ="month") - 1)


charts.PerformanceSummary(oosreturns)
for( i in 1:10 ){
   print( namelabels[i] )
   print( table.Drawdowns(oosreturns[,i],top=10) )
}

oosreturns_bear = oosreturns[ monthlyR[time(oosreturns),2]<mean(monthlyR[time(oosreturns),2]) ]
oosreturns_bull = oosreturns[ monthlyR[time(oosreturns),2]>=mean(monthlyR[time(oosreturns),2]) ]


# Descriptive stats
histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }


print("Full period") #median, skew; exkur; histVaR
apply( oosreturns , 2 , 'mean' )*100  ; 
apply( oosreturns , 2 , 'sd' )*100
100*apply( oosreturns , 2 , 'histCVaR')

print("Bear market")
apply( oosreturns_bear , 2 , 'mean' )*100; 
apply( oosreturns_bear , 2 , 'sd' )*100
100*apply( oosreturns_bear , 2 , 'histCVaR')

print("Bull market")
apply( oosreturns_bull , 2 , 'mean' )*100    ;
apply( oosreturns_bull , 2 , 'sd' )*100
100*apply( oosreturns_bull , 2 , 'histCVaR')

# Herfindahl Index of Concentration

oosCVaR = function( weightedR , seriesVaR ){ 
    # Gives the weighted returns for each month where the portfolio return is below the quantile
    series = rowSums(weightedR) ; q = as.numeric( seriesVaR  ) ; 
    out = -weightedR[series<(-q),]; #print(out)  # Maal gewichten doen
    return( out )
}
R=checkData(monthlyR,method="zoo")
oosR = R[ ( index(R)>=head(from,1)& index(R)<=tail(from,1)),]

out1 = out2 = out3 = out4 = out5 = out6 = out7 = out8 = out9 = out10 = c();

for( strat in 1:10 ){
    criterion = criteria[strat];
    weightedR = c(); portfolioVaR = c();
    weights = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
    for (row in 1:length(from)){
          Rrebalperiod = window(R, start = as.Date(from[row]) , end = as.Date(to[row])) ;
          weightedR = rbind( weightedR , matrix( rep( as.numeric(weights[row,]),nrow(Rrebalperiod)) , nrow = nrow(Rrebalperiod) )*Rrebalperiod   );  
          pfoosR = rowSums( matrix( rep( as.numeric(weights[row,]),nrow(oosR)) , nrow = nrow(oosR) )*oosR )
          portfolioVaR = c( portfolioVaR , histVaR( pfoosR ) ) ;
    }
    if( strat==1 ){ out1 = rbind( out1 , oosCVaR( weightedR , portfolioVaR ) ) } ;  if( strat==2 ){ out2 = rbind( out2 , oosCVaR( weightedR , portfolioVaR ) ) }
    if( strat==3 ){ out3 = rbind( out3 , oosCVaR( weightedR , portfolioVaR ) ) } ;  if( strat==4 ){ out4 = rbind( out4 , oosCVaR( weightedR , portfolioVaR ) ) }
    if( strat==5 ){ out5 = rbind( out5 , oosCVaR( weightedR , portfolioVaR ) ) } ;  if( strat==6 ){ out6 = rbind( out6 , oosCVaR( weightedR , portfolioVaR ) ) }
    if( strat==7 ){ out7 = rbind( out7 , oosCVaR( weightedR , portfolioVaR ) ) } ;  if( strat==8 ){ out8 = rbind( out8 , oosCVaR( weightedR , portfolioVaR ) ) }
    if( strat==9 ){ out9 = rbind( out9 , oosCVaR( weightedR , portfolioVaR ) ) } ;  if( strat==10){ out10= rbind( out10 , oosCVaR( weightedR , portfolioVaR ) ) }
}

HF = function(Z){ 
    T = nrow(Z); n = ncol(Z)
    h = apply(Z^2,2,'sum') 
    out = ( sum(h)/nrow(Z) )
    return(out)
}

rbind( namelabels , c( HF(out1),HF(out2),HF(out3),HF(out4),HF(out5),HF(out6),HF(out7),HF(out8),HF(out9),HF(out10)) ) 

table.DownsideRisk(oosreturns)


# Portfolio turnover per strategy:

# Load portfolio weights:

Z                   = read.csv( file = paste(criteria[1],".csv" , sep="")  );
wstart_S1           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI )
Z                   = read.csv( file = paste(criteria[2],".csv" , sep="")  );
wstart_S2           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[3],".csv" , sep="")  );
wstart_S3           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI);
Z                   = read.csv( file = paste(criteria[4],".csv" , sep="")  );
wstart_S4           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[5],".csv" , sep="")  );
wstart_S5           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[6],".csv" , sep="")  );
wstart_S6           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[7],".csv" , sep="")  );
wstart_S7           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[8],".csv" , sep="")  );
wstart_S8           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[9],".csv" , sep="")  );
wstart_S9           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)
Z                   = read.csv( file = paste(criteria[10],".csv" , sep="")  );
wstart_S10           = cbind(Z$Bond,Z$SP500,Z$EAFE,Z$SPGSCI)

cRebalancing = length(from)

cumR = c()
oosR = window (monthlyR , start = from[1] , end = to[ length(to) ] )

for( i in 1:cRebalancing ){
   sel = seq( (i-1)*3+1 , i*3 )
   cumR = rbind( cumR , apply((1+oosR[sel,]),2,'cumprod')[3,] )
}

wend_S1          = (wstart_S1[1:cRebalancing,]*cumR)/rowSums( wstart_S1[1:cRebalancing,]*cumR ) 
wend_S2          = (wstart_S2[1:cRebalancing,]*cumR)/rowSums( wstart_S2[1:cRebalancing,]*cumR )
wend_S3          = (wstart_S3[1:cRebalancing,]*cumR)/rowSums( wstart_S3[1:cRebalancing,]*cumR )
wend_S4          = (wstart_S4[1:cRebalancing,]*cumR)/rowSums( wstart_S4[1:cRebalancing,]*cumR )
wend_S5          = (wstart_S5[1:cRebalancing,]*cumR)/rowSums( wstart_S5[1:cRebalancing,]*cumR ) 
wend_S6          = (wstart_S6[1:cRebalancing,]*cumR)/rowSums( wstart_S6[1:cRebalancing,]*cumR ) 
wend_S7          = (wstart_S7[1:cRebalancing,]*cumR)/rowSums( wstart_S7[1:cRebalancing,]*cumR ) 
wend_S8          = (wstart_S8[1:cRebalancing,]*cumR)/rowSums( wstart_S8[1:cRebalancing,]*cumR ) 
wend_S9          = (wstart_S9[1:cRebalancing,]*cumR)/rowSums( wstart_S9[1:cRebalancing,]*cumR ) 
wend_S10         = (wstart_S10[1:cRebalancing,]*cumR)/rowSums( wstart_S10[1:cRebalancing,]*cumR ) 

turnover_S1          = mean( abs(wend_S1[1:(cRebalancing-1),] -    wstart_S1[2:cRebalancing,]         ) )
turnover_S2          = mean( abs(wend_S2[1:(cRebalancing-1),] -    wstart_S2[2:cRebalancing,]    ) )
turnover_S3          = mean( abs(wend_S3[1:(cRebalancing-1),] -    wstart_S3[2:cRebalancing,]) )
turnover_S4          = mean( abs(wend_S4[1:(cRebalancing-1),] -    wstart_S4[2:cRebalancing,]    ) )
turnover_S5          = mean( abs(wend_S5[1:(cRebalancing-1),] -    wstart_S5[2:cRebalancing,]  ) ) 
turnover_S6          = mean( abs(wend_S6[1:(cRebalancing-1),] -    wstart_S6[2:cRebalancing,]  ) ) 
turnover_S7          = mean( abs(wend_S7[1:(cRebalancing-1),] -    wstart_S7[2:cRebalancing,]  ) ) 
turnover_S8          = mean( abs(wend_S8[1:(cRebalancing-1),] -    wstart_S8[2:cRebalancing,]  ) ) 
turnover_S9          = mean( abs(wend_S9[1:(cRebalancing-1),] -    wstart_S9[2:cRebalancing,]  ) ) 
turnover_S10         = mean( abs(wend_S10[1:(cRebalancing-1),] -    wstart_S10[2:cRebalancing,]  ) ) 

print( cbind( namelabels , c(turnover_S1, turnover_S2 , turnover_S3, turnover_S4, turnover_S5,
                           turnover_S6, turnover_S7 , turnover_S8 , turnover_S9, turnover_S10) ));
