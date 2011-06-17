
setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Optimized portfolio you want to analyse out-of-sample performance through (Component) Sharpe ratios

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
criteria = c( paste(mainname,"EW",sep="")  , paste(  criterion , "-InfInf" , sep="") , paste( criterion , "-Inf" , 0.4 , sep="")  , 
         paste( criterion , "0.2" , "0.3" , sep="") ); 
# Load additional programs to interpret the data
library(zoo); library("PerformanceAnalytics"); 


#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2009; lastquarter = 2; 

data = read.table( file= paste("data/",datacase,"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
data = zoo( data[,2:ncol(data)] , order.by = date )

# "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 

cAssets = 4; 
head(data); 

monthlyR = data[,(1:(cAssets+1))]
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


# Load the weights

# Load portfolio weights:

weightsEW = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/EW.csv" , sep="")  );
criterion = paste(crit1,".",estyears,"yr",sep="")
weightsInf= read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/",criterion , "-InfInf.csv" , sep="")  );
weightsRBcon = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/", criterion , "-Inf",RBconstraint,".csv" , sep="")  );
weightsERisk = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/", criterion , 0.2,0.3,".csv" , sep="")  );

 

# Construct matrix holding the corresponding weights for each month 

if(frequency=="yearly"){ cc = 12 }else{  cc = 3 }

cRebalancing = length(from);
WEW = WInf = WRBcon = WERisk =qEW = qInf = qRBcon = qERisk =c()

evalR = window(monthlyR , start = as.Date(from[1]) , end = as.Date(tail(to,n=1)) ) ;nobs = dim(evalR)[1];

for (row in 1:cRebalancing){ 
        WInfrow = as.numeric(weightsInf[row,]); WRBconrow = as.numeric(weightsRBcon[row,]) ; WERiskrow = as.numeric(weightsERisk[row,]) ;  
        WEW = rbind( WEW , matrix( rep( c(rep(1/cAssets,cAssets),0) , cc ) , nrow= cc , byrow=T) )
        WInf = rbind( WInf , matrix( rep( WInfrow , cc ) , nrow= cc , byrow=T) )
        WRBcon = rbind( WRBcon , matrix(  rep(WRBconrow , cc )  , nrow= cc , byrow=T) )
        WERisk = rbind( WERisk , matrix(  rep(WERiskrow , cc )  , nrow= cc , byrow=T) )
        # compute also in this loop the 5% quantile: different for each portfolio weight 
        preturnsEWrow = rowSums( evalR*matrix( rep(  c(rep(1/cAssets,cAssets),0),nrow(evalR) ), nrow =nrow(evalR) , byrow=T ) );
        preturnsInfrow = rowSums( evalR*matrix( rep( WInfrow,nrow(evalR) ), nrow =nrow(evalR) , byrow=T ) );
        preturnsRBconrow = rowSums( evalR*matrix(rep( WRBconrow,nrow(evalR)), nrow =nrow(evalR),byrow=T ) );
        preturnsERiskrow = rowSums( evalR*matrix(rep( WERiskrow,nrow(evalR)), nrow =nrow(evalR),byrow=T ) );
        qEW = c( qEW , as.numeric(quantile( preturnsInfrow, probs=alpha )) )
        qInf = c( qInf , as.numeric(quantile( preturnsInfrow, probs=alpha )) )
        qRBcon = c( qRBcon , as.numeric(quantile( preturnsRBconrow, probs=alpha )) )
        qERisk = c( qERisk , as.numeric(quantile( preturnsERiskrow, probs=alpha )) )
}

# Compute portfolio returns (We need the aggregation property)

preturnsEW = rowSums( evalR*WEW )
preturnsInf = rowSums( evalR*WInf )
preturnsRBcon = rowSums( evalR*WRBcon )
preturnsERisk = rowSums( evalR*WERisk )

# Compute portfolio Sharpe ratio

riskfree.R = window(monthlyRTBill, start = as.Date(from[1]) , end = as.Date(tail(to,n=1)) ) ;
excessEW    = mean(preturnsEW) -  mean(riskfree.R) 
excessInf   = mean(preturnsInf) -  mean(riskfree.R) 
excessRBcon = mean(preturnsRBcon) -  mean(riskfree.R) 
excessERisk = mean(preturnsERisk) -  mean(riskfree.R)

# Standard sharpe ratio 

portfolioSREW = sqrt(12)*excessEW/( sd(preturnsEW) )           ; print(- portfolioSREW )
portfolioSRInf = sqrt(12)*excessInf/(sd(preturnsInf) )         ; print(- portfolioSRInf )
portfolioSRRBcon = sqrt(12)*excessRBcon/( sd(preturnsRBcon) )  ; print(- portfolioSRRBcon )
portfolioSRERisk = sqrt(12)*excessERisk /( sd(preturnsERisk) ) ; print(- portfolioSRERisk ) 


# Indicator for beyon VaR losses

indicatorEW = na.omit( c(1:nobs)[preturnsEW<qEW ]);
indicatorInf = na.omit( c(1:nobs)[preturnsInf<qInf ]);
indicatorRBcon = na.omit( c(1:nobs)[preturnsRBcon<qRBcon ]);
indicatorERisk = na.omit( c(1:nobs)[preturnsERisk<qERisk ]);

HCVaREW = -( sum(preturnsEW[indicatorEW]) )
HCVaRInf = -(sum(preturnsInf[indicatorInf]) )
HCVaRRBcon = -( sum(preturnsRBcon[indicatorRBcon]) )
HCVaRERisk = -( sum(preturnsERisk[indicatorERisk]) )

portfolioSREW = sqrt(12)*excessEW/HCVaREW
portfolioSRInf = sqrt(12)*excessInf/HCVaRInf
portfolioSRRBcon = sqrt(12)*excessRBcon/HCVaRRBcon
portfolioSRERisk = sqrt(12)*excessERisk /HCVaRERisk


# Compute Component Sharpe ratio

componentSREW = sqrt(12)*( apply( evalR*WEW - riskfree.R*WEW, 2 , 'mean' )  )/HCVaREW
componentSRInf = sqrt(12)*( apply( evalR*WInf - riskfree.R*WInf, 2 , 'mean' )  )/HCVaRInf
componentSRRBcon = sqrt(12)*( apply( evalR*WRBcon - riskfree.R*WRBcon, 2 , 'mean' )  )/HCVaRRBcon
componentSRERisk = sqrt(12)*( apply( evalR*WERisk - riskfree.R*WERisk, 2 , 'mean' )  )/HCVaRERisk

adjEW    = ( apply( WEW[indicatorEW,]*(evalR[indicatorEW,]-preturnsEW[indicatorEW]) , 2 , 'sum')   )/HCVaREW^2
adjInf   = ( apply( WInf[indicatorInf,]*(evalR[indicatorInf,]-preturnsInf[indicatorInf]) , 2 , 'sum')   )/HCVaRInf^2
adjRBcon = ( apply( WRBcon[indicatorRBcon,]*(evalR[indicatorRBcon,]-preturnsRBcon[indicatorRBcon]) , 2 , 'sum')   )/HCVaRRBcon^2
adjERisk = ( apply( WERisk[indicatorERisk,]*(evalR[indicatorERisk,]-preturnsERisk[indicatorERisk]) , 2 , 'sum')   )/HCVaRERisk^2

print("Equal-weighted") ;  
print("------------------------------------------------------------")
print("Total Sharpe ratio portfolio without risk budget constraints")
print(portfolioSREW); 
print("And its decomposition with lambda = 0")
print(componentSREW)
print("And its decomposition with lambda = 0.05")
print(componentSREW + 0.05*adjEW)
print("And its decomposition with lambda = 0.1")
print(componentSREW + 0.1*adjEW)

print("Min CVaR portfolios") ;  
print("------------------------------------------------------------")
print("Total Sharpe ratio portfolio without risk budget constraints")
print(portfolioSRInf); 
print("And its decomposition with lambda = 0")
print(componentSRInf)
print("And its decomposition with lambda = 0.05")
print(componentSRInf + 0.05*adjInf)
print("And its decomposition with lambda = 0.1")
print(componentSRInf + 0.1*adjInf)

print("------------------------------------------------------------")
print("Total Sharpe ratio portfolio with risk budget constraints")
print(portfolioSRRBcon );
print("And its decomposition without adjustment")
print(componentSRRBcon)
print("And its decomposition with lambda = 0.05")
print(componentSRRBcon+ 0.05*adjRBcon)
print("And its decomposition with lambda = 0.1")
print(componentSRRBcon+ 0.1*adjRBcon)

print("------------------------------------------------------------")
print("Total Sharpe ratio equal-risk portfolio")
print(portfolioSRERisk );
print("And its decomposition without adjustment")
print(componentSRERisk)
print("And its decomposition with lambda = 0.05")
print(componentSRERisk+ 0.05*adjERisk)
print("And its decomposition with lambda = 0.1")
print(componentSRERisk+ 0.1*adjERisk)

