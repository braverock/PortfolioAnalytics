# Paper: Portfolio Optimization with Conditional Value-at-Risk Budgets
# Boudt, Carl, Peterson (2010)
# This R script serves to make the exhibits regarding the four asset efficient frontier

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs/insample")

cAssets = 4; p = priskbudget = 0.95;
mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("PortfolioAnalytics")
clean = TRUE; CC = T

#   Load the data
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste(getwd(),"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

monthlyR = zoo( data[,2:(1+cAssets)] , order.by = date )
set.seed(1234)
if(clean){ monthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]] }
mu = apply(monthlyR,2,'mean')
sigma = cov(monthlyR)
if(!CC){
  M3 = PerformanceAnalytics:::M3.MM(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  ); 
  M4 = PerformanceAnalytics:::M4.MM(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  )
}else{
  source( "coskewkurtosis.R"  ) 
  M3 = coskewCC(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  ); 
  M4 = cokurtCC(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  );
}
N = ncol(monthlyR)

# Summary stats individual assets
apply(monthlyR,2,'mean')*12
apply(monthlyR,2,'sd')*sqrt(12)
apply(monthlyR,2,'skewness')
apply(monthlyR,2,'kurtosis')
ES(monthlyR[,1],method="modified"); ES(monthlyR[,2],method="modified")
ES(monthlyR[,3],method="modified"); ES(monthlyR[,4],method="modified")

#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################

# Layout 3 
source("chart.StackedBar.R"); 

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )

mESfun2 = function( w ){ return( operPortMES(w,mu=mu,alpha=0.05,sigma=sigma,M3=M3,M4=M4)[[1]]   ) }
assetCVaR = rep(0,4);
for( i in 1:4 ){
  w = rep(0,4); w[i]=1;
  assetCVaR[i] = mESfun2( as.matrix(w) )
}

#> assetCVaR
#[1] 0.01235899 0.12465759 0.27755656 0.20558527

minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);

# Load the data

labelnames = c( "US bond" , "S&P 500" , "NAREIT" , "GSCI" )


if(clean){
 # Portfolio weights
 W_MCC                 = read.csv( file = "EffFrontierMinCVaRConc_weights_clean_CC.csv" )[,2:(1+cAssets)]
 W_minCVaR             = read.csv( file = "EffFrontierMinCVaR_weights_clean_CC.csv" )[,2:(1+cAssets)]
 W_minVar              = read.csv( file = "EffFrontierMinVar_weights_clean.csv" )[,2:(1+cAssets)]
 # Percentage CVaR contributions
 PercCVaR_MCC         = read.csv(  file = "EffFrontierMinCVaRConc_percrisk_clean_CC.csv" )[,2:(1+cAssets)]
 PercCVaR_minCVaR     = read.csv(  file = "EffFrontierMinCVaR_percrisk_clean_CC.csv" )[,2:(1+cAssets)]
 PercCVaR_minVar      = read.csv(  file = "EffFrontierMinVar_percrisk_clean.csv" )[,2:(1+cAssets)]
 # Summary stats
 EffFrontier_stats_MCC = read.csv(file = "EffFrontierMinCVaRConc_stats_clean_CC.csv")[,2:5]
 EffFrontier_stats_minCVaR = read.csv(file = "EffFrontierMinCVar_stats_clean_CC.csv")[,2:5]
 EffFrontier_stats_minVar = read.csv(file = "EffFrontierMinVar_stats_clean.csv")[,2:5]

}else{
 # Portfolio weights
 W_MCC                 = read.csv(file = "EffFrontierMinCVaRConc_weights_CC.csv")[,2:(1+cAssets)]
 W_minCVaR             = read.csv(file = "EffFrontierMinCVaR_weights_CC.csv")[,2:(1+cAssets)]
 W_minVar              = read.csv(file = "EffFrontierMinVar_weights.csv")[,2:(1+cAssets)]
 # Percentage CVaR contributions
 PercCVaR_MCC          = read.csv( file = "EffFrontierMinCVaRConc_percrisk_CC.csv" )[,2:(1+cAssets)]
 PercCVaR_minCVaR      = read.csv( file = "EffFrontierMinCVaR_percrisk_CC.csv" )[,2:(1+cAssets)]
 PercCVaR_minVar        = read.csv(file = "EffFrontierMinVar_percrisk.csv")[,2:(1+cAssets)]
 # Summary stats
 EffFrontier_stats_MCC       = read.csv(file = "EffFrontierMinCVaRConc_stats_CC.csv")[,2:5]
 EffFrontier_stats_minCVaR   = read.csv(file = "EffFrontierMinCVar_stats_CC.csv")[,2:5]
 EffFrontier_stats_minVar = read.csv(file = "EffFrontierMinVar_stats.csv")[,2:5]
}

vmu_MCC             = EffFrontier_stats_MCC[,1] ;  vmu_minCVaR             = EffFrontier_stats_minCVaR[,1] ; vmu_minVar     = EffFrontier_stats_minVar[,1] ;
vrisk_MCC           = EffFrontier_stats_MCC[,2] ;  vrisk_minCVaR           = EffFrontier_stats_minCVaR[,2] ; vrisk_minVar   = EffFrontier_stats_minVar[,2] ;
vmaxpercrisk_MCC    = EffFrontier_stats_MCC[,3] ;  vmaxpercrisk_minCVaR    = EffFrontier_stats_minCVaR[,3] ;
vriskconc_MCC       = EffFrontier_stats_MCC[,4] ;  vriskconc_minCVaR       = EffFrontier_stats_minCVaR[,4] ; vriskconc_minVar  = EffFrontier_stats_minVar[,4] ;

vstdev_MCC = c(); 
for( r in 1:nrow(W_MCC) ){  wr = matrix( as.numeric(W_MCC[r,]) , ncol = 1 ) ; 
                            vstdev_MCC = c( vstdev_MCC , sqrt( t(wr)%*%sigma%*%wr ) )  }
vstdev_minVar = c(); 
for( r in 1:nrow(W_minVar) ){  wr = matrix( as.numeric(W_minVar[r,]) , ncol = 1 ) ; 
                            vstdev_minVar = c( vstdev_minVar , sqrt( t(wr)%*%sigma%*%wr ) )  }

vstdev_minCVaR = c(); 
for( r in 1:nrow(W_minCVaR) ){  wr = matrix( as.numeric(W_minCVaR[r,]) , ncol = 1 ) ; 
                            vstdev_minCVaR = c( vstdev_minCVaR , sqrt( t(wr)%*%sigma%*%wr ) )  }

# The MCC and Min CVaR were obtained with DEoptim. The solutions are not always optimal. Correct for this: 

order_MCC = sort(vmu_MCC,index.return=T)$ix      ; order_minCVaR = sort(vmu_minCVaR,index.return=T)$ix 
vmu_MCC = vmu_MCC[order_MCC]                     ; vmu_minCVaR = vmu_minCVaR[order_minCVaR] 
vrisk_MCC = vrisk_MCC[order_MCC]                 ; vrisk_minCVaR = vrisk_minCVaR[order_minCVaR]     
vstdev_MCC = vstdev_MCC[order_MCC]                 ; vstdev_minCVaR = vstdev_minCVaR[order_minCVaR]       
vmaxpercrisk_MCC = vmaxpercrisk_MCC[order_MCC]   ; vmaxpercrisk_minCVaR = vmaxpercrisk_MCC[order_minCVaR] 
vriskconc_MCC = vriskconc_MCC[order_MCC]         ; vriskconc_minCVaR = vriskconc_minCVaR[order_minCVaR] 
W_MCC  = W_MCC[order_MCC,]                       ; W_minCVaR  = W_minCVaR[order_minCVaR,]     
PercCVaR_MCC = PercCVaR_MCC[order_MCC,]          ; PercCVaR_minCVaR = PercCVaR_minCVaR[order_minCVaR,]   

# eliminate duplicates in mu
# Determines which elements of a vector or data frame are duplicates of elements with smaller subscripts, 
# and returns a logical vector indicating which elements (rows) are duplicates.
a_MCC = duplicated(vmu_MCC)                         ; a_minCVaR = duplicated(vmu_minCVaR)  
vmu_MCC = vmu_MCC[!a_MCC]                           ; vmu_minCVaR = vmu_minCVaR[!a_minCVaR] 
vrisk_MCC = vrisk_MCC[!a_MCC]                       ; vrisk_minCVaR = vrisk_minCVaR[!a_minCVaR]
vstdev_MCC = vstdev_MCC[!a_MCC]                       ; vstdev_minCVaR = vstdev_minCVaR[!a_minCVaR]
vmaxpercrisk_MCC = vmaxpercrisk_MCC[!a_MCC]         ; vmaxpercrisk_minCVaR = vmaxpercrisk_minCVaR[!a_minCVaR] 
vriskconc_MCC = vriskconc_MCC[!a_MCC]               ; vriskconc_minCVaR = vriskconc_minCVaR[!a_minCVaR]  
W_MCC = W_MCC[!a_MCC,]                              ; W_minCVaR = W_minCVaR[!a_minCVaR,]    
PercCVaR_MCC = PercCVaR_MCC[!a_MCC,]                ; PercCVaR_minCVaR = PercCVaR_minCVaR[!a_minCVaR,] 
 
# eliminate (efficiency: lower mu, higher CVaR alloc

sel_MCC = sel_minCVaR = c(); nmu_MCC = length(vmu_MCC); nmu_minCVaR = length(vmu_minCVaR); 
for( i in 1:(nmu_MCC-1) ){
    if( any(vriskconc_MCC[(i+1):nmu_MCC]<=vriskconc_MCC[i]) ){ }else{sel_MCC = c(sel_MCC, i) }
}
for( i in 1:(nmu_minCVaR-1) ){
    if( any(vrisk_minCVaR[(i+1):nmu_minCVaR]<=vrisk_minCVaR[i]) ){ }else{sel_minCVaR = c(sel_minCVaR, i) }
}
vmu_MCC = vmu_MCC[sel_MCC]                        ; vmu_minCVaR = vmu_minCVaR[sel_minCVaR]  
vrisk_MCC = vrisk_MCC[sel_MCC]                    ; vrisk_minCVaR = vrisk_minCVaR[sel_minCVaR] 
vstdev_MCC = vstdev_MCC[sel_MCC]                    ; vstdev_minCVaR = vstdev_minCVaR[sel_minCVaR] 
vmaxpercrisk_MCC = vmaxpercrisk_MCC[sel_MCC]      ; vmaxpercrisk_minCVaR = vmaxpercrisk_minCVaR[sel_minCVaR]
vriskconc_MCC = vriskconc_MCC[sel_MCC]            ; vriskconc_minCVaR = vriskconc_minCVaR[sel_minCVaR] 
W_MCC = W_MCC[sel_MCC,]                           ; W_minCVaR = W_minCVaR[sel_minCVaR,]
PercCVaR_MCC = PercCVaR_MCC[sel_MCC,]             ; PercCVaR_minCVaR = PercCVaR_minCVaR[sel_minCVaR,] 

wEW <- rep(1/4,4)
muEW       = mean(assetmu*12)
outES      = ES(weights=wEW, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE) 
riskEW     = outES$MES; riskconcEW = max(outES$contribution)

if(clean){
   postscript('frontier_fourassets_clean.eps')
}else{ postscript('frontier_fourassets.eps')  } 

 layout( matrix(  c(1,2,3,1,2,3),  ncol = 2 ) , height= c(5,5,1), width=c(1,1) )
 ylim = c( min(assetmu) , max(assetmu) )
 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vrisk_MCC , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized mean return" , xlab="Monthly 95% Portfolio CVaR" , 
          lwd=2, xlim = c(0.01,0.13) , ylim = (ylim*12+c(0,0.01)) , col="black" )
 lines( vrisk_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vrisk_minVar , vmu_minVar*12,  lty = 3, lwd=2, col="black" )

 c = 1; text(y=assetmu[c]*12,x= assetCVaR[c]-0.005  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3); points(y=assetmu[c]*12,x= assetCVaR[c] )  
 for( c in 2:cAssets ){ 
   text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3) 
   points(y=assetmu[c]*12,x= assetCVaR[c] ) 
 };
 # Plot also EW portfolio
 text(y=muEW,x=riskEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 3)
 points(y=muEW,x= riskEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vrisk_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_MCC[1]*12,x=vrisk_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1]-0.0035, label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12-0.001,x=vrisk_minVar[1]+0.0035, label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vrisk_minVar[1],  pch=22 ) 

 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vriskconc_MCC/vrisk_MCC , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized  mean return" , xlab="Largest Perc. CVaR contribution" , 
          lwd=2, xlim = c(0.2,1.1), ylim = c(ylim*12+c(0,0.01)) , col="black" )
 lines( vriskconc_minCVaR/vrisk_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vriskconc_minVar/vrisk_minVar   , vmu_minVar*12 ,  lty = 3, lwd=2, col="black" )

 text(y=muEW,x=riskconcEW/riskEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=muEW,x= riskconcEW/riskEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vriskconc_MCC[1]/vrisk_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_MCC[1]*12,x=vriskconc_MCC[1]/vrisk_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1]/vrisk_minCVaR[1], label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1]/vrisk_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12,x=vriskconc_minVar[1]/vrisk_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vriskconc_minVar[1]/vrisk_minVar[1],  pch=22 ) 

 for( c in 1:cAssets ){ 
   text(y=assetmu[c]*12,x= 1  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 4) 
   points(y=assetmu[c]*12,x= 1 ) 
 };
 
 par( mar = c(0,1,1,0) )
 plot.new()
  legend("center",legend=c("Mean-CVaR concentration", "Mean-CVaR","Mean-StdDev"  ),
   lty=c("solid","solid","dashed"), cex=0.8,ncol=3,lwd=c(4,4,2),col=c("black","darkgray","black"))

dev.off()

if(clean){
   postscript('frontier_fourassets_bis_clean.eps')
}else{ postscript('frontier_fourassets_bis.eps')  } 

 layout( matrix(  c(1,2,3,1,2,3),  ncol = 2 ) , height= c(5,5,1), width=c(1,1) )
 ylim = c( min(assetmu) , max(assetmu) )
 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vrisk_MCC , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized mean return" , xlab="Monthly 95% Portfolio CVaR" , 
          lwd=2, xlim = c(0,0.13) , ylim = (ylim*12+c(0,0.02)) , col="black" )
 lines( vrisk_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vrisk_minVar , vmu_minVar*12,  lty = 3, lwd=2, col="black" )

 c = 1; text(y=assetmu[c]*12,x= assetCVaR[c]-0.005  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3); points(y=assetmu[c]*12,x= assetCVaR[c] )  
 for( c in 2:cAssets ){ 
   text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3) 
   points(y=assetmu[c]*12,x= assetCVaR[c] ) 
 };
 # Plot also EW portfolio
 text(y=muEW,x=riskEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 3)
 points(y=muEW,x= riskEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vrisk_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_MCC[1]*12,x=vrisk_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1]-0.0035, label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12-0.0035,x=vrisk_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vrisk_minVar[1],  pch=22 ) 

 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)

 plot( vriskconc_MCC, vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized  mean return" , xlab="Largest CVaR contribution" , 
          lwd=2, xlim = c(0,0.13), ylim = c(ylim*12+c(0,0.02)) , col="black" )
 lines( vriskconc_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vriskconc_minVar   , vmu_minVar*12 ,  lty = 3, lwd=2, col="black" )

 text(y=muEW,x=riskconcEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=muEW,x= riskconcEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vriskconc_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 2)
 points(y=vmu_MCC[1]*12,x=vriskconc_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1], label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12-0.003,x=vriskconc_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vriskconc_minVar[1],  pch=22 ) 

 #for( c in 1:cAssets ){ 
 #  text(y=assetmu[c]*12,x= 1  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 4) 
 #  points(y=assetmu[c]*12,x= 1 ) 
 #};
 
 par( mar = c(0,1,1,0) )
 plot.new()
  legend("center",legend=c("Mean-CVaR concentration", "Mean-CVaR","Mean-StdDev"  ),
   lty=c("solid","solid","dashed"), cex=0.8,ncol=3,lwd=c(4,4,2),col=c("black","darkgray","black"))

dev.off()



if(clean){
   postscript('frontier_fourassets_bis_clean.eps')
}else{ postscript('frontier_fourassets_bis.eps')  } 

 layout( matrix(  c(1,2,3,1,2,3),  ncol = 2 ) , height= c(5,5,1), width=c(1,1) )
 ylim = c( min(assetmu) , max(assetmu) )
 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vrisk_MCC , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized mean return" , xlab="Monthly 95% Portfolio CVaR" , 
          lwd=2, xlim = c(0,0.13) , ylim = (ylim*12+c(0,0.02)) , col="black" )
 lines( vrisk_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vrisk_minVar , vmu_minVar*12,  lty = 3, lwd=2, col="black" )

 c = 1; text(y=assetmu[c]*12,x= assetCVaR[c]-0.005  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3); 
 points(y=assetmu[c]*12,x= assetCVaR[c] )  
 for( c in 2:cAssets ){ 
   text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3) 
   points(y=assetmu[c]*12,x= assetCVaR[c] ) 
 };
 # Plot also EW portfolio
 text(y=muEW,x=riskEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 3)
 points(y=muEW,x= riskEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vrisk_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_MCC[1]*12,x=vrisk_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1]-0.0035, label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12-0.0035,x=vrisk_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vrisk_minVar[1],  pch=22 ) 

 par( mar = c(4.5,5,2,2), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)

 plot( vriskconc_MCC, vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized  mean return" , xlab="Largest CVaR contribution" , 
          lwd=2, xlim = c(0,0.13), ylim = c(ylim*12+c(0,0.02)) , col="black" )
 lines( vriskconc_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=2, col="darkgray" ) 
 lines( vriskconc_minVar   , vmu_minVar*12 ,  lty = 3, lwd=2, col="black" )

 text(y=muEW,x=riskconcEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=muEW,x= riskconcEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vriskconc_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 2)
 points(y=vmu_MCC[1]*12,x=vriskconc_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1], label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12-0.003,x=vriskconc_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minVar[1]*12,x=vriskconc_minVar[1],  pch=22 ) 

 #for( c in 1:cAssets ){ 
 #  text(y=assetmu[c]*12,x= 1  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 4) 
 #  points(y=assetmu[c]*12,x= 1 ) 
 #};
 
 par( mar = c(0,1,1,0) )
 plot.new()
  legend("center",legend=c("Mean-CVaR concentration", "Mean-CVaR","Mean-StdDev"  ),
   lty=c("solid","solid","dashed"), cex=0.8,ncol=3,lwd=c(4,4,2),col=c("black","darkgray","black"))

dev.off()

assetStdDev = sqrt(diag(sigma))
StdDevEW = sqrt(matrix( rep(1/4,4) , ncol = 4 )%*%sigma%*%matrix( rep(1/4,4) , ncol = 1 ))
if(clean){
   postscript('frontier_fourassets_tris_clean.eps')
}else{ postscript('frontier_fourassets_tris.eps')  } 

 layout( matrix(  c(1,2,3,4,1,2,3,4),  ncol = 2 , nrow=4 ) , height= c(5,5,5,1), width=c(1,1) )

 layout( matrix(  c(1,2,3,4),  ncol = 2 , nrow=2 ) , height= c(5,5), width=c(1,1) )

 ylim = c( min(assetmu) , max(assetmu) )
 par( mar = c(4.5,5,2,0.1), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vstdev_MCC*sqrt(12) , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized mean return" , xlab="Annualized Portfolio StdDev" , 
          lwd=2, xlim = c(-0.01,0.2) , ylim = (ylim*12+c(0,0.01)) , col="black" )
 lines( vstdev_minCVaR*sqrt(12) , vmu_minCVaR*12,  lty = 1, lwd=1, col="darkgray" ) 
 lines( vstdev_minVar*sqrt(12) , vmu_minVar*12,  lty = 3, lwd=2, col="black" )

 c = 1; text(y=assetmu[c]*12+0.0005,x= assetStdDev[c]*sqrt(12)  , label=labelnames[c]  ,
       cex = 0.7, offset = 0.2, pos = 4); 
 points(y=assetmu[c]*12,x= assetStdDev[c]*sqrt(12) )  
 for( c in 2:cAssets ){ 
   text(y=assetmu[c]*12,x= assetStdDev[c]*sqrt(12)  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3) 
   points(y=assetmu[c]*12,x= assetStdDev[c]*sqrt(12) ) 
 };
 # Plot also EW portfolio
 text(y=muEW,x=StdDevEW*sqrt(12), label="EW"  , cex = 0.7, offset = 0.2, pos = 3)
 points(y=muEW,x= StdDevEW*sqrt(12) , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vstdev_MCC[1]*sqrt(12), label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_MCC[1]*12,x=vstdev_MCC[1]*sqrt(12),  pch=22 ) 
 text(y=vmu_minCVaR[1]*12-0.0035,x=vstdev_minCVaR[1]*sqrt(12)-0.0035, label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_minCVaR[1]*12,x=vstdev_minCVaR[1]*sqrt(12),  pch=22 ) 
 text(y=vmu_minVar[1]*12+0.0035,x=vstdev_minVar[1]*sqrt(12), label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 2)
 points(y=vmu_minVar[1]*12,x=vstdev_minVar[1]*sqrt(12),  pch=22 ) 

 # Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right 




 par( mar = c(0,1,1,0.1) )
 plot.new()
  legend("center",legend=c("Mean-CVaR concentration", "Mean-CVaR","Mean-StdDev"  ),
   lty=c("solid","solid","dashed"), cex=0.8,ncol=1,lwd=c(4,2,2),col=c("black","darkgray","black"))


 ylim = c( min(assetmu) , max(assetmu) )
 par( mar = c(4.5,5,2,0.1), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
 plot( vrisk_MCC , vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized mean return" , xlab="Monthly 95% Portfolio CVaR" , 
          lwd=2, xlim = c(-0.01,0.13) , ylim = (ylim*12+c(0,0.01)) , col="black" )
 lines( vrisk_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=1, col="darkgray" ) 
 lines( vrisk_minVar , vmu_minVar*12,  lty = 3, lwd=2, col="black" )

 c = 1; text(y=assetmu[c]*12,x= assetCVaR[c]-0.005  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3); points(y=assetmu[c]*12,x= assetCVaR[c] )  
 for( c in 2:cAssets ){ 
   text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 3) 
   points(y=assetmu[c]*12,x= assetCVaR[c] ) 
 };
 # Plot also EW portfolio
 text(y=muEW,x=riskEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 3)
 points(y=muEW,x= riskEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vrisk_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_MCC[1]*12,x=vrisk_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1]-0.0035, label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=vmu_minCVaR[1]*12,x=vrisk_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12,x=vrisk_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_minVar[1]*12,x=vrisk_minVar[1],  pch=22 ) 



 par( mar = c(4.5,5,2,0.1), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)

 plot( vriskconc_MCC, vmu_MCC*12 , type="l", lty = 1, main = "" , 
          ylab="Annualized  mean return" , xlab="Largest contribution to monthly 95% CVaR" , 
          lwd=2, xlim = c(-0.01,0.13), ylim = c(ylim*12+c(0,0.01)) , col="black" )
 lines( vriskconc_minCVaR , vmu_minCVaR*12,  lty = 1, lwd=1, col="darkgray" ) 
 lines( vriskconc_minVar   , vmu_minVar*12 ,  lty = 3, lwd=2, col="black" )

 text(y=muEW,x=riskconcEW, label="EW"  , cex = 0.7, offset = 0.2, pos = 1)
 points(y=muEW,x= riskconcEW , pch=22 ) 
 text(y=vmu_MCC[1]*12,x=vriskconc_MCC[1], label="MCC=ERC"  , cex = 0.7, offset = 0.2, pos = 2)
 points(y=vmu_MCC[1]*12,x=vriskconc_MCC[1],  pch=22 ) 
 text(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1], label="Min CVaR"  , cex = 0.7, offset = 0.2, pos = 4)
 points(y=vmu_minCVaR[1]*12,x=vriskconc_minCVaR[1],  pch=22 ) 
 text(y=vmu_minVar[1]*12,x=vriskconc_minVar[1], label="Min StdDev"  , cex = 0.7, offset = 0.2, pos = 2)
 points(y=vmu_minVar[1]*12,x=vriskconc_minVar[1],  pch=22 ) 



 #for( c in 1:cAssets ){ 
 #  text(y=assetmu[c]*12,x= 1  , label=labelnames[c]  , cex = 0.7, offset = 0.2, pos = 4) 
 #  points(y=assetmu[c]*12,x= 1 ) 
 #};
 


dev.off()


# Make the weight/CVaR allocation plots
#---------------------------------------

Wsel_MCC = PercCVaRsel_MCC  = Wsel_minCVaR = PercCVaRsel_minCVaR  = Wsel_minVar = PercCVaRsel_minVar  = c(); 
vmusel_MCC = vrisksel_MCC = vriskconcsel_MCC = vmusel_minCVaR = vrisksel_minCVaR = vriskconcsel_minCVaR =  vmusel_minVar = vrisksel_minVar = vriskconcsel_minVar =   c();
lm = minmu = min( c(vmu_MCC,vmu_minCVaR) ) ; maxmu = max( c(vmu_MCC,vmu_minCVaR) );
ylim = c( min(assetmu) - 0.0003 , max(assetmu) + 0.0003 )
xlim = c( 0 , max(assetCVaR) + 0.01 )

binning = T # for the weight plots, binned data such that no visual misinterpretation is possible
step = quantile( diff(vmu_MCC) , 0.1 ) 
if( binning ){
   for( rm in seq( minmu+step , maxmu , step )  ){
      selection    =  c(vmu_MCC >= lm & vmu_MCC < rm) ;
      if( any(selection) ){
         selection    = c(1:length(selection))[selection]
         selone       = sort(vriskconc_MCC[ selection ],index.return=T)$ix[1]
         selone       = selection[selone]
         vmusel_MCC       = c( vmusel_MCC       ,  mean(vmu_MCC[selone  ] ));
         Wsel_MCC         = rbind( Wsel_MCC     ,  apply( W_MCC[selone,] ,2,'mean' ))
         PercCVaRsel_MCC  = rbind( PercCVaRsel_MCC  ,  apply( PercCVaR_MCC[selone,] ,2,'mean' )) 
         vrisksel_MCC     = c( vrisksel_MCC     ,  mean(vrisk_MCC[ selone ]) );
         vriskconcsel_MCC = c( vriskconcsel_MCC ,  mean(vriskconc_MCC[ selone ])  )
      }else{
         vmusel_MCC       = c( vmusel_MCC       ,  NA );
         Wsel_MCC         = rbind( Wsel_MCC     ,  rep(NA,cAssets)  )
         PercCVaRsel_MCC  = rbind( PercCVaRsel_MCC  ,  rep(NA,cAssets) ) 
         vrisksel_MCC     = c( vrisksel_MCC     ,  NA );
         vriskconcsel_MCC = c( vriskconcsel_MCC ,  NA  )
      }
      selection    =  c(vmu_minCVaR >= lm & vmu_minCVaR < rm) ;
      if( any(selection) ){
         selection    = c(1:length(selection))[selection]; selone       = sort(vrisk_minCVaR[ selection ],index.return=T)$ix[1]
         selone       = selection[selone]
         vmusel_minCVaR       = c( vmusel_minCVaR       ,  mean(vmu_minCVaR[selone  ] ));
         Wsel_minCVaR         = rbind( Wsel_minCVaR     ,  apply( W_minCVaR[selone,] ,2,'mean' ))
         PercCVaRsel_minCVaR  = rbind( PercCVaRsel_minCVaR  ,  apply( PercCVaR_minCVaR[selone,] ,2,'mean' )) 
         vrisksel_minCVaR     = c( vrisksel_minCVaR     ,  mean(vrisk_minCVaR[ selone ]) );
         vriskconcsel_minCVaR = c( vriskconcsel_minCVaR ,  mean(vriskconc_minCVaR[ selone ])  )
      }else{
         vmusel_minCVaR       = c( vmusel_minCVaR       ,  NA );
         Wsel_minCVaR         = rbind( Wsel_minCVaR     ,  rep(NA,cAssets) )
         PercCVaRsel_minCVaR  = rbind( PercCVaRsel_minCVaR  ,  rep(NA,cAssets) ) 
         vrisksel_minCVaR     = c( vrisksel_minCVaR     ,  NA );
         vriskconcsel_minCVaR = c( vriskconcsel_minCVaR ,  NA )
      }
      selection    =  c(vmu_minVar >= lm & vmu_minVar < rm) ;
      if( any(selection) ){
         selection    = c(1:length(selection))[selection]; selone       = sort(vrisk_minVar[ selection ],index.return=T)$ix[1]
         selone       = selection[selone]
         vmusel_minVar       = c( vmusel_minVar       ,  mean(vmu_minVar[selone  ] ));
         Wsel_minVar         = rbind( Wsel_minVar     ,  apply( W_minVar[selone,] ,2,'mean' ))
         PercCVaRsel_minVar  = rbind( PercCVaRsel_minVar  ,  apply( PercCVaR_minVar[selone,] ,2,'mean' )) 
         vrisksel_minVar     = c( vrisksel_minVar     ,  mean(vrisk_minVar[ selone ]) );
         vriskconcsel_minVar = c( vriskconcsel_minVar ,  mean(vriskconc_minVar[ selone ])  )
      }else{
         vmusel_minVar       = c( vmusel_minVar       ,  NA );
         Wsel_minVar         = rbind( Wsel_minVar     ,  rep(NA,cAssets) )
         PercCVaRsel_minVar  = rbind( PercCVaRsel_minVar  ,  rep(NA,cAssets) ) 
         vrisksel_minVar     = c( vrisksel_minVar     ,  NA );
         vriskconcsel_minVar = c( vriskconcsel_minVar ,  NA  )
      }
      lm = rm; 
   }
}else{
   vmusel_MCC = vmu_MCC              ;   vmusel_minCVaR = vmu_minCVaR 
   Wsel_MCC   = W_MCC                ;   Wsel_minCVaR   = W_minCVaR                ;
   vrisksel_MCC = vrisk_MCC          ;   vrisksel_minCVaR = vrisk_minCVaR  
   vriskconcsel_MCC = vriskconc_MCC  ;   vriskconcsel_minCVar = vriskconc_minCVaR
   PercCVaRsel_MCC =  PercCVaR_MCC   ;   PercCVaRsel_minCVar =  PercCVaR_minCVar       ; 
}

library(zoo)
cAssets = ncol(monthlyR)
 colorset = sort(gray( 1-(seq(1,cAssets,1)/(cAssets))^1.5 )) ; 
colnames( Wsel_MCC ) = colnames( Wsel_minCVaR ) = labelnames
rownames( Wsel_MCC ) = rownames( PercCVaRsel_MCC ) = round( interpNA(vmusel_MCC)*12,4); 
rownames( Wsel_minCVaR ) = rownames( PercCVaRsel_minCVaR ) = round( interpNA(vmusel_minCVaR)*12,4) ;

rownames( Wsel_minVar ) = rownames( PercCVaRsel_minVar ) = round( interpNA(vmusel_minVar)*12,4) ; 
colnames( Wsel_minVar ) = labelnames


w.names = c( "US bond" , "S&P 500", "NAREIT"  , "GSCI" )
namelabels = c("Mean-StdDev" , "Mean-CVaR","Mean-CVaR concentration"  )
l = 2 ; mar1 =c(2,l,2,1.1) ; mar3 = c(3,l+1,3,0.1)

source("chart.StackedBar.R"); 

# Stacked weights plot: 
if(clean){
   postscript('stackedweightsriskcont_efficientfrontier_withNA_clean.eps')
}else{ postscript('stackedweightsriskcont_efficientfrontier_withNA.eps')  } 

   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(Wsel_minVar    ,col=colorset,space=0,  main = namelabels[1], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(Wsel_minCVaR,col=colorset,space=0,  main = namelabels[2], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(Wsel_MCC    ,col=colorset,space=0,  main = namelabels[3], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,fill=colorset,ncol=4)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(PercCVaRsel_minVar,col=colorset,space=0,  main = namelabels[1], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(PercCVaRsel_minCVaR,col=colorset,space=0,  main = namelabels[2], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(PercCVaRsel_MCC,col=colorset,space=0,  main = namelabels[3], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()


if(clean){
   postscript('stackedweightsriskcont_efficientfrontier_clean.eps')
}else{ postscript('stackedweightsriskcont_efficientfrontier.eps')  } 

   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(interpNA(Wsel_minVar)    ,col=colorset,space=0,  main = namelabels[1], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(interpNA(Wsel_minCVaR),col=colorset,space=0,  main = namelabels[2], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(interpNA(Wsel_MCC)    ,col=colorset,space=0,  main = namelabels[3], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,col=colorset,ncol=4,lwd=6)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(interpNA(PercCVaRsel_minVar),col=colorset,space=0,  main = namelabels[1], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(interpNA(PercCVaRsel_minCVaR),col=colorset,space=0,  main = namelabels[2], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(interpNA(PercCVaRsel_MCC),col=colorset,space=0,  main = namelabels[3], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()



#----------------------------------------------------------------------------------------------------
# DISCUSSION RESULTS


# unconstrained:
12*vmu_minVar[1] ; sqrt(12)*vstdev_minVar[1] ; vrisk_minVar[1] ; W_minVar[1,] ; PercCVaR_minVar[1,]; 
12*vmu_minCVaR[1]; sqrt(12)*vstdev_minCVaR[1] ;  vrisk_minCVaR[1]; W_minCVaR[1,]; PercCVaR_minCVaR[1,]; 
12*vmu_MCC[1]    ; sqrt(12)*vstdev_MCC[1] ;  vrisk_MCC[1]    ; W_MCC[1,]    ; PercCVaR_MCC[1,]; 

> # unconstrained:
> 12*vmu_minVar[1] ; sqrt(12)*vstdev_minVar[1] ; vrisk_minVar[1] ; W_minVar[1,] ; PercCVaR_minVar[1,]; 
[1] 0.07705461
[1] 0.04967331
[1] 0.0277955
         V1         V2         V3         V4
1 0.8404353 0.03862133 0.03643427 0.08450912
         V1         V2         V3        V4
1 0.5620443 0.08734358 0.08376784 0.2668442
> 12*vmu_minCVaR[1]; sqrt(12)*vstdev_minCVaR[1] ;  vrisk_minCVaR[1]; W_minCVaR[1,]; PercCVaR_minCVaR[1,]; 
[1] 0.075321
[1] 0.05230409
[1] 0.0228343
         V1 V2 V3         V4
2 0.9771176  0  0 0.02288243
         V1 V2 V3         V4
2 0.9771177  0  0 0.02288231
> 12*vmu_MCC[1]    ; sqrt(12)*vstdev_MCC[1] ;  vrisk_MCC[1]    ; W_MCC[1,]    ; PercCVaR_MCC[1,]; 
[1] 0.08714249
[1] 0.06588349
[1] 0.038659
         V1        V2        V3        V4
1 0.4733987 0.2070037 0.1843733 0.1352243
       V1        V2        V3        V4
1 0.25064 0.2505296 0.2497479 0.2490826



cbind( vmu_minVar*12 , vriskconc_minVar ) :

cbind( vmu_minCVaR*12 , vriskconc_minCVaR ) : 0.0816 is turning point
# [124,] 0.09017165        0.01210119 is turning point

cbind( vmu_MCC*12 , vrisk_MCC, vriskconc_MCC ) 

# Mean/CVaR concentration efficient portfolios

# On Cleaned Data: Three segments

> cbind( vmu_MCC*12 , vrisk_MCC, vriskconc_MCC )
  [1,] 0.08714249 0.03865900   0.009689492
  [2,] 0.08726420 0.03849615   0.009802639
#24
 [23,] 0.08997886 0.03585947   0.011824072
 [24,] 0.09010885 0.03585032   0.011871467
 [25,] 0.09024551 0.03589501   0.011974648
 [26,] 0.09036701 0.03594272   0.012064923
 [27,] 0.09050353 0.03610158   0.012242378
 [28,] 0.09062808 0.03621858   0.012405637
 [29,] 0.09075394 0.03635143   0.012544703
 [30,] 0.09087482 0.03674130   0.012857458
# 174
[170,] 0.10996933 0.06748652   0.039200481
[171,] 0.11008934 0.06789785   0.042263237
[172,] 0.11020953 0.06842979   0.045351289
[173,] 0.11033011 0.06909579   0.048542053
[174,] 0.11045013 0.06985814   0.051639874
[175,] 0.11057105 0.07075940   0.054836023
[176,] 0.11069142 0.07176853   0.057990740

[193,] 0.11273404 0.10433383   0.104615672
[194,] 0.11273413 0.10433618   0.104617643

> W_MCC
              V1         V2        V3            V4
1   4.733987e-01 0.20700375 0.1843733  1.352243e-01
3   4.759377e-01 0.20759359 0.1855799  1.308888e-01
#24
23  5.561089e-01 0.22228044 0.2020208  1.958989e-02
24  5.614145e-01 0.22306274 0.2027785  1.274428e-02
25  5.651643e-01 0.22387831 0.2036273  7.330121e-03
26  5.664972e-01 0.22511059 0.2048818  3.510355e-03
27  5.674492e-01 0.22627029 0.2060026  2.778562e-04
28  5.623908e-01 0.22852300 0.2080519  1.034299e-03
29  5.595434e-01 0.23008873 0.2098842  4.836782e-04

# 174
170 1.444844e-02 0.51421725 0.4710510  2.833091e-04
171 1.074035e-02 0.51609949 0.4727863  3.738407e-04
172 7.379454e-03 0.51796198 0.4745030  1.555284e-04
173 3.671705e-03 0.51988524 0.4762691  1.739490e-04
174 2.930070e-04 0.52170814 0.4779540  4.485562e-05
175 2.228161e-05 0.50313662 0.4968072  3.392269e-05
176 9.137182e-07 0.48289134 0.5170837  2.405561e-05

199 1.461185e-06 0.01379234 0.9862059  2.992504e-07
200 8.716672e-07 0.01377375 0.9862242  1.214785e-06

# Unconstrained: Equal Risk Contribution Portfolio:
# > W_MCC[1,]
# 0.4733987 0.2070037 0.1843733 0.1352243
#> PercCVaR_MCC[1,]
#  0.25064 0.2505296 0.2497479 0.2490826
# > vmu_MCC[1]*12
# [1] 0.08714249
#> vrisk_MCC[1]
#[1] 0.038659
#> vriskconc_MCC[1]
#[1]  0.009689492
# Segment 1: increasing allocation to bonds and decreasing allocation to commodities
# Portfolio risk concentration increases, portfolio risk decreases
# > W_MCC[25:27,]
# 0.7052788 0.1584357 0.1321813 0.0041042550
# 0.7085301 0.1597522 0.1313759 0.0003417814
# 0.7025028 0.1628656 0.1344935 0.0001380022
#> vmu_MCC[26]*12
#[1] 0.0812571
#> vrisk_MCC[26]
#[1] 0.03352778
#> vriskconc_MCC[26]
#[1] 0.0112
# Segment 2: increasing allocation to both US equity and EAFE
# Portfolio risk concentration and risk increase together
# > W_MCC[138:140,]
# 0.0078323664 0.5106416 0.4814090 1.169711e-04
# 0.0005986683 0.5136429 0.4856355 1.228907e-04
# 0.0002258733 0.5201106 0.4796231 4.051641e-05
# > PercCVaR_MCC[138:140,]
#  0e+00 0.4999 0.5000  0
#> vmu_MCC[139]*12
#[1] 0.09483605
# > vrisk_MCC[139]
# [1] 0.09808784
# > vriskconc_MCC[139]
# [1] 0.049
# Segment 3: increasing alllocation to US equity and decreasing to EAFE
# Portfolio risk concentration and risk increase together
# > tail(W_MCC,1)
# 5.45402e-05 0.9998762 4.340072e-05 2.585630e-05


