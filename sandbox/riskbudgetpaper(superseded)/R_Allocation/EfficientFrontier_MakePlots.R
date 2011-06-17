# Paper: Portfolio Optimization with Conditional Value-at-Risk Budgets
# Boudt, Carl, Peterson (2010)
# This R script serves to make the exhibits regarding the four asset efficient frontier

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

cAssets = 4; p = priskbudget = 0.95;
mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("PortfolioAnalytics")
clean = TRUE

#   Load the data
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste("data/","/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

monthlyR = zoo( data[,2:(1+cAssets)] , order.by = date )
if(clean){ monthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]] }

mu = apply(monthlyR,2,'mean')
sigma = cov(monthlyR)
M3 = PerformanceAnalytics:::M3.MM(monthlyR) 
M4 = PerformanceAnalytics:::M4.MM(monthlyR) 

# Summary stats individual assets
apply(monthlyR,2,'mean')*12
apply(monthlyR,2,'sd')
apply(monthlyR,2,'skewness')
apply(monthlyR,2,'kurtosis')
ES(monthlyR[,1],method="modified"); ES(monthlyR[,2],method="modified")
ES(monthlyR[,3],method="modified"); ES(monthlyR[,4],method="modified")

#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################

# Layout 3 
source("R_interpretation/chart.StackedBar.R"); 

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )
minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);

# Load the data

labelnames = c( "US bond" , "S&P 500" , "EAFE" , "GSCI" )

clean = TRUE
if(clean){
 # Portfolio weights
 W_MCC                 = read.csv( file = "EffFrontierMinCVaRConc_weights_clean.csv" )[,2:(1+cAssets)]
 W_minCVaR             = read.csv( file = "EffFrontierMinCVaR_weights_clean.csv" )[,2:(1+cAssets)]
 W_minVar              = read.csv( file = "EffFrontierMinVar_weights_clean.csv" )[,2:(1+cAssets)]
 # Percentage CVaR contributions
 PercCVaR_MCC         = read.csv(  file = "EffFrontierMinCVaRConc_percrisk_clean.csv" )[,2:(1+cAssets)]
 PercCVaR_minCVaR     = read.csv(  file = "EffFrontierMinCVaR_percrisk_clean.csv" )[,2:(1+cAssets)]
 PercCVaR_minVar      = read.csv(  file = "EffFrontierMinVar_percrisk_clean.csv" )[,2:(1+cAssets)]
 # Summary stats
 EffFrontier_stats_MCC = read.csv(file = "EffFrontierMinCVaRConc_stats_clean.csv")[,2:5]
 EffFrontier_stats_minVar = read.csv(file = "EffFrontierMinVar_stats_clean.csv")[,2:5]
 EffFrontier_stats_minCVaR = read.csv(file = "EffFrontierMinCVar_stats_clean.csv")[,2:5]
}else{
 # Portfolio weights
 W_MCC                 = read.csv(file = "EffFrontierMinCVaRConc_weights.csv")[,2:(1+cAssets)]
 W_minCVaR             = read.csv(file = "EffFrontierMinCVaR_weights.csv")[,2:(1+cAssets)]
 W_minVar              = read.csv(file = "EffFrontierMinVar_weights.csv")[,2:(1+cAssets)]
 # Percentage CVaR contributions
 mPercrisk_MCC         = read.csv( file = "EffFrontierMinCVaRConc_percrisk.csv" )[,2:(1+cAssets)]
 mPercrisk_minCVaR     = read.csv( file = "EffFrontierMinCVaR_percrisk.csv" )[,2:(1+cAssets)]
 mPercrisk_minVar      = read.csv(file = "EffFrontierMinVar_percrisk.csv")[,2:(1+cAssets)]
 # Summary stats
 EffFrontier_stats_MCC       = read.csv(file = "EffFrontierMinCVaRConc_stats.csv")[,2:5]
 EffFrontier_stats_minVar = read.csv(file = "EffFrontierMinVar_stats.csv")[,2:5]
 EffFrontier_stats_minCVaR   = read.csv(file = "EffFrontierMinCVar_stats.csv")[,2:5]
}

vmu_MCC             = EffFrontier_stats_MCC[,1] ;  vmu_minCVaR             = EffFrontier_stats_minCVaR[,1] ; vmu_minVar     = EffFrontier_stats_minVar[,1] ;
vrisk_MCC           = EffFrontier_stats_MCC[,2] ;  vrisk_minCVaR           = EffFrontier_stats_minCVaR[,2] ; vrisk_minVar   = EffFrontier_stats_minVar[,2] ;
vmaxpercrisk_MCC    = EffFrontier_stats_MCC[,3] ;  vmaxpercrisk_minCVaR    = EffFrontier_stats_minCVaR[,3] ;
vriskconc_MCC       = EffFrontier_stats_MCC[,4] ;  vriskconc_minCVaR       = EffFrontier_stats_minCVaR[,4] ; vriskconc_minVar  = EffFrontier_stats_minVar[,4] ;

# The MCC and Min CVaR were obtained with DEoptim. The solutions are not always optimal. Correct for this: 

order_MCC = sort(vmu_MCC,index.return=T)$ix      ; order_minCVaR = sort(vmu_minCVaR,index.return=T)$ix 
vmu_MCC = vmu_MCC[order_MCC]                     ; vmu_minCVaR = vmu_minCVaR[order_minCVaR] 
vrisk_MCC = vrisk_MCC[order_MCC]                 ; vrisk_minCVaR = vrisk_minCVaR[order_minCVaR]      
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
vmaxpercrisk_MCC = vmaxpercrisk_MCC[sel_MCC]      ; vmaxpercrisk_minCVaR = vmaxpercrisk_minCVaR[sel_minCVaR]
vriskconc_MCC = vriskconc_MCC[sel_MCC]            ; vriskconc_minCVaR = vriskconc_minCVaR[sel_minCVaR] 
W_MCC = W_MCC[sel_MCC,]                           ; W_minCVaR = W_minCVaR[sel_minCVaR,]
PercCVaR_MCC = PercCVaR_MCC[sel_MCC,]             ; PercCVaR_minCVaR = PercCVaR_minCVaR[sel_minCVaR,] 

wEW <- rep(1/4,4)
muEW       = mean(assetmu*12)
outES      = ES(weights=wEW, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE) 
riskEW     = outES$MES; riskconcEW = max(outES$contribution)

postscript('frontier_fourassets.eps') 

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
 colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; 
colnames( Wsel_MCC ) = colnames( Wsel_minCVaR ) = labelnames
rownames( Wsel_MCC ) = rownames( PercCVaRsel_MCC ) = round( interpNA(vmusel_MCC)*12,4); 
rownames( Wsel_minCVaR ) = rownames( PercCVaRsel_minCVaR ) = round( interpNA(vmusel_minCVaR)*12,4) ;

rownames( Wsel_minVar ) = rownames( PercCVaRsel_minVar ) = round( interpNA(vmusel_minVar)*12,4) ; 
colnames( Wsel_minVar ) = labelnames


w.names = c( "US bond" , "S&P 500", "EAFE"  , "GSCI" )
namelabels = c("Mean-StdDev" , "Mean-CVaR","Mean-CVaR concentration"  )
l = 2 ; mar1 =c(2,l,2,1.1) ; mar3 = c(3,l+1,3,0.1)

source("R_interpretation/chart.StackedBar.R"); 

# Stacked weights plot: 
postscript('stackedweightsriskcont_efficientfrontier_withNA.eps') 
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


postscript('stackedweightsriskcont_efficientfrontier.eps') 
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
12*vmu_minVar[1] ; vrisk_minVar[1] ; W_minVar[1,] ; PercCVaR_minVar[1,]; 
12*vmu_minCVaR[1]; vrisk_minCVaR[1]; W_minCVaR[1,]; PercCVaR_minCVaR[1,]; 
12*vmu_MCC[1]    ; vrisk_MCC[1]    ; W_MCC[1,]    ; PercCVaR_MCC[1,]; 


cbind( vmu_minVar*12 , vriskconc_minVar/vrisk_minVar ) : 0.0816 is turning point
cbind( vmu_minCVaR*12 , vriskconc_minCVaR/vrisk_minCVaR ) : 0.0816 is turning point

# Mean/CVaR concentration efficient portfolios

# On Cleaned Data: Three segments

# > assetmu
#      Bond      SP500       EAFE     SPGSCI 
# 0.07545960 0.10249338 0.08677129 0.05413622 
# > assetCVaR
#      Bond      SP500       EAFE     SPGSCI 
# 0.02456233 0.09974267 0.10868066 0.12775266 


# Unconstrained: Equal Risk Contribution Portfolio:
# > W_MCC[1,]
# 0.6431897 0.1337981 0.1044994 0.1185127
#> PercCVaR_MCC[1,]
# 0.2516 0.2517 0.2504 0.2463
# > vmu_MCC[1]*12
# [1] 0.07773165
#> vrisk_MCC[1]
#[1] 0.03467514
#> vriskconc_MCC[1]
#[1] 0.0087
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


