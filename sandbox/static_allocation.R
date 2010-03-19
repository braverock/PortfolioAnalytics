# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Equal risk portfolio
cAssets = 4;
p = priskbudget = 0.95;

mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2009; lastquarter = 4; 

data = read.table( file= paste("data/","/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
data = zoo( data[,2:ncol(data)] , order.by = date )

# "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 

cAssets = ncol(data)-2; # number of risky assets

# Summary statistics
histVaR = function( series ){ return(-quantile(series,probs=0.05) ) }
histCVaR = function( series ){ series = as.numeric(series) ; q = as.numeric(histVaR(series)) ; return( -mean( series[series<(-q)] )) }
mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }

summarystats = F;
if( summarystats ){
   monthlyR = data[,(1:(cAssets+1))]
   apply( monthlyR , 2 , 'mean' )                    ;  apply( monthlyR , 2 , 'sd' )
   apply( as.data.frame(monthlyR) , 2 , 'skewness' ) ;  apply( as.data.frame(monthlyR) , 2 , 'kurtosis' )
   cor(monthlyR)
   apply( monthlyR , 2 , 'histVaR')
   apply( monthlyR , 2 , 'histCVaR')
   apply( monthlyR , 2 , 'mESfun')
}
  
# Estimation uses real returns
monthlyR = data[,(1:(cAssets+1))]-data[,cAssets+2]
monthlyR = monthlyR[,1:cAssets] 
muinflation = mean(data[,cAssets+2] )
plot(monthlyR)

if( summarystats ){
   apply( monthlyR , 2 , 'mean' ) ; apply( monthlyR , 2 , 'sd' )
   apply( as.data.frame(monthlyR) , 2 , 'skewness' )
   apply( as.data.frame(monthlyR) , 2 , 'kurtosis' )
   cor(monthlyR)
   apply( monthlyR , 2 , 'histVaR')
   apply( monthlyR , 2 , 'histCVaR')
}

# Construct efficient frontier: given a return target minimize the largest percentage CVaR in the portfolio

mu = apply( monthlyR , 2 , 'mean' )
minmu = min(mu); maxmu = max(mu); print(minmu); print(maxmu);
# unconstrained solution is equal risk portfolio:
# sol = MinMaxCompCVaRconportfolio(R=monthlyR, Riskupper = Inf ,Returnlower= -Inf )
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= -Inf) 

W               = as.vector( sol[[1]] ) 
vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) 
vmaxpercrisk    = max( sol[[4]] ) 
vmaxriskcontrib = max( sol[[5]] ) 

# mutarget = 0.0047478;

for( mutarget in seq(minmu+0.0001,maxmu,0.0001) ){
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= mutarget) 
   W = rbind( W, as.vector( sol[[1]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}


write.csv( W , file = "EffFrontierMinCVaRConc _weights.csv" )
EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats.csv" )


sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= -Inf) 

W               = as.vector( sol[[1]] ) 
vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) 
vmaxpercrisk    = max( sol[[4]] ) 
vmaxriskcontrib = max( sol[[5]] ) 

for( mutarget in seq(minmu+0.0001,maxmu,0.0001) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= mutarget) 
   W = rbind( W, as.vector( sol[[1]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}


write.csv( W , file = "EffFrontierMinCVaR _weights.csv" )
EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats.csv" )


# Plotting 

postscript('frontier.eps') 
layout( matrix(  c(1,2,3,4,5,3),  ncol = 2 ) , height= c(4,4,0.4), width=1)


################################
# 1e kolom: klassiek Min CVaR
################################

W = read.csv(file = "EffFrontierMinCVaR _weights.csv")[,2:5]

EffFrontier_stats = read.csv(file = "EffFrontierMinCVaR_stats.csv")[,2:5]
vmu             = EffFrontier_stats[,1] ;
vrisk           = EffFrontier_stats[,2] ;
vmaxpercrisk    = EffFrontier_stats[,3] ;
vriskconc       = EffFrontier_stats[,4] ;

labelnames = c( "US bond" , "S&P 500" , "EAFE" , "GSCI")

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }

assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )

minassetmu   = min(assetmu);
maxassetmu   = max(assetmu);

minmu = min(vmu) ; maxmu = max(vmu);

Wsel = vmusel = vrisksel = vriskconcsel = c();

lm = minmu;
for( rm in seq( minmu + 0.000005 , maxmu , 0.000005 ) ){
   selection    =  c(vmu >= lm & vmu < rm) ;
   if( any(selection) ){
      vmusel       = c( vmusel       ,  mean(vmu[ selection ] ));
      Wsel         = rbind( Wsel     ,  apply( W[selection,] ,2,'mean' ))
      vrisksel     = c( vrisksel     ,  mean(vrisk[ selection ]) );
      vriskconcsel = c( vriskconcsel ,  mean(vriskconc[ selection ])  )
   }
   lm = rm; 
}
 
cAssets = ncol(monthlyR)
 colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; 
colnames( Wsel ) = c("US bond", "S&P 500", "EAFE", "GSCI")
rownames( Wsel ) = vmusel;

xlim = c( -0.02 , max(assetCVaR) + 0.02 )
ylim = c( 0 , maxassetmu + 0.0003 )

par( mar = c(4,5,3,3) , las=1 ,cex=0.9 , cex.axis=0.9)
plot( vriskconcsel, vmusel ,   type="l", lty = 3 , 
          main = "Min CVaR \n efficient frontier" , ylab="Monthly return" , xlab="Monthly CVaR (full) \n Monthly CVaR concentration(dotted)" , lwd=3 , xlim = xlim , ylim = ylim)
lines( vrisksel , vmusel,  lty = 1, lwd=3 )



for( c in 1:4 ){ text(y=assetmu[c],x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.8) };


#legend( legend=c("CVaR", "CVaR Conc" ) , lty = c(1,2), bty="o", x="bottomleft", lwd=3, cex=0.9)

par( mar = c(3,3,3,3) , las=1 )

barplot( t(Wsel) , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel,4), xlab="Monthly return" , 
      main="Weight allocation of Min CVaR portfolios \n in function of their average return" )

par( mar = c(0,3,0,2) )
plot.new()
legend("center",legend=c("US bond", "S&P 500", "EAFE", "GSCI"),fill=colorset,cex=0.8,ncol=4)

####################################
# 2e kolom: Min CVaR Concentration
####################################

W = read.csv(file = "EffFrontierMinCVaRConc _weights.csv")[,2:5]
EffFrontier_stats = read.csv(file = "EffFrontierMinCVaRConc_stats.csv")[,2:5]
vmu             = EffFrontier_stats[,1] ;
vrisk           = EffFrontier_stats[,2] ;
vmaxpercrisk    = EffFrontier_stats[,3] ;
vriskconc       = EffFrontier_stats[,4] ;

Wsel = vmusel = vrisksel = vriskconcsel = c();


for( rm in seq( minmu + 0.000005 , maxmu , 0.000005 ) ){
   selection    =  c(vmu >= lm & vmu < rm) ;
   if( any(selection) ){
      vmusel       = c( vmusel       ,  mean(vmu[ selection ] ));
      Wsel         = rbind( Wsel     ,  apply( W[selection,] ,2,'mean' ))
      vrisksel     = c( vrisksel     ,  mean(vrisk[ selection ]) );
      vriskconcsel = c( vriskconcsel ,  mean(vriskconc[ selection ])  )
   }
   lm = rm; 
}
 
cAssets = ncol(monthlyR)
 colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; 
colnames( Wsel ) = c("US bond", "S&P 500", "EAFE", "GSCI")
rownames( Wsel ) = vmusel;

par( mar = c(4,5,3,3), las=1 ,cex=0.9 , cex.axis=0.9)
plot( vriskconcsel, vmusel , type="l", lty = 3 , 
          main = "Min CVaR concentration \n efficient frontier" , ylab="Monthly return" , xlab="Monthly CVaR (full) \n Monthly CVaR concentration(dotted)" , lwd=3, xlim = xlim , ylim = ylim )
lines( vrisksel , vmusel,  lty = 1, lwd=3 )

for( c in 1:4 ){ text(y=assetmu[c],x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.8) };

#legend( legend=c("CVaR", "CVaR Conc" ) , lty = c(1,2), bty="o", x="bottomleft", lwd=3, cex=0.9)

par( mar = c(3,3,3,3) , las=1 )

barplot( t(Wsel) , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel,4), xlab="Monthly return" , 
      main="Weight allocation of Min CVaR concentration portfolios \n in function of their average return" )

dev.off()

