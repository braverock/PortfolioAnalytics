setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Equal risk portfolio
cAssets = 2;
p = priskbudget = 0.95;

mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 

data = read.table( file= paste("data/","/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
monthlyR = indexes = zoo( data[,2:3] , order.by = date )

# Summary stats individual assets

head(indexes[,1:2],2); tail(indexes[,1:2],2)
apply(indexes[,1:2],2,'mean')*12
apply(indexes[,1:2],2,'sd')
ES(indexes[,1],method="modified")
ES(indexes[,2],method="modified")


#################################################################################
# Make Exhibit 2 Risk budget paper: weight and CVaR allocation static portfolios
#################################################################################

# Equal-weight portfolio
w5050 <- c(0.5,0.5)
mu = apply(indexes,2,'mean')
sum( w5050*mu*12 ) ;
#VaR(R=indexes[,1:2], weights=w5050, portfolio_method="component")
ES(R=indexes[,1:2], weights=w5050, portfolio_method="component")

# 60/40 bond equity allocation
w6040 <- c(0.4,0.6); 
sum( w6040*mu*12 ) ; 
#VaR(R=indexes[,1:2], weights=w6040, portfolio_method="component")
ES(R=indexes[,1:2], weights=w6040, portfolio_method="component")


# Min CVaR portfolio
sigma = cov(indexes)
M3 = PerformanceAnalytics:::M3.MM(indexes) 
M4 = PerformanceAnalytics:::M4.MM(indexes) 

library(DEoptim)
obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 ES(R=indexes[,1:2],weights = matrix(w,ncol=1), mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
wMinCVaR <- wstar / sum(wstar)
print(wMinCVaR)
#      par1       par2 
# 0.97808288 0.02191712 
# wMinCVaR = c( 0.97808288, 0.02191712)
print(sum(wMinCVaR*mu*12))
ES(R=indexes[,1:2], weights=matrix(wMinCVaR,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)


# Min CVaR Concentration portfolio

obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 CVaR <- ES(R=indexes[,1:2], weights=matrix(w,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)
 out <- max(CVaR$contribution)
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
wMinCVaRConc <- wstar / sum(wstar) 
print(wMinCVaRConc)
#> print(wMinCVaRConc)
#     par1      par2 
#0.8519865 0.1480135
# wMinCVaRConc = c( 0.8519865 , 0.1480135  )
print(sum(wMinCVaRConc*mu*12))
ES(R=indexes[,1:2], weights=wMinCVaRConc, portfolio_method="component")


# 60/40 Risk allocation portfolio
obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 CVaR <- ES(R=indexes[,1:2], weights=matrix(w,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)
 tmp1 <- CVaR$MES
 tmp2 <- max(CVaR$pct_contrib_MES - c(0.401, 0.601) , 0)
 tmp3 <- max(c(0.395, 0.595) - CVaR$pct_contrib_MES , 0)
 out  <- tmp1 + 1e3 * tmp2 + 1e3 * tmp3
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
w6040riskalloc <- wstar / sum(wstar) 
print(w6040riskalloc)
#      par1      par2 
# 0.8263106 0.1736894 
print(sum(w6040riskalloc*mu*12))
# w6040riskalloc = c( 0.8263106 , 0.1736894 ) 
ES(R=indexes[,1:2], weights=w6040riskalloc, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)

#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################

# Construct efficient frontier: given a return target minimize the largest percentage CVaR in the portfolio

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )
minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);

# unconstrained solution is Minimum CVaR Concentration portfolio (having the equal risk contribution property):
# sol = MinMaxCompCVaRconportfolio(R=monthlyR, Riskupper = Inf ,Returnlower= -Inf )
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= minmu) 

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   vmaxpercrisk    = max( sol[[4]] ) 
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


write.csv( W , file = "EffFrontierMinCVaRConc _weights_biv.csv" )
EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_biv.csv" )


sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= minmu) 

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


write.csv( W , file = "EffFrontierMinCVaR _weights_biv.csv" )
EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats_biv.csv" )


# Plotting 


postscript('frontier_bondequity.eps') 
layout( matrix(  c(1,2,3),  ncol = 1 ) , height= c(4,3.6,0.4), width=1)
####################################
# Min CVaR Concentration
####################################
labelnames = c( "US bond" , "S&P 500" )
W = read.csv(file = "EffFrontierMinCVaRConc _weights_biv.csv")[,2:3]
EffFrontier_stats = read.csv(file = "EffFrontierMinCVaRConc_stats_biv.csv")[,2:5]
vmu             = EffFrontier_stats[,1] ;
vrisk           = EffFrontier_stats[,2] ;
vmaxpercrisk    = EffFrontier_stats[,3] ;
vriskconc       = EffFrontier_stats[,4] ;

Wsel = vmusel = vrisksel = vriskconcsel = c();
lm = minmu = min(vmu) ; maxmu = max(vmu);
ylim = c( min(assetmu) - 0.0003 , max(assetmu) + 0.0003 )
xlim = c( 0 , max(assetCVaR) + 0.01 )

# seq( minmu + 0.00005 , maxmu , 0.00005 )
for( rm in seq( minmu + 0.00005 , maxmu , 0.00005 )  ){
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
colnames( Wsel ) = c("US bond", "S&P 500")
rownames( Wsel ) = vmusel;

par( mar = c(4,5,3,1), las=1 ,cex=0.9 , cex.axis=0.9)
plot( vriskconcsel, vmusel*12 , type="l", lty = 2 , 
          main = "Minimum CVaR concentration efficient frontier" , 
          ylab="Annualized mean return" , xlab="" , 
          lwd=2, xlim = xlim , ylim = ylim*12  )
lines( vrisksel , vmusel*12,  lty = 1, lwd=2 ) # , col="darkgray" )
legend("bottomright",legend=c("Total Portfolio CVaR" , "Largest Component CVaR" ),lty=c(1,2), cex=0.8,ncol=1,lwd=2)

for( c in 1:2 ){ text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.8) };

par( mar = c(5,5,3,1) , las=1 )

barplot( t(Wsel) , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel*12,3), xlab="Annualized mean return" , 
      ylab="Weight allocation" , main = "Minimum CVaR concentration efficient portfolios" )

par( mar = c(0,5,0,1) )
plot.new()
legend("center",legend=c("US bond", "S&P 500"),fill=colorset[1:2],cex=0.8,ncol=2)

dev.off()

# Other layout


postscript('frontier_bondequity.eps') 
layout( matrix(  c(1,2,3,4),  ncol = 2 ) , height= c(4,0.4,4,0.4), width=1 )
####################################
# Min CVaR Concentration
####################################
labelnames = c( "US bond" , "S&P 500" )
W = read.csv(file = "EffFrontierMinCVaRConc _weights_biv.csv")[,2:3]
EffFrontier_stats = read.csv(file = "EffFrontierMinCVaRConc_stats_biv.csv")[,2:5]
vmu             = EffFrontier_stats[,1] ;
vrisk           = EffFrontier_stats[,2] ;
vmaxpercrisk    = EffFrontier_stats[,3] ;
vriskconc       = EffFrontier_stats[,4] ;

Wsel = vmusel = vrisksel = vriskconcsel = c();
lm = minmu = min(vmu) ; maxmu = max(vmu);
ylim = c( min(assetmu) - 0.0003 , max(assetmu) + 0.0003 )
xlim = c( 0 , max(assetCVaR) + 0.01 )

# seq( minmu + 0.00005 , maxmu , 0.00005 )
for( rm in seq( minmu + 0.00005 , maxmu , 0.00005 )  ){
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
colnames( Wsel ) = c("US bond", "S&P 500")
rownames( Wsel ) = vmusel;

par( mar = c(4,5,5,1), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
plot( vriskconcsel, vmusel*12 , type="l", lty = 2 , 
          main = "Ann. Return vs total CVaR and Largest Component CVaR \n for minimum CVaR concentration efficient portfolios" , 
          ylab="Annualized mean return" , xlab="" , 
          lwd=2, xlim = xlim , ylim = ylim*12  )
lines( vrisksel , vmusel*12,  lty = 1, lwd=2 ) # , col="darkgray" )
for( c in 1:2 ){ text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.8) };

par( mar = c(0,5,0,1) )
plot.new()
legend("center",legend=c("Total Portfolio CVaR" , "Largest Component CVaR" ),lty=c(1,2), cex=0.8,ncol=1,lwd=2)


par( mar = c(5,5,5,1) , las=1 )

barplot( t(Wsel) , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel*12,3), xlab="Annualized mean return" , 
      ylab="Weight allocation" , main = "Weight allocation of\n  minimum CVaR concentration  efficient portfolios" )

par( mar = c(0,5,0,1) )
plot.new()
legend("center",legend=c("US bond", "S&P 500"),fill=colorset[1:2],cex=0.8,ncol=1)

dev.off()




