setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Equal risk portfolio
cAssets = 2;
p = priskbudget = 0.95;

mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("DEoptim")

#   Load the data

firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 

data = read.table( file= paste("data/","/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
monthlyR = indexes = zoo( data[,2:5] , order.by = date )

clean = TRUE
if(clean){ # multivariate cleaning, on all assets s.t. same value in efficient frontier
   monthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]]
}
monthlyR = indexes = monthlyR[,1:2]

mu = apply(indexes,2,'mean')
# > mu*12
#     Bond     SP500 
# 0.0754596 0.1024934 
sigma = cov(indexes)
M3 = PerformanceAnalytics:::M3.MM(indexes) 
M4 = PerformanceAnalytics:::M4.MM(indexes) 

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
sum( w5050*mu*12 ) ;
#VaR(R=indexes[,1:2], weights=w5050, portfolio_method="component")
ES(R=indexes[,1:2], weights=w5050, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)

# 60/40 bond equity allocation
w6040 <- c(0.6,0.4); 
sum( w6040*mu*12 ) ; 
#VaR(R=indexes[,1:2], weights=w6040, portfolio_method="component")
ES(R=indexes[,1:2], weights=w6040, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)


# Min CVaR portfolio


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
#0.96864323 0.03135677 
# wMinCVaR = c( 0.96864323 , 0.03135677 )
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
#0.7700542 0.2299458 
# wMinCVaRConc = c( 00.7700542 , 0.2299458   )
print(sum(wMinCVaRConc*mu*12))
ES(R=indexes[,1:2], weights=wMinCVaRConc, portfolio_method="component")


# 60/40 Risk allocation portfolio
obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 CVaR <- ES(R=indexes[,1:2], weights=matrix(w,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)
 tmp1 <- CVaR$MES
 tmp2 <- max(CVaR$pct_contrib_MES - c(0.601,0.401 ) , 0)
 tmp3 <- max(c(0.599,0.399 ) - CVaR$pct_contrib_MES , 0)
 out  <- tmp1 + 1e3 * tmp2 + 1e3 * tmp3
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
w6040riskalloc <- wstar / sum(wstar) 
print(w6040riskalloc)
#      par1      par2 
#0.8123427 0.1876573  
print(sum(w6040riskalloc*mu*12))
# w6040riskalloc = c( 0.7290461 , 0.2709539 ) 
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
                Riskupper = Inf ,Returnlower= minmu, 
                mu = mu, sigma = sigma, M3=M3, M4=M4) 

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   vmaxpercrisk    = max( sol[[4]] ) 
vmaxriskcontrib = max( sol[[5]] ) 

# mutarget = 0.0047478;

for( mutarget in seq(minmu+0.00001,maxmu,0.00001) ){
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                      Riskupper = Inf ,Returnlower= mutarget,
                      mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}

# For the highest return targets, a very high penalty parameter is needed

for( mutarget in c(seq( max(vmu) , maxmu, 0.000001),maxmu) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget, penalty = 1e9,
            mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}




write.csv( W , file = "EffFrontierMinCVaRConc _weights_biv.csv" )
EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_biv.csv" )

# Min CVaR portfolio 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                                      R=monthlyR, Riskupper = Inf ,Returnlower= minmu) 

W               = as.vector( sol[[1]] ) 
vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) 
vmaxpercrisk    = max( sol[[4]] ) 
vmaxriskcontrib = max( sol[[5]] ) 

for( mutarget in seq(minmu+0.00001,maxmu,0.00001) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget,
            mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}


# For the highest return targets, a very high penalty parameter is needed

for( mutarget in c(seq( max(vmu) , maxmu, 0.000001),maxmu) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget , penalty = 1e9,
            mu = mu, sigma = sigma, M3=M3, M4=M4) 
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
for( rm in seq( minmu + 0.000005 , maxmu , 0.000005 )  ){
   selection    =  c(vmu >= lm & vmu < rm) ;
   if( any(selection) ){
      vmusel       = c( vmusel       ,  mean(vmu[ selection ] ));
      Wsel         = rbind( Wsel     ,  apply( W[selection,] ,2,'mean' ))
      vrisksel     = c( vrisksel     ,  mean(vrisk[ selection ]) );
      vriskconcsel = c( vriskconcsel ,  mean(vriskconc[ selection ])  )
   }
   lm = rm; 
}


# add the full investment in the S&P500     NEW ATTENTION
vmusel = c( vmusel , max(assetmu) )
Wsel = rbind( Wsel , c(0,1) )
vrisksel = c( vrisksel , max(assetCVaR) )
vriskconcsel = c( vriskconcsel , max(assetCVaR) ) 

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
for( c in 1:2 ){ text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.8, offset = 0) };

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


# Layout 3 
source("R_interpretation/chart.StackedBar.R"); 

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )
minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);


postscript('frontier_bondequity.eps') 
layout( matrix(  c(1,2,3,4,5,6),  ncol = 3 ) , height= c(4,0.4,4.25,0.15,4.25,0.15), width=c(1,0.78,0.78) )
####################################
# Min CVaR Concentration
####################################
labelnames = c( "US bond" , "S&P 500" )
W = read.csv(file = "EffFrontierMinCVaRConc _weights_biv.csv")[,2:3]
EffFrontier_stats = read.csv(file = "EffFrontierMinCVaRConc_stats_biv.csv")[,2:5]
vmu             = EffFrontier_stats[,1] ;
vrisk           = EffFrontier_stats[,2] ;
vmaxpercrisk    = EffFrontier_stats[,3] ;
vmaxriskcontrib = vriskconc       = EffFrontier_stats[,4] ;

order = sort(vmu,index.return=T)$ix
vmu = vmu[order]
vrisk = vrisk[order]
vmaxpercrisk = vmaxpercrisk[order]
vmaxriskcontrib = vmaxriskcontrib[order]
W  = W[order,]
a = duplicated(vmu)
vmu = vmu[!a]
vrisk = vrisk[!a]
vmaxpercrisk = vmaxpercrisk[!a]
vriskconc = vmaxriskcontrib = vmaxriskcontrib[!a]
W = W[!a,]

# Discontinuity in the frontier: For return targets in between
# vmu[41:42]*12
# the solution is no longer on the frontier of the feasible space


Wsel = vmusel = vrisksel = vriskconcsel = CVaRsel = c();
lm = minmu = min(vmu) ; maxmu = max(vmu);
ylim = c( min(assetmu) - 0.0003 , max(assetmu) + 0.0003 )
xlim = c( 0 , max(assetCVaR) + 0.01 )

binning = T # for the weight plots, binned data such that no visual misinterpretation is possible
step = 0.000015
if( binning ){
   for( rm in seq( minmu+step , maxmu , step )  ){
      selection    =  c(vmu >= lm & vmu < rm) ;
      if( any(selection) ){
         vmusel       = c( vmusel       ,  mean(vmu[ selection ] ));
         Wsel         = rbind( Wsel     ,  apply( W[selection,] ,2,'mean' ))
         vrisksel     = c( vrisksel     ,  mean(vrisk[ selection ]) );
         vriskconcsel = c( vriskconcsel ,  mean(vriskconc[ selection ])  )
         CVaRsel      = rbind( CVaRsel  , c( 1-mean(vriskconc[ selection ]/vrisk[selection]) , mean(vriskconc[ selection ]/vrisk[selection]) ) )
      }else{
         vmusel       = c( vmusel       ,  NA );
         Wsel         = rbind( Wsel     ,  rep(NA,2) )
         vrisksel     = c( vrisksel     ,  NA );
         vriskconcsel = c( vriskconcsel ,  NA  )
         CVaRsel      = rbind( CVaRsel  ,  rep(NA,2) )
 
      }
      lm = rm; 
   }
}else{
   vmusel = vmu;
   Wsel   = W
   vrisksel = vrisk
   vriskconcsel = vriskconc
   CVaRsel = cbind( 1-vriskconc/vrisk , vriskconc/vrisk ) 
}

# add the full investment in the S&P500     NEW ATTENTION
#vmusel = c( vmusel , max(assetmu) )
#Wsel = rbind( Wsel , c(0,1) )
#vrisksel = c( vrisksel , max(assetCVaR) )
#vriskconcsel = c( vriskconcsel , max(assetCVaR) ) 
#CVaRsel =  rbind( CVaRsel , c(0,1) )


cAssets = ncol(monthlyR)
 colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; 
colnames( Wsel ) = c("US bond", "S&P 500")
rownames( Wsel ) = vmusel;

par( mar = c(4,5,5,0), las=1 ,cex=0.9 , cex.axis=0.9, cex.main=0.95)
plot( vriskconc, vmu*12 , type="l", lty = 1, 
          main = "Ann. Return vs CVaR (concentration)     " , 
          ylab="Annualized mean return" , xlab="" , 
          lwd=2, xlim = xlim , ylim = ylim*12 , col="darkgray" )
points(  vriskconc, vmu*12 ,  pch=21 , col="darkgray")
lines( vrisk , vmu*12,  lty = 1, lwd=2 ) # , col="darkgray" )
points(  vrisk , vmu*12 ,  pch=21 )
for( c in 1:2 ){ text(y=assetmu[c]*12,x= assetCVaR[c]  , label=labelnames[c]  , cex = 0.7, offset = 0) };

par( mar = c(0,5,0,0) )
plot.new()
legend("center",legend=c("CVaR" , "CVaR concentration" ),lty=c(1,1), cex=0.8,ncol=1,lwd=2,
  col=c("black","darkgray"))


par( mar = c(5,1,14,0) , las=1 )

chart.StackedBar2(Wsel , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel*12,3), xlab="Annualized mean return" , 
      ylab="" , main = "Weight allocation  " ,
        las=1, l=2.5, r=0, u = 5, legend.loc = NULL,ylim=c(0,1),border = F)
abline(h=0);abline(h=1)
par( mar = c(0,1,0,0) )
plot.new()
legend("center",legend=c("US bond", "S&P 500"),fill=colorset[1:2],cex=0.8,ncol=2)


par( mar = c(5,1,14,0) , las=1 ) # top programmeren in chart.StackedBar2

chart.StackedBar2(CVaRsel , axisnames=T ,col=colorset,space=0 , names.arg = round(vmusel*12,3), xlab="Annualized mean return" , 
      ylab="" , main = "CVaR allocation " ,
        las=1, l=2.5, r=0, u = 5,  legend.loc = NULL,ylim=c(0,1),border = F)
abline(h=0);abline(h=1)

par( mar = c(0,1,0,0) )
plot.new()
legend("center",legend=c("US bond", "S&P 500"),fill=colorset[1:2],cex=0.8,ncol=2)

dev.off()


