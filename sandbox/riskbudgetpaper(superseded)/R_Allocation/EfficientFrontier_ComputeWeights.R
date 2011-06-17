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

#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )
minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);

# Mean/CVaR concentration efficient portfolios
#-----------------------------------------------

# unconstrained solution is Minimum CVaR Concentration portfolio (having the equal risk contribution property):
# sol = MinMaxCompCVaRconportfolio(R=monthlyR, Riskupper = Inf ,Returnlower= -Inf )
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                Riskupper = Inf ,  mu = mu, sigma = sigma, M3=M3, M4=M4)

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   mPercrisk       = as.vector( sol[[4]] )  
vmaxpercrisk    = max( sol[[4]] )       ;   vmaxriskcontrib = max( sol[[5]] ) 

for( mutarget in seq(minmu+0.00001,maxmu,0.00001) ){
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                      Riskupper = Inf ,Returnlower= mutarget,
                      mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}

# For the highest return targets, a very high penalty parameter is (sometimes) needed

for( mutarget in c(seq( max(vmu) , maxmu, 0.000001),maxmu) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget, penalty = 1e9,
            mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}


if(clean){
 write.csv( W , file = "EffFrontierMinCVaRConc_weights_clean.csv" )
 write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk_clean.csv" )
 EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
 write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_clean.csv" )
}else{
 write.csv( W , file = "EffFrontierMinCVaRConc_weights.csv" )
 write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk.csv" )
 EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
 write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats.csv" )
}



# Mean/CVaR efficient portfolios
#--------------------------------

# unconstrained solution is Minimum CVaR portfolio (having the property that percentage CVaR corresponds to porfolio weights):
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                Riskupper = Inf , mu = mu, sigma = sigma, M3=M3, M4=M4) 

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   mPercrisk       = as.vector( sol[[4]] )  
vmaxpercrisk    = max( sol[[4]] )       ;   vmaxriskcontrib = max( sol[[5]] ) 

for( mutarget in seq(minmu+0.00001,maxmu,0.00001) ){
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                      Riskupper = Inf ,Returnlower= mutarget,
                      mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}

# For the highest return targets, a very high penalty parameter is (sometimes) needed

for( mutarget in c(seq( max(vmu) , maxmu, 0.000001),maxmu) ){
   print( c("mutarget equal to",mutarget) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget, penalty = 1e9,
            mu = mu, sigma = sigma, M3=M3, M4=M4) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}

if(clean){
   write.csv( W , file = "EffFrontierMinCVaR_weights_clean.csv" )
   write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk_clean.csv" )
   EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
   write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats_clean.csv" )
}else{
   write.csv( W , file = "EffFrontierMinCVaR_weights.csv" )
   write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk.csv" )
   EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
   write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats.csv" )
}


# Mean/StdDev efficient portfolios (use quadprog)
#-------------------------------------------------

library(quadprog)
N = 4 ;  maxweight = 1; minweight = 0; sumweights = 1; 
Amat = rbind( rep(1,N) ,  diag(x =1,nrow=N,ncol=N) ,  diag(x =-1,nrow=N,ncol=N) , as.numeric(mu) );
bvec =  c( sumweights , rep(minweight,N), rep(-maxweight,N)  ) 
dvec = matrix( rep(0,N) , ncol=1 )

mutarget = -10000; 
# min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0.
optw = solve.QP( Dmat = sigma   , dvec = dvec , Amat = t(Amat) , 
   bvec = matrix( c(bvec,mutarget) , ncol = 1) , meq =1  )$solution
sol = ES(weights=optw, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)
W               = optw                             ;   vmu             = sum(optw*mu)
vrisk           = sol$MES                          ;   mPercrisk       = as.vector( sol$pct_contrib_MES )  
vmaxpercrisk    = max( sol$pct_contrib_MES )       ;   vmaxriskcontrib = max( sol$contribution ) 

# unconstrained solution is Minimum CVaR portfolio (having the property that percentage CVaR corresponds to porfolio weights):
minmu = min( apply(monthlyR,2,'mean' )); 


for( mutarget in seq(minmu+0.00001,maxmu,0.00001) ){
   optw = solve.QP( Dmat = sigma   , dvec = dvec , Amat = t(Amat) , 
          bvec = matrix( c(bvec,mutarget) , ncol = 1) , meq =1  )$solution
   sol = ES(weights=optw, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)
   W = rbind( W, optw )
   mPercrisk = rbind( mPercrisk , as.vector( sol$pct_contrib_MES ) )
   vmu = c( vmu , sum(optw*mu) )
   vrisk = c( vrisk , sol$MES )
   vmaxpercrisk = c( vmaxpercrisk , max( sol$pct_contrib_MES ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol$contribution ) )
}

if(clean){
   write.csv( W , file = "EffFrontierMinVar_weights_clean.csv" )
   write.csv( mPercrisk , file = "EffFrontierMinVar_percrisk_clean.csv" )
   EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
   write.csv( EffFrontier_stats , file = "EffFrontierMinVar_stats_clean.csv" )
}else{
   write.csv( W , file = "EffFrontierMinVar_weights.csv" )
   write.csv( mPercrisk , file = "EffFrontierMinVar_percrisk.csv" )
   EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
   write.csv( EffFrontier_stats , file = "EffFrontierMinVar_stats.csv" )
}



