
#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################


setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs/insample")
#setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")
cAssets = 4; p = priskbudget = 0.95;
mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("PortfolioAnalytics")
clean = TRUE
CC = TRUE

#   Load the data
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste(getwd(),"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

monthlyR = zoo( data[,2:(1+cAssets)] , order.by = date )

if(clean){ monthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]] }
mu = apply(monthlyR,2,'mean')
sigma = cov(monthlyR)
if(!CC){
  M3 = PerformanceAnalytics:::M3.MM(monthlyR) 
  M4 = PerformanceAnalytics:::M4.MM(monthlyR) 
}else{
  source( "coskewkurtosis.R"  ) 
  M3 = coskewCC(monthlyR-matrix( rep(mu,nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  ); 
  M4 = cokurtCC(monthlyR-matrix( rep(mu,nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  );
}
N = ncol(monthlyR)


#################################################################################
# Make Exhibit 3 Risk budget paper: Efficient frontier plot
#################################################################################

mESfun = function( series ){ return( operMES(  series , alpha = 0.05 , r = 2 ) ) }
assetmu = apply( monthlyR , 2 , 'mean' )
assetCVaR = apply( monthlyR , 2 , 'mESfun' )
#      Bond      SP500     NAREIT     SPGSCI 
#0.01228896 0.12481216 0.27768243 0.20588189 
mESfun2 = function( w ){ return( operPortMES(w,mu=mu,alpha=0.05,sigma=sigma,M3=M3,M4=M4)[[1]]   ) }
mESfun2( as.matrix(c(0,1,0,0)) )

minmu = min(assetmu); maxmu = max(assetmu); print(minmu*12); print(maxmu*12);

# Mean/CVaR concentration efficient portfolios
#-----------------------------------------------

eps = 0.025
set.seed(1234)
rpconstraint<-constraint(assets=N, min_sum=(1-eps), max_sum=(1+eps), 
      min=rep(0,N), max=rep(1,N), weight_seq=generatesequence(),by=.001,rounding=3)
rp<- random_portfolios(rpconstraints=rpconstraint,permutations=200)
rp<-rbind( rp , diag( rep(1,4) ) )  
rp <-rp/rowSums(rp)
controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=6, c=.4,
                NP=as.numeric(nrow(rp)),initialpop=rp) 
#controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=2, c=0,
#                NP=as.numeric(nrow(rp)),initialpop=rp) 
# unconstrained solution is Minimum CVaR Concentration portfolio (having the equal risk contribution property):
# sol = MinMaxCompCVaRconportfolio(R=monthlyR, Riskupper = Inf ,Returnlower= -Inf )
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                Riskupper = Inf ,  mu = mu, sigma = sigma, M3=M3, M4=M4, controlDE = controlDE)

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   mPercrisk       = as.vector( sol[[4]] )  
vmaxpercrisk    = max( sol[[4]] )       ;   vmaxriskcontrib = max( sol[[5]] ) 

mutarget = minmu+0.00001
while( mutarget <= maxmu ){
   print("------------")
    print( c(minmu,mutarget,maxmu) ) 
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                      Riskupper = Inf ,Returnlower= mutarget,
                      mu = mu, sigma = sigma, M3=M3, M4=M4, controlDE = controlDE) 
   controlDE$initialpop = rbind( rp , W )
   controlDE$NP         = as.numeric( nrow(controlDE$initialpop) )
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
   oldmutarget = mutarget
   mutarget = as.vector( sol[[2]] ) + 0.00001
   if( mutarget == oldmutarget ){ mutarget = mutarget +  0.00001 }
}

# For the highest return targets, a very high penalty parameter is (sometimes) needed

for( mutarget in c(seq( max(vmu) , maxmu,0.00001),maxmu) ){
   print("------------")
   print( c("mutarget equal to",mutarget) )
   controlDE$initialpop = rbind( rp , W )
   controlDE$NP         = as.numeric( nrow(controlDE$initialpop) )
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


EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
if(clean){
   if(!CC){
      write.csv( W , file = "EffFrontierMinCVaRConc_weights_clean.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk_clean.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_clean.csv" )
   }else{
      write.csv( W , file = "EffFrontierMinCVaRConc_weights_clean_CC.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk_clean_CC.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_clean_CC.csv" )
   }
}else{
   if(!CC){
      write.csv( W , file = "EffFrontierMinCVaRConc_weights.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats.csv" )
   }else{
      write.csv( W , file = "EffFrontierMinCVaRConc_weights_CC.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaRConc_percrisk_CC.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaRConc_stats_CC.csv" )
   }
}

# Mean/CVaR efficient portfolios
#--------------------------------

eps = 0.025
set.seed(1234)
rpconstraint<-constraint(assets=N, min_sum=(1-eps), max_sum=(1+eps), 
      min=rep(0,N), max=rep(1,N), weight_seq=generatesequence(),by=.001,rounding=3)
rp<- random_portfolios(rpconstraints=rpconstraint,permutations=200)
rp<-rbind( rp , diag( rep(1,4) ) )  
rp <-rp/rowSums(rp)
controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=6, c=.4,
                NP=as.numeric(nrow(rp)),initialpop=rp) 

# unconstrained solution is Minimum CVaR portfolio (having the property that percentage CVaR corresponds to porfolio weights):
minmu = min( apply(monthlyR,2,'mean' )); 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                Riskupper = Inf , mu = mu, sigma = sigma, M3=M3, M4=M4, controlDE = controlDE) 

W               = as.vector( sol[[1]] ) ;   vmu             = as.vector( sol[[2]] )
vrisk           = as.vector( sol[[3]] ) ;   mPercrisk       = as.vector( sol[[4]] )  
vmaxpercrisk    = max( sol[[4]] )       ;   vmaxriskcontrib = max( sol[[5]] ) 

mutarget = minmu+0.00001
while( mutarget <= maxmu ){
   print("------------")
   print( c("mutarget equal to",mutarget) )
   controlDE$initialpop = rbind( rp , W )
   controlDE$NP         = as.numeric( nrow(controlDE$initialpop) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                      Riskupper = Inf ,Returnlower= mutarget,
                      mu = mu, sigma = sigma, M3=M3, M4=M4, controlDE = controlDE) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
   oldmutarget = mutarget
   mutarget = as.vector( sol[[2]] ) + 0.00001
   if( mutarget == oldmutarget ){ mutarget = mutarget +  0.00001 }
}

# For the highest return targets, a very high penalty parameter is (sometimes) needed

for( mutarget in c(seq( max(vmu) , maxmu, 0.00001),maxmu) ){
   print( c("mutarget equal to",mutarget) )
   controlDE$initialpop = rbind( rp , W )
   controlDE$NP         = as.numeric( nrow(controlDE$initialpop) )
   sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
            Riskupper = Inf ,Returnlower= mutarget, penalty = 1e9,
            mu = mu, sigma = sigma, M3=M3, M4=M4, controlDE = controlDE) 
   W = rbind( W, as.vector( sol[[1]] ) )
   mPercrisk = rbind( mPercrisk , as.vector( sol[[4]] ) )
   vmu = c( vmu , as.vector( sol[[2]] ))
   vrisk = c( vrisk , as.vector( sol[[3]] ) )
   vmaxpercrisk = c( vmaxpercrisk , max( sol[[4]] ) )
   vmaxriskcontrib = c( vmaxriskcontrib , max( sol[[5]] ) )
}

EffFrontier_stats = cbind( vmu , vrisk , vmaxpercrisk , vmaxriskcontrib)
if(clean){
   if(!CC){
      write.csv( W , file = "EffFrontierMinCVaR_weights_clean.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk_clean.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats_clean.csv" )
   }else{
      write.csv( W , file = "EffFrontierMinCVaR_weights_clean_CC.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk_clean_CC.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats_clean_CC.csv" )
   }
}else{
   if(!CC){
      write.csv( W , file = "EffFrontierMinCVaR_weights.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats.csv" )
   }else{
      write.csv( W , file = "EffFrontierMinCVaR_weights_CC.csv" )
      write.csv( mPercrisk , file = "EffFrontierMinCVaR_percrisk_CC.csv" )
      write.csv( EffFrontier_stats , file = "EffFrontierMinCVaR_stats_CC.csv" )
   }
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


for( mutarget in seq(minmu+0.00005,maxmu,0.00005) ){
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



