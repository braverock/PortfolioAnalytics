
# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")
# setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")

# Options:

# Length estimation period
estyears = 5
CC = T;
mean_EMA = FALSE # use exponentially weighted moving average or not

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 

if(CC){ source( paste( getwd(),"/R_allocation/coskewkurtosis.R" ,sep="") ) }
source( paste( getwd(),"/R_allocation/estimationfunctions.R" ,sep="") )

#   Load the data

nominal = T;
newdata = T;
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 

if(newdata){
   data = read.table( file= paste("data/","data.txt",sep="") ,header=T)
    #  "Bond"      "SP500"     "NAREIT"    "SPGSCI"    "TBill"     "Inflation" "EAFE"   
   date = as.Date(data[,1],format="%Y-%m-%d")
   data = zoo( data[,2:ncol(data)] , order.by = date )
   # "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" "EAFE"  
   cAssets = ncol(data)-3; # number of risky assets
   # The loaded data has monthly frequency
   if(!nominal){  monthlyR = data[,(1:(cAssets))]-data[,cAssets+2] }else{ monthlyR = data[,1:cAssets] }
   plot(monthlyR)
   if(nominal){ save(monthlyR,file="data/monthlyR.RData") }else{ save(monthlyR,file="data/monthlyR_real.RData")  }
}else{
   if(nominal){ load(file="data/monthlyR.RData") }else{ load(file="data/monthlyR_real.RData") } 
}

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[1:(length(ep)-estyears*4)]+1
from = time(monthlyR)[ep.start]
from = seq( as.Date(paste(firstyear,"-01-01",sep="")), as.Date(paste(lastyear-estyears,"-07-01",sep="")), by="3 month") 
ep.end = ep[(1+estyears*4):length(ep)]
to = time(monthlyR)[ep.end]
cPeriods = length(from);

estimateMoments = TRUE;

matrix.sqrt = function(A){
# Function that computes the square root of a symmetric, positive definite matrix:
   sva = La.svd(A);
   if( min(sva$d)>=0 ){ 
     Asqrt = sva$u%*%diag(sqrt(sva$d)) }else{
    stop("Matrix square root is not defined")}
   return(Asqrt)
}

comoments_inception = TRUE
N = ncol (monthlyR)

if(estimateMoments){

   R = monthlyR

   mulist = sigmalist = M3list = M4list = {}

   for( per in c(1:cPeriods) ){

       print("-----------New estimation period ends on observation------------------")
       print( paste(to[per],"out of total number of obs equal to", max(to) ));
       print("----------------------------------------------------------------")

       # Estimate GARCH model with data from inception

       inception.R = window(R, start = as.Date(from[1]) , end = as.Date(to[per]) );

       # Estimate comoments of innovations with rolling estimation windows
       if(comoments_inception){
         in.sample.R = inception.R;
       }else{
         in.sample.R = window(R, start = as.Date(from[per]) , end = as.Date(to[per]) );
         in.sample.R = checkData(in.sample.R, method="matrix"); 
       }

       # Estimation of mean return
       if( mean_EMA ){
          M = c();
          library(TTR)
          Tmean = 47 # monthly returns: 4 year exponentially weighted moving average
          for( i in 1:cAssets ){
            M = cbind( M , as.vector( EMA(x=inception.R[,i],n=Tmean) ) ) #2/(n+1)
            M = zoo( M , order.by=time(inception.R) )
          }
          mulist[[per]] = matrix(tail(M,n=1),ncol=1 ) ;

          # Center returns (shift by one observations since M[t,] is rolling mean t-Tmean+1,...,t; otherwise lookahead bias)
          inception.R.cent = inception.R;
          ZZ = matrix( rep(as.vector( apply( inception.R[1:Tmean, ] , 2 , 'mean' )),Tmean),byrow=T,nrow=Tmean);
          inception.R.cent[1:Tmean,] = inception.R[1:Tmean, ] - ZZ;
          if( nrow(inception.R)>(Tmean+1) ){ 
                 A = M[Tmean:(nrow(inception.R)-1),];
                 A = zoo( A , order.by = time(inception.R[(Tmean+1):nrow(inception.R), ])) ; #shift dates; otherwise zoo poses problem
                 inception.R.cent[(Tmean+1):nrow(inception.R), ] = inception.R[(Tmean+1):nrow(inception.R), ] - A}

       }else{
          mu =  apply( inception.R , 2 , 'mean' ) 
          mulist[[per]] = mu
          ZZ = matrix( rep( mu , nrow(inception.R) ),byrow=T,nrow=nrow(inception.R));
          inception.R.cent = inception.R - ZZ;
       }


       
       # Garch estimation 
       S = S_forecast = c(); 


       hatsigma <- function( par , x ){
         alpha = par[1] ; beta = par[2];
         omega = uncH*(1-alpha-beta)
         T = length(x)
         s2 = rep( uncH , T )
         # Include the FILTER here to replace the loop
         e2 <- x^2
         # If the index is negative, it would strip the member whose position has the same absolute value as the negative index. 
         e2t <- omega + alpha*c(mean(e2),e2[-length(x)])
         s2 <- filter(e2t, beta, "recursive", init = mean(e2))
         return(s2)
       }

       llhGarch11N <- function(par, x)
       {
          s2 = hatsigma( par = par , x = x )
          -sum(log(dnorm(x, mean = 0, sd = sqrt(abs(s2)))))
       }

       parN = c(0.04,0.95)
       small <- 1e-3
       lowN <- c(  small, small)
       upN  <- c(  1-small, 1-small)

       vuncH = rep(NA,cAssets)
       for( i in 1:cAssets ){
         #k = qchisq(0.99,df=1) ; 
         #jumpIndicator = ( (inception.R.cent[,i]/mad(inception.R.cent[,i]))^2<=k ) 
         #xfilt = inception.R.cent[jumpIndicator,i]
         #uncH = mean(xfilt^2)*(0.99/pchisq(k,df=3))
         uncH = mean(inception.R.cent[,i]^2)
         fitN <- nlminb(start=parN, objective=llhGarch11N , x = inception.R.cent[,i], lower=lowN, upper=upN,
                  control=list(x.tol = 1e-8,trace=1)) 
           par <- round(fitN$par,6)
           x = inception.R.cent[,i]
           # Likelihood ratio test statistic for the case of time-varying garch effects

            # if less than 1% improvement wrt constant, we use the constant
                 sigmat = sqrt(hatsigma( par = par , x = inception.R.cent[,i]) )
                 e2 = as.numeric(tail(inception.R.cent[,i],1)^2)
                 S_forecast = c( S_forecast , 
                    sqrt(  uncH*(1-sum(par))+ par[1]*e2+ par[2]*tail(sigmat,1)^2  )  )

            S = cbind( S , sigmat)
            #gout =  garchFit(formula ~ garch(1,1), data = inception.R.cent[,i],include.mean = F, cond.dist="QMLE", trace = FALSE )
            vuncH[i] = uncH 
       }
       S = zoo( S , order.by=time(inception.R.cent) )
       plot(S)
       print( sqrt(vuncH) )

       # Estimate correlation, coskewness and cokurtosis matrix locally using cleaned innovation series in three year estimation window
       if(comoments_inception){
          selectU = window(inception.R.cent, start = as.Date(from[1]) , end = as.Date(to[per]) )
          selectU = selectU/window(S, start = as.Date(from[1]) , end = as.Date(to[per]) );
       }else{
          selectU = window(inception.R.cent, start = as.Date(from[per]) , end = as.Date(to[per]) )
          selectU = selectU/window(S, start = as.Date(from[per]) , end = as.Date(to[per]) ); 
       }
       selectU = clean.boudt2(selectU , alpha = 0.05 )[[1]];
       Rcor = cor(selectU)
       Rcor = cor(selectU)
       Rt = DCCestimation(  mDevolR.centered= as.matrix(selectU)  , parN = parN , uncR = Rcor )

       D = diag(S_forecast)
       sigma = D%*%Rcor%*%D;
       sigmalist[[per]] = sigma;
       in.sample.T = nrow(selectU);
       # set volatility of all U to last observation, such that cov(rescaled U)=sigma [correct this in PA]
       uncS = sqrt(diag( cov(selectU) ))
       selectU = selectU*matrix( rep(S_forecast/uncS,in.sample.T  ) , ncol = cAssets , byrow = T )
       if(!CC){
         M3list[[per]] = PerformanceAnalytics:::M3.MM(selectU)
         M4list[[per]] = PerformanceAnalytics:::M4.MM(selectU)
       }else{
         M3list[[per]] = coskewCC(selectU)
         M4list[[per]] = cokurtCC(selectU)
       }
  }

}

if(nominal){ 
     save( mulist , file="data/mulist.Rdata") ; save( sigmalist , file="data/sigmalist.Rdata") ;
     if(!CC){ save( M3list , file="data/M3list.Rdata") ; save( M4list , file="data/M4list.Rdata") }else{
       save( M3list , file="data/M3list_CC.Rdata") ; save( M4list , file="data/M4list_CC.Rdata")
     }
}else{ 
     save( mulist , file="data/mulist_real.Rdata") ; save( sigmalist , file="data/sigmalist_real.Rdata") ;
     if(!CC){ save( M3list , file="data/M3list_real.Rdata") ; save( M4list , file="data/M4list_real.Rdata") }else{
       save( M3list , file="data/M3list_real_CC.Rdata") ; save( M4list , file="data/M4list_real_CC.Rdata")
     }
} 


# sqrt(diag( sigmalist[[28]]))
# sqrt(diag( sigmalist[[29]]))

s1 = s2 = s3 = s4 =c()

for( k in 1:length(sigmalist) ){
  S = sigmalist[[k]]
  s1 = c( s1 , S[1,1] ) ; s2 = c( s2 , S[2,2] )
  s3 = c( s3 , S[3,3] ) ; s4 = c( s4 , S[4,4] )
}
par( mfrow=c(2,2) ); plot( sqrt(s1) , type="l" ) ; plot( sqrt(s2) , type="l") ; plot(  sqrt(s3) , type="l") ; plot( sqrt(s4) , type="l")



