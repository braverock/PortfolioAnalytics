
# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

# Programs:

source("R_Allocation/allocation_functions_monthly.R"); source("R_Allocation/estimators.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 
memory.limit(2048)

# Choose data
datacase = "equitybondscommodity" # "equitybondscommodity" or "equitybondscommodity"

# Define optimization criteria
criteria =  c( "EW"  )

# Yearly or quarterly rebalancing
yearly = F;

# Length estimation period
estyears = 4

source("R_Allocation/allocation_functions_monthly.R"); source("R_Allocation/estimators.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 
memory.limit(2048)

#############################################################################
#   Load the data
#############################################################################

firstyear = 1976 ; firstquarter = 1; lastyear = 2009; lastquarter = 2; 

data = read.table( file= paste("data/",datacase,"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")
data = zoo( data[,2:ncol(data)] , order.by = date )

# "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 

cAssets = ncol(data)-2; # number of risky assets
  
# The loaded data has monthly frequency

# Estimation uses real returns

monthlyR = data[,(1:(cAssets+1))]-data[,cAssets+2]
monthlyR.cash = monthlyR[,cAssets+1]
R = monthlyR[,1:cAssets] 
plot(R)

head(R);tail(R)

from=to=c();

if(yearly){
	   for( year in firstyear:(lastyear-estyears+1) ){ 
           from = c( from , paste( year,"-01-01",sep="" ) );
           to  = c( to  , paste( (year+estyears-1),"-12-31",sep="" ) ) }
         nsamples = length(from);
         names.input = paste( rep("y_",nsamples) , seq( (firstyear+estyears),lastyear+1,1) , sep="" );
}else{
	   for( year in firstyear:(lastyear-estyears) ){ 
      	from = c( from , paste( year,"-01-01",sep="" ) , paste( year,"-04-01",sep="" ) , paste( year,"-07-01",sep="" ) , paste( year,"-10-01",sep="" ) );
      	to  = c( to  , paste( (year+estyears-1),"-12-31",sep="" ), paste( (year+estyears),"-03-31",sep="" ), paste( (year+estyears),"-06-30",sep="" ), 
                           paste( (year+estyears),"-09-30",sep="" ) ) }
         from =  c( from , paste( year+1,"-01-01",sep="" )  ); to   =  c( to   , paste( (year+estyears),"-12-31",sep="" ) )
         nsamples = length(from);
         #names of quarters for which the forecast is made: 
   	   names.input = paste( c("Q1y_","Q2y_","Q3y_","Q4y_") , rep(seq( (firstyear+estyears),lastyear,1),each=4) , sep="" );
         names.input = c( names.input , paste( "Q1y_" , lastyear+1, sep="" ) );
         # this is assuming estimation window runs from firstyearQ1 to lastyearQ4
         from = from[firstquarter:(length(from)-4+lastquarter)]; to = to[firstquarter:(length(to)-4+lastquarter)] 
         names.input = names.input[firstquarter:(length(names.input)-4+lastquarter)]
}

cPeriods = length(to) ; cCriteria = length(criteria) ;

percriskcontribcriterion = "mES"; 

RCout = matrix(  rep(0,cPeriods*cAssets) , ncol= cAssets );

#wper = matrix(rep(1/cAssets, cAssets),ncol=1)

W = read.csv( file = paste("C:\\Documents and Settings\\Administrator\\Desktop\\risk budget programs\\weights\\mES\\equitybondscommodity\\quarterly\\unconstrained\\", "mES.4yr-InfInf" , ".csv" , sep="")  );


RCout = matrix(  rep(0, cCriteria*cPeriods*(cAssets)) , ncol= (cAssets) ); # add possibility of cash if risk budget constraints are infeasible. 


# downside risk
alpha = 0.05;

# Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix


for( per in c(1:cPeriods) ){

       print("-----------New estimation period ends on observation------------------")
       print( paste(to[per],"out of total number of obs equal to", max(to) ));
       print("----------------------------------------------------------------")

       matrix(as.vector(wper[1:4]),ncol=1)

       # Estimate GARCH model with data from inception

       inception.R = window(R, start = as.Date(from[1]) , end = as.Date(to[per]) );

       # Estimate comoments of innovations with rolling estimation windows
       in.sample.R = window(R, start = as.Date(from[per]) , end = as.Date(to[per]) );
       in.sample.R = checkData(in.sample.R, method="matrix"); 


       # Estimation of mean return
       M = c();
       library(TTR)
       Tmean = 47 # monthly returns: 4 year exponentially weighted moving average
       for( i in 1:cAssets ){
         # M = cbind( M , as.vector( rollmean(x=inception.R[,i],k=Tmean, align="right",na.pad=T) ) ) #2/(n+1)
         M = cbind( M , as.vector( EMA(x=inception.R[,i],n=Tmean) ) ) #2/(n+1)
         # M = cbind( M , as.vector(cumsum())/c(1:length(inception.R[,i])) ) 
       }
       M = zoo( M , order.by=time(inception.R) )

       # Center returns (shift by one observations since M[t,] is rolling mean t-Tmean+1,...,t; otherwise lookahead bias)
       inception.R.cent = inception.R;
       ZZ = matrix( rep(as.vector( apply( inception.R[1:Tmean, ] , 2 , 'mean' )),Tmean),byrow=T,nrow=Tmean);
       inception.R.cent[1:Tmean,] = inception.R[1:Tmean, ] - ZZ;
       if( nrow(inception.R)>(Tmean+1) ){ 
                 A = M[Tmean:(nrow(inception.R)-1),];
                 A = zoo( A , order.by = time(inception.R[(Tmean+1):nrow(inception.R), ])) ; #shift dates; otherwise zoo poses problem
                 inception.R.cent[(Tmean+1):nrow(inception.R), ] = inception.R[(Tmean+1):nrow(inception.R), ] - A}
       
       # Garch estimation 
       S = c();
       for( i in 1:cAssets ){
            gout =  garchFit(formula ~ garch(1,1), data = inception.R.cent[,i],include.mean = F, cond.dist="QMLE", trace = FALSE )
            if( as.vector(gout@fit$coef["alpha1"]) < 0.01 ){
               sigmat = rep( sd( as.vector(inception.R.cent[,i])), length(inception.R.cent[,i]) ); 
            }else{
               sigmat = gout@sigma.t
            }
            S = cbind( S , sigmat)
       }
       S = zoo( S , order.by=time(inception.R.cent) )
       #plot(S, main="Check visually garch volatility estimates \n If estimated alpha < 0.01, the standard deviation is used. " ) 


       # Estimate correlation, coskewness and cokurtosis matrix locally using cleaned innovation series in three year estimation window
       selectU = window(inception.R.cent, start = as.Date(from[per]) , end = as.Date(to[per]) )
       selectU = selectU/window(S, start = as.Date(from[per]) , end = as.Date(to[per]) );
       selectU = clean.boudt2(selectU , alpha = alpha )[[1]];
       in.sample.T = nrow(selectU);
       Rcor = matrix(rep(0,cAssets^2),nrow=cAssets,ncol=cAssets)
       M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
       M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3)

       for(t in c(1:in.sample.T)){
          centret = as.vector(selectU[t,]);
          Rcor = Rcor +  centret%*%t(centret)
          M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
          M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
       }
       resSigma = Rcor =  1/in.sample.T*Rcor
       sdzero = function(data){ return( sqrt(mean(data^2)) ) }
       D = diag( apply(selectU,2,'sdzero')^(-1),ncol=cAssets )
       Rcor = D%*%Rcor%*%D
       M3 = 1/in.sample.T*M3
       M4 = 1/in.sample.T*M4

       # we only need mean and conditional covariance matrix of last observation
       mu = matrix(tail(M,n=1),ncol=1 ) ;
       D = diag( as.vector(as.vector(tail(S,n=1) ) ),ncol=cAssets )
       sigma = D%*%Rcor%*%D

       switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GVaR = {percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alpha,mu=mu,sigma=sigma)[[3]] ; return( cont )  }},
                GES = {percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alpha,sigma=sigma)[[3]] ; return( cont ) }},
                mVaR = {percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }},
                mES = {percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
            )
            RCout[ per ,  ] =  as.vector(percriskcontrib(wper))

} # end loop periods  


#write.table( RCout ,  paste("riskcont/", percriskcontribcriterion,"/",datacase,"/",frequency,"/EW.csv" , sep=""),
#             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")

write.table( RCout ,file = paste("C:\\Documents and Settings\\Administrator\\Desktop\\risk budget programs\\riskcont\\mES\\equitybondscommodity\\quarterly\\unconstrained\\", "mES.4yr-InfInf" , ".csv" , sep="")  );
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")



 


