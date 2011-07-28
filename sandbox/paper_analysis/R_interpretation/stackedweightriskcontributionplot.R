#################################################################################
# Create stacked weights and risk contributions plot
#################################################################################

# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")
#setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")


# Options:
################

# specify the number of years used for the estimation
estyears = 5;
Tstart = 1+(8-estyears)*4
CC = T
# Load programs

source("R_interpretation/chart.StackedBar.R"); 
library(zoo);  library(PerformanceAnalytics)

# number of risky assets
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
cAssets = 4 
 
# "MinRisk_ReturnTarget"  "EqualRisk"
# "MinRiskConc_ReturnTarget"

names = c(  "EqualWeight" , "MinRisk" , "MinRisk_PositionLimit" , "MinRisk_RiskLimit" ,
            "MinRiskConc" , "MinRiskConc_PositionLimit", "EqualRisk" ,
            "MinRisk_ReturnTarget", "MinRiskConc_ReturnTarget" )
if(CC){ names[1:length(names)] = paste( names[1:length(names)], "_CC", sep="") }

namelabels = c( "Equal-Weight" , "Min CVaR" , "Min CVaR + 40% Position Limit" , "Min CVaR + 40% CVaR Alloc Limit" ,  
       "Min CVaR Concentration" , "Min CVaR Concentration + 40% Position Limit", "Min CVaR + ERC constraint" , "Min CVaR + Return Target" , "Min CVaR Conc + Return Target" )

riskcrit = "mES"

# frequency of rebalancing: yearly of quarterly
frequency = "quarterly"
# Load portfolio weights:
weightsS1 = read.csv( file = paste("weights/", riskcrit , "/", names[1], ".csv" , sep="")  );
weightsS2 = read.csv( file = paste("weights/", riskcrit , "/",  names[2], ".csv" , sep="")  );
weightsS3 = read.csv( file = paste("weights/", riskcrit , "/",  names[3], ".csv" , sep="")  );
weightsS4 = read.csv( file = paste("weights/", riskcrit , "/",  names[4], ".csv" , sep="")  );
weightsS5 = read.csv( file = paste("weights/", riskcrit , "/",  names[5], ".csv" , sep="")  );
weightsS6 = read.csv( file = paste("weights/", riskcrit , "/",  names[6], ".csv" , sep="")  );
weightsS7 = read.csv( file = paste("weights/", riskcrit , "/",  names[7], ".csv" , sep="")  );

# rownames(weightsS7) = weightsS7[,1] ;weightsS7 = weightsS7[,2:5]; colnames(weightsS7) = colnames(weightsS6)
# write.table( weightsS7 , file = paste("weights/", riskcrit , "/",  names[7], ".csv" , sep="") ,
#            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")


# Load percentage risk contributions:
riskcontS1 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[1], ".csv" , sep="")  );
riskcontS2 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[2], ".csv" , sep="")  );
riskcontS3 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[3], ".csv" , sep="")  );
riskcontS4 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[4], ".csv" , sep="")  );
riskcontS5 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[5], ".csv" , sep="")  );
riskcontS6 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[6], ".csv" , sep="")  );
riskcontS7 = read.csv( file = paste("riskcont/", riskcrit , "/",  names[7], ".csv" , sep="")  );

#rownames(riskcontS7) = riskcontS7[,1] ;riskcontS7 = riskcontS7[,2:5]
# write.table( riskcontS7 , file = paste("riskcont/", riskcrit , "/",  names[7], ".csv" , sep="") ,
#             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")

if(frequency=="yearly"){
   rebal.names = seq( (firstyear+estyears),lastyear+1,1) 
}else{

   # Name labels using quarters:
   rebal.names = paste(rep( seq( (firstyear+estyears),lastyear,1) , each=4),c("Q1","Q2","Q3","Q4"),sep="")  
   rebal.names = c( rebal.names , paste( lastyear+1, "Q1" , sep="" ) )  
   rebal.names = rebal.names[firstquarter:(length(rebal.names)-4+lastquarter)]

   # Name labels using months: 
   nominalreturns = TRUE;
   if(nominalreturns){ load(file="monthlyR.RData") }else{ load(file="monthlyR_real.RData") } 
   ep = endpoints(monthlyR,on='quarters')
   # select those for estimation period
   ep.start = ep[1:(length(ep)-estyears*4)]+1
   from = time(monthlyR)[ep.start]
   from = seq( as.Date(paste(firstyear,"-01-01",sep="")), as.Date(paste(lastyear-estyears,"-07-01",sep="")), by="3 month") 
   ep.end = ep[(1+estyears*4):length(ep)]
   to = time(monthlyR)[ep.end]
   rebal.names = as.yearmon(to+1)


}


rownames(weightsS1) = rownames(weightsS2) = rownames(weightsS3) = rownames(weightsS4) = rebal.names;
rownames(weightsS5) = rownames(weightsS6) = rownames(weightsS7) = rebal.names; 

rownames(riskcontS1) = rownames(riskcontS2) = rownames(riskcontS3) = rownames(riskcontS4) = rebal.names; 
rownames(riskcontS5) = rownames(riskcontS6) = rownames(riskcontS7) = rebal.names;  


 colorset = sort(gray( 1-(seq(1,cAssets,1)/(cAssets))^1.5 )) ; 
#due to rounding, the sum of the risk contributions is sometimes 1 + epsilon: avoid this in plot

riskcontS1 = riskcontS1/rowSums(riskcontS1); riskcontS2 = riskcontS2/rowSums(riskcontS2); 
riskcontS3 = riskcontS3/rowSums(riskcontS3); riskcontS4 = riskcontS4/rowSums(riskcontS4); 
riskcontS5 = riskcontS5/rowSums(riskcontS5); riskcontS6 = riskcontS6/rowSums(riskcontS6); 
riskcontS7 = riskcontS7/rowSums(riskcontS7); 

Tend = nrow(weightsS1)

w.names = c( "US bond" , "S&P 500", "NAREIT"  , "GSCI" )
 l = 2
mar1 =c(2,l,2,1.1)
mar2 =c(0,l,2,1)
mar3 = c(3,l+1,3,0.1)
mar4 = c(2,l+1,2,0.1)

# Stacked weights plot: 
if(!CC){
postscript('stackedweightsriskcont_benchmark.eps') 
}else{
postscript('stackedweightsriskcont_benchmark_CC.eps')
}
   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(weightsS1[Tstart:Tend,],col=colorset,space=0,  main = namelabels[1], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )

   chart.StackedBar2(weightsS2[Tstart:Tend,],col=colorset,space=0,  main = namelabels[2], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS5[Tstart:Tend,],col=colorset,space=0,  main = namelabels[5], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   #chart.StackedBar2(weightsS7[Tstart:Tend,],col=colorset,space=0,  main = namelabels[7], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,col=colorset,lwd=6,ncol=4)



   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(riskcontS1[Tstart:Tend,],col=colorset,space=0,  main = namelabels[1], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS2[Tstart:Tend,],col=colorset,space=0,  main = namelabels[2], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS5[Tstart:Tend,],col=colorset,space=0,  main = namelabels[5], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   #chart.StackedBar2(riskcontS1,col=colorset,space=0,  main = namelabels[1], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()

if(!CC){
postscript('MinCVaR_alternatives.eps') 
}else{
postscript('MinCVaR_alternatives_CC.eps') 
}
   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(weightsS3[Tstart:Tend,],col=colorset,space=0,  main = namelabels[3], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS4[Tstart:Tend,],col=colorset,space=0,  main = namelabels[4], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS6[Tstart:Tend,],col=colorset,space=0,  main = namelabels[6], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,col=colorset,lwd=6,ncol=4)
   par(mar=mar3 , cex.main=1)

   chart.StackedBar2(riskcontS3[Tstart:Tend,],col=colorset,space=0,  main = namelabels[3], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS4[Tstart:Tend,],col=colorset,space=0,  main = namelabels[4], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS6[Tstart:Tend,],col=colorset,space=0,  main = namelabels[6], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()


#############################################################################################
# Plot the CVaR of the portfolios for each period, relatively to CVaR minimum CVaR portfolio


source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 

# downside risk
alpha = alphariskbudget = 0.05;
CC=T
if(CC){ source( paste( getwd(),"/R_allocation/coskewkurtosis.R" ,sep="") ) }


#   Load the data

nominalreturns = T;
if(nominalreturns){ load(file="monthlyR.RData") }else{ load(file="monthlyR_real.RData") } 
R = monthlyR
cAssets = ncol(monthlyR)

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[1:(length(ep)-estyears*4)]+1
from = time(monthlyR)[ep.start]; from = seq( as.Date(paste(firstyear,"-01-01",sep="")), as.Date(paste(lastyear-estyears,"-07-01",sep="")), by="3 month") 
ep.end = ep[(1+estyears*4):length(ep)]
to = time(monthlyR)[ep.end]
cPeriods = length(from);

# Loop where for each rebalancing period:
# - Compute total CVaR of portfolio

mCVaR = mMU = c()

for( per in c(1:cPeriods) ){
 
 # At the end of each month, we compute the CVaR
 enddates = na.omit(time(window( monthlyR , start = as.Date(to[per]) , end = as.Date(to[per]+90) ),on='months')[1:3])

 for( enddate in enddates ){    

       # add a loop over the next months except when per = cPeriods or -1,-2

       # Estimate GARCH model with data from inception

       inception.R = window(R, start = as.Date(from[1]) , end = enddate );

       # Estimate comoments of innovations with rolling estimation windows
       in.sample.R = window(R, start = as.Date(from[per]) , end = as.Date(to[per]) );
       in.sample.R = checkData(in.sample.R, method="matrix"); 

       # Estimation of mean return
       M = c();
       library(TTR)
       Tmean = 47 # monthly returns: 4 year exponentially weighted moving average
       for( i in 1:cAssets ){
         M = cbind( M , as.vector( EMA(x=inception.R[,i],n=Tmean) ) ) #2/(n+1)
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

       # Estimate correlation, coskewness and cokurtosis matrix locally using cleaned innovation series in three year estimation window
       selectU = window(inception.R.cent, start = as.Date(from[per]) , end = as.Date(to[per]) )
       selectU = selectU/window(S, start = as.Date(from[per]) , end = as.Date(to[per]) );
       selectU = clean.boudt2(selectU , alpha = 0.05 )[[1]];
       Rcor = cor(selectU)
       D = diag( as.vector(tail(S,n=1)  ),ncol=cAssets )
       sigma = D%*%Rcor%*%D

       # we only need mean and conditional covariance matrix of last observation
       mu = matrix(tail(M,n=1),ncol=1 ) ;
       D = diag( as.vector(as.vector(tail(S,n=1) ) ),ncol=cAssets )
       sigma = D%*%Rcor%*%D
       in.sample.T = nrow(selectU);
       # set volatility of all U to last observation, such that cov(rescaled U)=sigma 
       selectU = selectU*matrix( rep(as.vector(tail(S,n=1)),in.sample.T  ) , ncol = cAssets , byrow = T )
       if(CC){
          M3 = coskewCC(selectU); M4 = cokurtCC(selectU); 
       }else{
          M3 = PerformanceAnalytics:::M3.MM(selectU)
          M4 = PerformanceAnalytics:::M4.MM(selectU)
       }
       CVaR_period = c( operPortMES(as.numeric(weightsS1[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS2[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS3[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS4[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS5[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS6[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]] ,
          operPortMES(as.numeric(weightsS7[per,]),mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[1]]  )

       mu_period = c( sum(as.numeric(weightsS1[per,])*mu) , sum(as.numeric(weightsS2[per,])*mu) , 
                      sum(as.numeric(weightsS3[per,])*mu) , sum(as.numeric(weightsS4[per,])*mu) ,
                      sum(as.numeric(weightsS5[per,])*mu) , sum(as.numeric(weightsS6[per,])*mu) ,
                      sum(as.numeric(weightsS7[per,])*mu)  )

       mCVaR = rbind( mCVaR , CVaR_period )
       mMU   = rbind( mMU   , mu_period )
   }
}

colnames(mCVaR) = colnames(mMU) = names[1:7]
mCVaR = xts( mCVaR , 
  order.by = as.Date(time( window( monthlyR , start = as.Date(to[1]) , end = as.Date(to[cPeriods]) ))+1))
mMU = xts( mMU , 
  order.by = as.Date(time( window( monthlyR , start = as.Date(to[1]) , end = as.Date(to[cPeriods]) ))+1))

head(mCVaR[,1:7],2)
#> head(mCVaR[,1:7],2)
#           EqualWeight MinRisk MinRisk_PositionLimit MinRisk_RiskLimit MinRiskConc MinRiskConc_PositionLimit EqualRisk
#1984-01-01      0.0492  0.0320                0.0380            0.0333      0.0352                    0.0388    0.0352
#1984-02-01      0.0490  0.0304                0.0374            0.0322      0.0344                    0.0383    0.0344


if(CC){
  save(mCVaR, file="mCVaR_CC.Rdata")
  save(mMU, file="mMU_CC.Rdata")
}else{
  save(mCVaR, file="mCVaR.Rdata")
  save(mMU, file="mMU.Rdata")
}

head(mMU[,1:7],2)
#> head(mMU[,1:7],2)
#           EqualWeight    MinRisk MinRisk_PositionLimit MinRisk_RiskLimit MinRiskConc MinRiskConc_PositionLimit   EqualRisk
#1984-01-01 0.009865862 0.00959764            0.01084905       0.009814883 0.009944563                0.01012198 0.009945766
#1984-02-01 0.010176409 0.01003861            0.01105078       0.010193260 0.010297694                0.01042968 0.010299787


###################################################

if(CC){
  load( file="mCVaR_CC.Rdata")
  load( file="mMU_CC.Rdata")
}else{
  load( file="mCVaR.Rdata")
  load( file="mMU.Rdata")
}

mMU = window(mMU , start=as.Date("1984-01-01") , end=tail(time(mMU),1) )
mCVaR = window(mCVaR , start=as.Date("1984-01-01") , end=tail(time(mCVaR),1) )

if(CC){
 postscript(file="portfolioMeanCVaR_CC.eps")
}else{
 postscript(file="portfolioMeanCVaR.eps")
}
par(mfrow=c(2,1),mar=c(3,2,3,2))



plot( mMU[,1]*12  ,  type = "l" , ylim=c(min(mMU),max(mMU))*12,col="darkgray", lwd=1.5 , 
   main = "Expected annualized portfolio return" )
lines( mMU[,2]*12  , type = "l", col="black",lwd=2 , lty=3)
lines( mMU[,7]*12  , type = "l", col="darkgray",lwd=4)
lines( mMU[,5]*12  , type = "l", col="black", lwd=1.5)

legend("bottomleft", legend = c("Equal-Weight","Min CVaR Concentration","Min CVaR+ERC constraint","Min CVaR" ), 
   col=c("darkgray","black","darkgray","black"), lty=c("solid","solid","solid","dashed"), lwd=c(2,2,4,2)  ,cex=0.7) 

plot( mCVaR[,1]  ,  type = "l" , ylim=c(0,max(mCVaR)),col="darkgray", lwd=1.5 , main = "Portfolio CVaR" )
lines( mCVaR[,2]  , type = "l", col="black",lwd=1.5 , lty=3)
lines( mCVaR[,7]  , type = "l", col="darkgray",lwd=4)
lines( mCVaR[,5]  , type = "l", col="black", lwd=1.5)
dev.off()

# do not plot the last month such that it is fully comparable with out-of-sample plots

sel = c( 1 : (nrow(mCVaR)-1) );


# Bear periods
sp500 = window (monthlyR , start = from[1] , end = to[ length(to) ] )[,2]
bear = c(1:length(sp500))[sp500<mean(sp500)]
bear = c(1:length(sp500))[sp500<(-0.12)]
m.bear.dates = list();
i=1;
for( b in bear){
 m.bear.dates[[i]] = c( b-0.5, b+0.5)
 i = i + 1;  
}

out = table.Drawdowns(sp500,top=10) 
start.bear = out$From[out$Depth<(-0.12)]
end.bear = out$Trough[out$Depth<(-0.12)]
start.bear.index = c(1:length(sp500))[ time(sp500) ]
m.bear.dates = list()
v.bear.dates = c()
for( i in 1:length(start.bear) ){
   m.bear.dates[[i]] = c( as.yearmon(start.bear[i]) , as.yearmon(end.bear[i]) )
   v.bear.dates = c( v.bear.dates , seq(start.bear[i],end.bear[i],"days") )
}
v.bear.dates = as.Date( v.bear.dates )


if(CC){
postscript(file="portfolioCVaR_CC.eps")
}else{
postscript(file="portfolioCVaR.eps")
}
par(mfrow=c(2,1),mar=c(3,4,1,2))
 
chart.TimeSeries( mCVaR[sel,c(1,5,2)]  , ylim=c(0,max(mCVaR)), ylab = "Portfolio CVaR" , main = "",
 col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2,2) , 
   auto.grid = TRUE, minor.ticks = FALSE ,
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y") 

legend("topleft", legend = namelabels[c(1,5,2)], 
   col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2)  ,cex=0.7) 

chart.TimeSeries( mCVaR[sel,c(4,3,6)]  ,  type = "l" , ylim=c(0,max(mCVaR)), ylab = "Portfolio CVaR"  , main = "",
 col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2,2)  ,
   auto.grid = TRUE, minor.ticks = FALSE ,
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y") 

legend("topleft", legend = namelabels[c(4,3,6)], 
   col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2)  ,cex=0.7) 


dev.off()




