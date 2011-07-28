#################################################################################
# Create CVaR + RelPerf plot
#################################################################################

# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")


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

if(CC){
  load( file="mCVaR_CC.Rdata")
  load( file="mMU_CC.Rdata")
}else{
  load( file="mCVaR.Rdata")
  load( file="mMU.Rdata")
}

mMU = window(mMU , start=as.Date("1984-01-01") , end=tail(time(mMU),1) )
mCVaR = window(mCVaR , start=as.Date("1984-01-01") , end=tail(time(mCVaR),1) )

# do not plot the last month such that it is fully comparable with out-of-sample plots

sel = c( 1 : (nrow(mCVaR)-1) );


#   Load the data

nominalreturns = T;
if(nominalreturns){ 
   load(file=paste(getwd(),"/data/monthlyR.RData",sep="")) }else{ load(file=paste(getwd(),"/data/monthlyR_real.RData")) } 
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


###############


mincriterion =  "mES" # "mES" ,  "StdDev" 
load(paste(getwd(),"/","/oosreturns/", "simplereturns_",mincriterion ,".Rdata" ,sep="") )

   criteria =  paste( rep("weights/",length(names) ) , rep(mincriterion,length(names) ) , "/", names , sep="")
   criteria[ criteria == "weights/StdDev/EqualWeight" ] = "weights/mES/EqualWeight"

#colnames(simplereturns) = names
date = time(simplereturns)

#> names(simplereturns)
#[1] "weights/mES/EqualWeight_CC"               "weights/mES/MinRisk_CC"                   "weights/mES/MinRisk_PositionLimit_CC"    
#[4] "weights/mES/MinRisk_RiskLimit_CC"         "weights/mES/EqualRisk_CC"                 "weights/mES/MinRiskConc_CC"              
#[7] "weights/mES/MinRiskConc_PositionLimit_CC"




###############


if(CC){
   postscript(file="portfolioCVaRRelPerf_CC.eps")
}else{
   postscript(file="portfolioCVaRRelPerf.eps")
}
par(mfrow=c(2,1),mar=c(3,4,1,2))
 
chart.TimeSeries( mCVaR[sel,c(1,5,2)]  , ylim=c(0,max(mCVaR)), ylab = "Portfolio CVaR" , main = "",
 col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2,2) , 
   auto.grid = TRUE, minor.ticks = FALSE ,
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y") 

legend("topleft", legend = namelabels[c(1,5,2)], 
   col=c("black","black","darkgray"), lty=c("dashed","solid","solid"), lwd=c(2,2,2)  ,cex=0.7) 

chart.RelativePerformance( simplereturns[,c(6,2)] , simplereturns[,c(1)] , 
    main = "" , lty=c("solid","solid") , ylab="Relative performance  vs equal-weight", xlab="",
    col=c("black","darkgray") , las=1, lwd=c(2,2,5) , 
    auto.grid = TRUE, minor.ticks = FALSE ,ylim=c(0.7,1.65),
    period.areas = m.bear.dates , period.color="lightgray",
   date.format.in = "%Y-%m-%d",date.format = "%b %Y") 
legend("topleft", legend = c("Min CVaR Concentration","Min CVaR"), 
   col=c("black","darkgray"), lty=c("solid","solid"), lwd=c(2,2,5)  ,cex=0.7) 

dev.off()

if(CC){
   postscript(file="portfolioCVaRRelPerf_CC_alternatives.eps")
}else{
   postscript(file="portfolioCVaRRelPerf.eps")
}
par(mfrow=c(2,1),mar=c(3,4,1,2))
 
chart.TimeSeries( mCVaR[sel,c(7,4,3,6)]  , ylim=c(0,max(mCVaR)), ylab = "Portfolio CVaR" , main = "",
 col=c("black","darkgray","darkgray","black"), lty=c("solid","dashed","solid","dashed"), lwd=c(2,2,2,2) , 
   auto.grid = TRUE, minor.ticks = FALSE ,
   period.areas = m.bear.dates , period.color="lightgray",
  date.format.in = "%Y-%m-%d",date.format = "%b %Y") 

legend("topleft", legend = namelabels[c(7,4,3,6)], 
  col=c("black","darkgray","darkgray","black"), lty=c("solid","dashed","solid","dashed"), lwd=c(2,2,2)  ,cex=0.7) 

chart.RelativePerformance( simplereturns[,c(5,4,3,7)] , simplereturns[,c(1)] , 
    main = "" , lty=c("solid","dashed","solid","dashed") , ylab="Relative performance  vs equal-weight", xlab="",
    col=c("black","darkgray","darkgray","black") , las=1, lwd=c(2,2,5) , 
    auto.grid = TRUE, minor.ticks = FALSE ,ylim=c(0.7,1.65),
    period.areas = m.bear.dates , period.color="lightgray",
   date.format.in = "%Y-%m-%d",date.format = "%b %Y") 

legend("topleft", legend = namelabels[c(7,4,3,6)], 
  col=c("black","darkgray","darkgray","black"), lty=c("solid","dashed","solid","dashed"), lwd=c(2,2,2)  ,cex=0.7) 



dev.off()
