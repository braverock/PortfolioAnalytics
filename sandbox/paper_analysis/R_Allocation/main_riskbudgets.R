
# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")
#setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")

# Options:

BIP = FALSE
CC = TRUE

# Length estimation period
estyears = 5
mincriterion = percriskcontribcriterion = "StdDev" ; # "StdDev" "GES" "mES"

# Equal risk portfolio
cAssets = 4;
p = priskbudget = 0.95;



# Define your portfolio allocation strategy
# names = c( "EqualRisk" , "EqualWeight" , "MinRisk" , "MinRiskConc" , 
#             "MinRisk_PositionLimit" , "MinRisk_RiskLimit" , "MinRisk_ReturnTarget",
#             "MinRiskConc_PositionLimit" , "MinRiskConc_RiskLimit" , "MinRiskConc_ReturnTarget")
strategy = "EqualWeight"   # "MinRiskConc_PositionLimit" , "MinRiskConc_RiskLimit"

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(xts); library(fGarch); library("PerformanceAnalytics"); 

#   Load the data

nominalreturns = T;
newdata = T;
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 

if(nominalreturns){ 
   load(file="data/monthlyR.RData") 
   if(!BIP){
     load( file="data/mulist.Rdata") ; load( file="data/sigmalist.Rdata") ;
     if(!CC){ load( file="data/M3list.Rdata") ; load( file="data/M4list.Rdata") }else{
        load( file="data/M3list_CC.Rdata") ; load( file="data/M4list_CC.Rdata")
     }
   }else{
     load( file="data/mulist_BIP.Rdata") ; load( file="data/sigmalist_BIP.Rdata") ;
     if(!CC){ load( file="data/M3list_BIP.Rdata") ; load( file="data/M4list_BIP.Rdata") }else{
        load( file="data/M3list_BIP_CC.Rdata") ; load( file="data/M4list_BIP_CC.Rdata") 
      }
   }
}else{ 
   load(file="data/monthlyR_real.RData") 
   if(!BIP){
      load( file="data/mulist_real.Rdata") ; load( file="data/sigmalist_real.Rdata") ;
      if(!CC){ load( file="data/M3list_real.Rdata") ; load( file="data/M4list_real.Rdata") }else{
        load( file="data/M3list_real_CC.Rdata") ; load( file="data/M4list_real_CC.Rdata") 
      }
   }else{
      load( file="data/mulist_real_BIP.Rdata") ; load( file="data/sigmalist_real_BIP.Rdata") ;
      if(!CC){ load( file="data/M3list_real_BIP.Rdata") ; load( file="data/M4list_real_BIP.Rdata") }else{
        load( file="data/M3list_real_BIP_CC.Rdata") ; load( file="data/M4list_real_BIP_CC.Rdata")
     }
   }
} 

N = ncol(monthlyR)

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[1:(length(ep)-estyears*4)]+1
from = time(monthlyR)[ep.start]
from = seq( as.Date(paste(firstyear,"-01-01",sep="")), as.Date(paste(lastyear-estyears,"-07-01",sep="")), by="3 month") 
ep.end = ep[(1+estyears*4):length(ep)]
to = time(monthlyR)[ep.end]
nsamples = length(from);


#names of quarters for which the forecast is made: 
names.input = paste( c("Q1y_","Q2y_","Q3y_","Q4y_") , rep(seq( (firstyear+estyears),lastyear-1,1),each=4) , sep="" );
names.input = c( names.input , paste( c("Q1y_","Q2y_","Q3y_") , rep(lastyear,each=3) , sep="" ) );

# Construction of rebalanced portfolios:

library(PortfolioAnalytics)
eps = 0.025
rpconstraint<-constraint(assets=N, min_sum=(1-eps), max_sum=(1+eps), 
      min=rep(0,N), max=rep(1,N), weight_seq=generatesequence(),by=.001,rounding=3)
rp<- random_portfolios(rpconstraints=rpconstraint,permutations=200)
rp <-rp/rowSums(rp)

if( (strategy == "MinRisk"|strategy== "MinRisk_PositionLimit") & mincriterion == "StdDev" ){
   out = findportfolio.dynamic( R =monthlyR , mulist = mulist ,  sigmalist = sigmalist , M3list = M3list, M4list = M4list ,  from=from, to=to, 
            names.input=names.input, names.assets = colnames(monthlyR) , 
             p = p , priskbudget = priskbudget , mincriterion = mincriterion , 
             percriskcontribcriterion = percriskcontribcriterion , 
             strategy , optimize_method = "quadprog" )
}else{
# TRY on subset: from = from[1:2] ; to = to[1:2]; names.input = names.input[1:2]
   if( strategy == "EqualRisk" ){
     controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=2, c=0,
                NP=as.numeric(nrow(rp)),initialpop=rp) 
   }else{
     controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=6, c=.4,
                NP=as.numeric(nrow(rp)),initialpop=rp) 
   }
   out = findportfolio.dynamic( R =monthlyR , mulist = mulist ,  sigmalist = sigmalist , M3list = M3list, M4list = M4list ,  from=from, to=to, 
            names.input=names.input, names.assets = colnames(monthlyR) , 
             p = p , priskbudget = priskbudget , mincriterion = mincriterion , 
             percriskcontribcriterion = percriskcontribcriterion , 
             strategy , optimize_method = "DEoptim+L-BFGS-B" , controlDE = controlDE)
}


if(!BIP){
 if(!CC){
   write.table( out[[1]] , file = paste("weights/",mincriterion,"/",strategy,".csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[2]] , file = paste("riskcont/",mincriterion,"/",strategy,".csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[3]] , file = paste("riskreturn/",mincriterion,"/",strategy,".csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
  }else{
   write.table( out[[1]] , file = paste("weights/",mincriterion,"/",strategy,"_CC.csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[2]] , file = paste("riskcont/",mincriterion,"/",strategy,"_CC.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[3]] , file = paste("riskreturn/",mincriterion,"/",strategy,"_CC.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
  }
}else{
 if(!CC){
   write.table( out[[1]] , file = paste("weights/",mincriterion,"/",strategy,"_BIP.csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[2]] , file = paste("riskcont/",mincriterion,"/",strategy,"_BIP.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[3]] , file = paste("riskreturn/",mincriterion,"/",strategy,"_BIP.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
 }else{
   write.table( out[[1]] , file = paste("weights/",mincriterion,"/",strategy,"_BIP_CC.csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[2]] , file = paste("riskcont/",mincriterion,"/",strategy,"_BIP_CC.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
   write.table( out[[3]] , file = paste("riskreturn/",mincriterion,"/",strategy,"_BIP_CC.csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
 }
}
