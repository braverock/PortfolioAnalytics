
# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")
# setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")

# Options:

# Length estimation period
estyears = 8

# Equal risk portfolio
cAssets = 4;
p = priskbudget = 0.95;

mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Define your portfolio allocation strategy
# names = c( "EqualRisk" , "EqualWeight" , "MinRisk" , "MinRiskConc" , 
#             "MinRisk_PositionLimit" , "MinRisk_RiskLimit" , "MinRisk_ReturnTarget",
#             "MinRiskConc_PositionLimit" , "MinRiskConc_RiskLimit" , "MinRiskConc_ReturnTarget")
strategy = "MinRiskConc_RiskLimit"  # "MinRiskConc_PositionLimit" , "MinRiskConc_RiskLimit"

# Load programs

source("R_Allocation/Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); 

#   Load the data

nominalreturns = T;
newdata = T;
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 

if(newdata){
   data = read.table( file= paste("data/","data.txt",sep="") ,header=T)
    # "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 
   date = as.Date(data[,1],format="%Y-%m-%d")
   data = zoo( data[,2:ncol(data)] , order.by = date )
   # "Bond"      "SP500"     "EAFE"      "SPGSCI"    "TBill"     "Inflation" 
   cAssets = ncol(data)-2; # number of risky assets
   # The loaded data has monthly frequency
   if(!nominalreturns){  monthlyR = data[,(1:(cAssets))]-data[,cAssets+2] }else{ monthlyR = data[,1:cAssets] }
   plot(monthlyR)
   if(nominalreturns){ save(monthlyR,file="monthlyR.RData") }else{ save(monthlyR,file="monthlyR_real.RData")  }
}else{
   if(nominalreturns){ load(file="monthlyR.RData") }else{ load(file="monthlyR_real.RData") } 
}

# Define rebalancing periods:

ep = endpoints(monthlyR,on='quarters')
# select those for estimation period
ep.start = ep[1:(length(ep)-estyears*4)]+1
from = time(monthlyR)[ep.start]
ep.end = ep[(1+estyears*4):length(ep)]
to = time(monthlyR)[ep.end]
nsamples = length(from);

#names of quarters for which the forecast is made: 
names.input = paste( c("Q1y_","Q2y_","Q3y_","Q4y_") , rep(seq( (firstyear+estyears),lastyear-1,1),each=4) , sep="" );
names.input = c( names.input , paste( c("Q1y_","Q2y_","Q3y_") , rep(lastyear,each=3) , sep="" ) );

#from = from[firstquarter:(length(from)-4+lastquarter)]; to = to[firstquarter:(length(to)-4+lastquarter)] 
#names.input = names.input[firstquarter:(length(names.input)-4+lastquarter)]

# Construction of rebalanced portfolios:


out = findportfolio.dynamic( R=monthlyR,  from=from, to=to, names.input=names.input, names.assets = colnames(monthlyR) , 
             p = p , priskbudget = priskbudget , mincriterion = mincriterion , 
             percriskcontribcriterion = percriskcontribcriterion , 
             strategy ,  controlDE = list( VTR = 0 , NP=200 , itermax = 200,trace=F ) )

write.table( out[[1]] , file = paste("weights/",strategy,".csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")

write.table( out[[2]] , file = paste("riskcont/",strategy,".csv",sep=""),
             append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,col.names = TRUE, qmethod = "escape")
