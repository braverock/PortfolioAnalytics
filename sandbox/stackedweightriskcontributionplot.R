#################################################################################
# Create stacked weights and risk contributions plot
#################################################################################

# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")


# Options:
################

# specify the number of years used for the estimation
estyears = 8;

# Load programs

source("R_interpretation/chart.StackedBar.R"); 
library(zoo);  library(PerformanceAnalytics)

# number of risky assets
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
cAssets = 4 
 
# "MinRisk_ReturnTarget"  "EqualRisk"
# "MinRiskConc_ReturnTarget"

names = c(  "EqualWeight"  ,"MinRisk" , "MinRisk_PositionLimit" ,"MinRisk_RiskLimit" ,
            "MinRiskConc" , "MinRiskConc_PositionLimit"  , "MinRisk_ReturnTarget" , "MinRiskConc_ReturnTarget" )

namelabels = c(  "Equal Weight"  , "Min CVaR" ,  "Min CVaR + Position Limit"  ,"Min CVaR + CVaR Alloc Limit"  , 
                 "Min CVaR Concentration" ,"Min CVaR Concentration + Position Limit" , "Min CVaR + Return Target" ,"Min CVaR Concentration + Return Target"    ) 

# frequency of rebalancing: yearly of quarterly
frequency = "quarterly"
# Load portfolio weights:
weightsS1 = read.csv( file = paste("weights/", names[1], ".csv" , sep="")  );
weightsS2 = read.csv( file = paste("weights/", names[2], ".csv" , sep="")  );
weightsS3 = read.csv( file = paste("weights/", names[3], ".csv" , sep="")  );
weightsS4 = read.csv( file = paste("weights/", names[4], ".csv" , sep="")  );
weightsS5 = read.csv( file = paste("weights/", names[5], ".csv" , sep="")  );
weightsS6 = read.csv( file = paste("weights/", names[6], ".csv" , sep="")  );
weightsS7 = read.csv( file = paste("weights/", names[7], ".csv" , sep="")  );
weightsS8 = read.csv( file = paste("weights/", names[8], ".csv" , sep="")  );

# Load percentage risk contributions:
riskcontS1 = read.csv( file = paste("riskcont/", names[1], ".csv" , sep="")  );
riskcontS2 = read.csv( file = paste("riskcont/", names[2], ".csv" , sep="")  );
riskcontS3 = read.csv( file = paste("riskcont/", names[3], ".csv" , sep="")  );
riskcontS4 = read.csv( file = paste("riskcont/", names[4], ".csv" , sep="")  );
riskcontS5 = read.csv( file = paste("riskcont/", names[5], ".csv" , sep="")  );
riskcontS6 = read.csv( file = paste("riskcont/", names[6], ".csv" , sep="")  );
riskcontS7 = read.csv( file = paste("riskcont/", names[7], ".csv" , sep="")  );
riskcontS8 = read.csv( file = paste("riskcont/", names[8], ".csv" , sep="")  );

# Relabel rownames in the plots

if(frequency=="yearly"){
   names = seq( (firstyear+estyears),lastyear+1,1) 
}else{
   names = paste(rep( seq( (firstyear+estyears),lastyear,1) , each=4),c("Q1","Q2","Q3","Q4"),sep="")  
   names = c( names , paste( lastyear+1, "Q1" , sep="" ) )  
   names = names[firstquarter:(length(names)-4+lastquarter)]
}


rownames(weightsS1) = rownames(weightsS2) = rownames(weightsS3) = rownames(weightsS4) = names;
rownames(weightsS5) = rownames(weightsS6) = rownames(weightsS7) = rownames(weightsS8) = names; 

rownames(riskcontS1) = rownames(riskcontS2) = rownames(riskcontS3) = rownames(riskcontS4) = names; 
rownames(riskcontS5) = rownames(riskcontS6) = rownames(riskcontS7) = rownames(riskcontS8) = names;  


 colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; 
#due to rounding, the sum of the risk contributions is sometimes 1 + epsilon: avoid this in plot

riskcontS1 = riskcontS1/rowSums(riskcontS1); riskcontS2 = riskcontS2/rowSums(riskcontS2); 
riskcontS3 = riskcontS3/rowSums(riskcontS3); riskcontS4 = riskcontS4/rowSums(riskcontS4); 
riskcontS5 = riskcontS5/rowSums(riskcontS5); riskcontS6 = riskcontS6/rowSums(riskcontS6); 
riskcontS7 = riskcontS7/rowSums(riskcontS7); riskcontS8 = riskcontS8/rowSums(riskcontS8);

w.names = c( "US bond" , "S&P 500", "EAFE"  , "GSCI" )
 l = 2
mar1 =c(2,l,2,1.1)
mar2 =c(0,l,2,1)
mar3 = c(3,l+1,3,0.1)
mar4 = c(2,l+1,2,0.1)

# Stacked weights plot: 
postscript('stackedweightsriskcont_benchmark.eps') 
   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(weightsS1,col=colorset,space=0,  main = namelabels[1], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS2,col=colorset,space=0,  main = namelabels[2], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS5,col=colorset,space=0,  main = namelabels[5], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,fill=colorset,ncol=4)



   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(riskcontS1,col=colorset,space=0,  main = namelabels[1], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS2,col=colorset,space=0,  main = namelabels[2], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS5,col=colorset,space=0,  main = namelabels[5], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()

postscript('MinCVaR_alternatives.eps') 
   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height= c(1.5,1.5,1.5,0.7), width=1)

   par(mar=mar3 , cex.main=1)
   chart.StackedBar2(weightsS3,col=colorset,space=0,  main = namelabels[3], ylab="Weight allocation", las=1, l=3.9, r=0, cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T, legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS4,col=colorset,space=0,  main = namelabels[4], ylab="Weight allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(weightsS6,col=colorset,space=0,  main = namelabels[6], ylab="Weight allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

   par(mar=mar1 , cex.main=1)
   plot.new()
   legend("center",legend=w.names,fill=colorset,ncol=4)
   par(mar=mar3 , cex.main=1)

   chart.StackedBar2(riskcontS3,col=colorset,space=0,  main = namelabels[3], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS4,col=colorset,space=0,  main = namelabels[4], ylab="CVaR allocation", las=1, l=3.9, r=0,  cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )
   chart.StackedBar2(riskcontS6,col=colorset,space=0,  main = namelabels[6], ylab="CVaR allocation", las=1, l=3.9, r=0,   cex.axis=1, cex.lab=1,  cex.main=1, axisnames=T,legend.loc = NULL,ylim=c(0,1),border = F )

dev.off()
