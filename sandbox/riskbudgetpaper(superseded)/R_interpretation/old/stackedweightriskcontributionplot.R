#################################################################################
# Create stacked risk contributions plot
#################################################################################

# ! Set your working directory (folder containing the subfolders R_allocation, R_interpretation, data, weights, etc)

setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")


# Options:
################

# Choose data

datacase = "equitybondscommodity"# "intequitybonds" # equitybondscommodity

percriskcontribcriterion = "mES"

# specify the optimization criteria you want to analyze
# crit1 = "StdDev"; crit2="SR.StdDev" 
crit1 = "mES"; crit2="SR.mES" 

# specify the number of years used for the estimation
estyears = 4;

# specify the risk budget constraint
RBconstraint = 0.4; 

# Load programs

source("R_interpretation/chart.StackedBar.R"); 
library(zoo);  library(PerformanceAnalytics)

# number of risky assets
firstyear = 1976 ; firstquarter = 1; lastyear = 2009; lastquarter = 2; 
cAssets = 4 
 

# frequency of rebalancing: yearly of quarterly
frequency = "quarterly"
# Load portfolio weights:
criterion = paste(crit1,".",estyears,"yr",sep="")
weightsEW = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/EW.csv" , sep="")  );
weightsconst.crit1 = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/", criterion , "-Inf",RBconstraint,".csv" , sep="")  );
weightsInf.crit1 = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/",criterion , "-InfInf.csv" , sep="")  );
weightsconst.crit1 = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/", criterion , "-Inf",RBconstraint,".csv" , sep="")  );
weightsERisk = read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/unconstrained/", criterion , 0.22,0.28,".csv" , sep="")  );
weightsMinCVaR =  read.csv( file = paste("weights/", percriskcontribcriterion,"/",datacase,"/",frequency,"/unconstrained/", criterion , 0.22,0.28,".csv" , sep="")  );

# Load percentage risk contributions:

riskcontribEW = read.csv( file = paste("riskcont/", percriskcontribcriterion,"/",datacase,"/",frequency,"/EW.csv" , sep="")  );
criterion = paste("riskcont/",percriskcontribcriterion,"/",datacase,"/",frequency,"/",crit1,".",estyears,"yr",sep="")
riskcontribInf.crit1 = read.csv( file = paste( criterion , "-InfInf.csv" , sep="")  );
riskcontribconst.crit1 = read.csv( file = paste( criterion , "-Inf",RBconstraint,".csv" , sep="")  );
criterion = paste("riskcont/",percriskcontribcriterion,"/",datacase,"/",frequency,"/unconstrained/",crit1,".",estyears,"yr",sep="")
riskcontribERisk = read.csv( file = paste( criterion , 0.22,0.28,".csv" , sep="")  );
riskcontribMinCVaR = read.csv( file = paste( criterion , -Inf,Inf,".csv" , sep="")  );

# Is there cash in any of the portfolios due to infeasibility of the risk budget constraints?

if( any(weightsconst.crit1[,(1+cAssets)]!=0) | any(weightsERisk[,(1+cAssets)]!=0) ){
     if(datacase=="intequitybonds"){ w.names = c( "US bond" , "MSCI Europe" , "MSCI Japan", "MSCI US" ,  "T-Bill" ) };
     if(datacase=="equitybondscommodity"){ w.names = c( "US bond" , "S&P 500" , "MSCI EAFE" , "S&P GSCI",  "T-Bill" ) };
     colorset = gray( seq(0,(cAssets),1)/5 ) ; #c(gray.colors(11,start=0,end=1)
}else{
     if(datacase=="intequitybonds"){    w.names = c( "US bond" , "MSCI Europe" , "MSCI Japan", "MSCI US"   ) };
     if(datacase=="equitybondscommodity"){ w.names = c( "US bond" , "S&P 500", "MSCI EAFE"  , "S&P GSCI" ) };
     weightsInf.crit1 = weightsInf.crit1[,1:cAssets]; 
     weightsconst.crit1 = weightsconst.crit1[,1:cAssets]; 
     weightsERisk = weightsERisk[,1:cAssets]; 
     weightsMinCVaR = weightsMinCVaR[,1:cAssets]; 
     colorset = gray( seq(0,(cAssets-1),1)/cAssets ) ; #c(gray.colors(11,start=0,end=1)
}

# Relabel rownames in the plots

if(frequency=="yearly"){
   names = seq( (firstyear+estyears),lastyear+1,1) 
}else{
   names = paste(rep( seq( (firstyear+estyears),lastyear,1) , each=4),c("Q1","Q2","Q3","Q4"),sep="")  
   names = c( names , paste( lastyear+1, "Q1" , sep="" ) )  
   names = names[firstquarter:(length(names)-4+lastquarter)]
}

rownames(riskcontribEW) = rownames(riskcontribInf.crit1) = rownames(riskcontribconst.crit1) = rownames(riskcontribERisk) = rownames(riskcontribMinCVaR) = names;  

#due to rounding, the sum of the risk contributions is sometimes 1 + epsilon: avoid this in plot

riskcontribEW = riskcontribEW/rowSums(riskcontribEW)

riskcontribInf.crit1 = riskcontribInf.crit1/rowSums(riskcontribInf.crit1);
riskcontribconst.crit1 = riskcontribconst.crit1/rowSums(riskcontribconst.crit1);
riskcontribERisk = riskcontribERisk/rowSums(riskcontribERisk)
riskcontribMinCVaR = riskcontribMinCVaR/rowSums(riskcontribMinCVaR)

    # make weights versus risk allocation plot

postscript( file=paste("fig/",percriskcontribcriterion,"/",datacase,"/",frequency,"/stackedweightriskcontinf.eps",sep="") )


   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height=c(0.15,2,2*1.4,0.7), width=1)
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "min StdDev" , cex.main=1.3 )
   par(mar=c(0,4,2,1) , cex.main=1)
   barplot(t(weightsInf.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", 
          cex.axis=1, cex.lab=1, cex.main=1, axisnames=F )

   par(mar=c(1,4,1,1.1))
   chart.StackedBar2(riskcontribInf.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.2,1.2))
   par(mar=c(0.1,4,1,1.1))
   plot.new()
   legend("center",legend=w.names,fill=colorset,cex=1,ncol=4)
   
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "max Sharpe ratio", cex.main=1.3  )
   par(mar=c(0,4,2,1.1) , cex.main=1)
   barplot(t(weightsInf.crit2),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F)
   par(mar=c(1,4,1,1.1))
   chart.StackedBar2(riskcontribInf.crit2,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.2,1.2))
dev.off()

    # make weights versus risk allocation plot

postscript( file=paste("fig/",percriskcontribcriterion,"/",datacase,"/",frequency,"/stackedweightriskcontconst.eps",sep="") )

   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height=c(0.15,2,2*1.2,0.6), width=1)
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "min StdDev" , cex.main=1.3 )
   par(mar=c(0,4,2,1) , cex.main=1)
   barplot(t(weightsconst.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F)

   par(mar=c(1,4,2,1.1))
   chart.StackedBar2(riskcontribconst.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL)
   par(mar=c(0.1,4,1,1.1))
   plot.new()
   legend("center",legend=w.names,fill=colorset,cex=1,ncol=4)
   
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "max Sharpe ratio", cex.main=1.3  )
   par(mar=c(0,4,2,1.1) , cex.main=1)
   barplot(t(weightsconst.crit2),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F)
   par(mar=c(1,4,1,1.1))
   chart.StackedBar2(riskcontribconst.crit2,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL)

dev.off()


postscript( file=paste("fig/",percriskcontribcriterion,"/",datacase,"/",frequency,"/stackedweightriskcontconstminCVaR.eps",sep="") )

   layout( matrix(  c(1,2,3,4,5,6,7,4),  ncol = 2 ) , height=c(0.15,1.6,1.6*2,0.6), width=1)
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "Investment weight constrained" , cex.main=1.3 )
   par(mar=c(0,4,2,1) , cex.main=1)
   barplot(t(weightsInf.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F)

   par(mar=c(1,4,2,1.1))
   chart.StackedBar2(riskcontribInf.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4))
   par(mar=c(0.1,4,1,1.1))
   plot.new()
   legend("center",legend=w.names,fill=colorset,cex=1,ncol=4)
   
   par(mar=c(0.1,4,1,1.1) , cex.main=1)
   plot.new()
   title( main = "Risk budget constrained", cex.main=1.3  )
   par(mar=c(0,4,2,1.1) , cex.main=1)
   barplot(t(weightsconst.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F)
   par(mar=c(1,4,1,1.1))
   chart.StackedBar2(riskcontribconst.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4) )


dev.off()

mar1 =c(0.1,l,1,1.1)
mar2 =c(0,l,2,1)
mar3 = c(1,l,2,1.1)
postscript( file=paste("fig/",percriskcontribcriterion,"/",datacase,"/",frequency,"/stackedweightriskcontconstminCVaR.eps",sep="") )
   l = 4

   layout( matrix(  c(1,2,3,4,5,6,7,4,8,9,10,4,11,12,13,4,14,15,16,4),  ncol = 5 ) , height= c(0.15,1.5,1.5*2,0.6), width=1)

   par(mar=mar1 , cex.main=1)
   plot.new()
   title( main = "Equal-weighted" , cex.main=1.3 )
   par(mar=mar2 , cex.main=1)
   barplot(t(weightsEW),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1,  cex.main=1, axisnames=F,border = F )

   par(mar=mar3)
   chart.StackedBar2(riskcontribEW,axisnames=T, colorset=colorset,space=0, cex.names=1.3,    
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4),border = F)

   par(mar=mar1)

   plot.new()
   legend("center",legend=w.names,fill=colorset,cex=1,ncol=4)

   plot.new()
   title( main = "Min CVaR" , cex.main=1.3 )
   par(mar=mar2 , cex.main=1)
   barplot(t(weightsMinCVaR),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1,  cex.main=1, axisnames=F,border = F )

   par(mar=mar3)
   chart.StackedBar2(riskcontribMinCVaR,axisnames=T, colorset=colorset,space=0, cex.names=1.3,    
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4),border = F)

   par(mar=mar1 , cex.main=1)
   plot.new()
   title( main = "Min CVaR, w <= 0.4" , cex.main=1.3 )
   par(mar=c(0,l,2,1) , cex.main=1)
   barplot(t(weightsInf.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F,border = F)

   par(mar=mar2)
   chart.StackedBar2(riskcontribInf.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,   
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4),border = F)
   par(mar=mar3)

  # plot.new()
   #legend("center",legend=w.names,fill=colorset,cex=1,ncol=4)
   
   par(mar=mar1 , cex.main=1)
   plot.new()
   title( main = "Min CVaR, %Contrib CVaR <= 0.4", cex.main=1.3  )
   par(mar=mar2 , cex.main=1)
   barplot(t(weightsconst.crit1),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1,  cex.main=1, axisnames=F,border =F)
   par(mar=mar3)
   chart.StackedBar2(riskcontribconst.crit1,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4),border = F )


   par(mar=mar1 , cex.main=1)
   plot.new()
   title( main = "Equal %Contrib to CVaR", cex.main=1.3  )
   par(mar=mar2 , cex.main=1)
   barplot(t(weightsERisk),col=colorset,space=0, cex=.7, main = "Weight allocation", ylab="", cex.axis=1, cex.lab=1, cex.main=1, axisnames=F,border = F)
   par(mar=mar3)
   chart.StackedBar2(riskcontribERisk,axisnames=T, colorset=colorset,space=0, cex.names=1,  
          main = "Risk allocation", ylab="",cex.axis=1.1, cex.lab=1, cex.main=1,legend.loc = NULL, ylim=c(-0.4,1.4),border = F )

dev.off()





