# 1. Set working directory
# setwd("C:/Documents and Settings/n06054/Desktop/PhDDefence");
setwd("Y:/PhDDefence");
#      Load libraries  
library(chron)

# Asymmetrie and fat tails in weekly returns IBM

postscript("monthlyreturns.eps")
data=read.table("monthlyIBM.txt",skip=1,nrows=-1)
IBMdate = chron( as.character(data[,1]),format=c(dates="d/m/y") ); IBMreturns = data[,2];
z99 = qnorm(0.99);
start=261; end=length(IBMdate);
IBMdate = IBMdate[start:end] ; IBMreturns = IBMreturns[start:end]; print(length(IBMdate));
band = mean(IBMreturns)+z99*sd(IBMreturns)
par(mfrow=c(2,1),mar=c(2,2,3,2),cex=1.3)
maxvalue = 1.05*max( abs( min(IBMreturns) ) , IBMreturns );
range = c(-maxvalue,maxvalue)
plot(IBMdate,IBMreturns,type="h",main="Monthly returns on IBM stock",xlab="",ylab="",ylim=range)
abline( h= band , lty=3, col="blue" ,lwd=3 ); abline(h=-band , lty=3 , col="blue",lwd=3)
plot(IBMdate, rnorm(length(IBMdate), mean=mean(IBMreturns),sd=sd(IBMreturns)),
          main="Simulated returns from a normal distribution" ,type="h",ylim=range)
abline( h=band, lty=3, col="blue" ,lwd=3 ); abline(h=-band , lty=3 , col="blue" ,lwd=3)
dev.off()

library(PerformanceAnalytics);
modifiedVaR( data[,2] )
VaR.Beyond( data[,2] , p=0.99 ); VaR.Beyond( data[,2] , modified=T, p=0.99 ); 

postscript("monthlyreturnsVaR.eps")
data=read.table("monthlyIBM.txt",skip=1,nrows=-1)
IBMdate = chron( as.character(data[,1]),format=c(dates="d/m/y") ); IBMreturns = data[,2];
z99 = qnorm(0.99);
start=261; end=length(IBMdate);
IBMdate = IBMdate[start:end] ; IBMreturns = IBMreturns[start:end]; print(length(IBMdate));
band = mean(IBMreturns)+z99*sd(IBMreturns)
par(mfrow=c(1,1),mar=c(2,2,3,2),cex=1.3)
maxvalue = 1.05*max( abs( min(IBMreturns) ) , IBMreturns );
range = c(-maxvalue,maxvalue)
plot(IBMdate,IBMreturns,type="h",main="Monthly returns on IBM stock \n and expected loss in 1% worst cases",xlab="",ylab="",ylim=range)
#abline( h= band , lty=3 ); abline(h=-band , lty=3 )

GVaR = VaR.traditional( data[,2] , p=0.99 );
MVaR = modifiedVaR( data[,2] , p=0.99 ) ;
GES  = VaR.Beyond( data[,2] , p=0.99 ); 
MES =  VaR.Beyond( data[,2] , modified=T, p=0.99 ); 

abline( h = - GES , lty=1, col="red",lwd=3 ); text(  IBMdate[5] , y = -0.9*GES , labels = "Normal approach" , pos=4, col="red",lwd=3)
abline( h = - MES , lty=1, col="darkgreen",lwd=3 ); 
text(  IBMdate[5] , y = -0.93*MES , labels = "Modified approach" , pos=4 , col="darkgreen",lwd=3)
dev.off()

# Portfolio risk:

dataIBM=read.table("monthlyIBM.txt",skip=1,nrows=-1)
IBMdate = chron( as.character(dataIBM[,1]),format=c(dates="d/m/y") ); IBMreturns = dataIBM[,2];
start=261; end=length(IBMdate);
IBMdate = IBMdate[start:end] ; IBMreturns = IBMreturns[start:end]; print(length(IBMdate));

dataUSTnote=read.table("monthlyUS10yrsnote.txt",skip=1,nrows=-1)
USTnotedate = chron( as.character(dataUSTnote[,1]),format=c(dates="d/m/y") ); USTnotereturns = dataUSTnote[,2];
start=261; end=length(USTnotedate);
USTnotedate = USTnotedate[start:end] ; USTnotereturns = USTnotereturns[start:end]; print(length(USTnotedate));

maxvalue = 1.05*max( abs( min(IBMreturns,USTnotereturns) ) , IBMreturns,USTnotereturns );range = c(-maxvalue,maxvalue)

pfreturns = 0.5*(USTnotereturns+IBMreturns);
MES =  VaR.Beyond( pfreturns , modified=T, p=0.99 ); 


source("functions-managers_changed.R");

data = cbind( IBMreturns , USTnotereturns )
mu = apply(data,2,'mean'); sigma = cov(data)
invSigma = solve(sigma); N=2;
M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)
T = length(IBMreturns)
for(t in c(1:T))
{
      centret = as.numeric(matrix(data[t,]-mu,nrow=N,ncol=1))
      M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
      M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
}
   M3 = M3/T
   M4 = M4/T

PortgausES(alpha=0.01,w=rep(1,2)/2,mu=mu,sigma=sigma,)
PortMES(alpha=0.01,w=rep(1,2)/2,mu=mu,sigma=sigma,M3=M3,M4=M4)
MES =  VaR.Beyond( pfreturns , modified=T, p=0.99 ); 


postscript("riskcontributions.eps")
par(mfrow=c(3,1),mar=c(2,2,0,2),cex=1.3)

plot(IBMdate,pfreturns,type="h",main="",xlab="",ylab="",ylim=range, xaxt="n")
text( x=IBMdate[5], y = 0.9*range[2], labels="Monthly returns on portfolio with 50% IBM and 50% 10 yrs US T-Note" ,pos=4)
abline( h=-MES, lty=1 ,lwd=3 , col="blue"); 
text( IBMdate[1],y = -1.3*MES, labels="1% Portfolio modES",pos=4, col="blue",cex=0.8 )

par(mar=c(2,2,0,2),cex=1.3)

plot(IBMdate,IBMreturns,type="h",main="",xlab="",ylab="",ylim=range, xaxt="n")
text( IBMdate[5],y = 0.9*range[2], labels="Monthly returns on IBM stock" ,pos=4 )
text( IBMdate[1],y = -1.3*MES, labels="1% Portfolio modES",pos=4, col="blue" ,cex=0.8)

abline( h=-MES, lty=1 ,lwd=3 , col="blue" ); 

par(mar=c(2,2,0,2),cex=1.3 )

plot(IBMdate,USTnotereturns,type="h",main="",xlab="",ylab="",ylim=range)
text( x=IBMdate[5], y = 0.9*range[2], labels="Monthly returns on 10yrs US T-Note" ,pos=4 )
abline( h=-MES, lty = 1,lwd=3 , col="blue" ); 
text( IBMdate[1],y = -1.3*MES, labels="1% Portfolio modES",pos=4 , col="blue",cex=0.8)

dev.off()


######################################################################

# Intraweek pattern

###################################################################
#  Conditional normality and jumps                                #
#  Authors: Kris Boudt, Christophe Croux and Sebastien Laurent    #
###################################################################

source("seasonalityfunctions15augustbis.R")

# load data

EUR = read.table("EUR_USD_5minEST_1987_2004.txt",skip=1)
EUR.dow = read.table("EUR_USD_5minDOW_1987_2004.txt",skip=1)
#EUR.date=EUR[1,];
subset = c(3452:4371)  # January 2001 till September 20, 2004
#subset = c(3201:4371)  # January 2000 till September 20, 2004
#subset = c(1956:4371)  # January 1995 till September 20, 2004
#subset= c(4127:4371)   #   October 1, 2003 till September 30, 2004
EUR = EUR[subset,]
EUR.date=EUR[,1]; 
EUR = EUR[,c(2:dim(EUR)[2])];
cDays = length(EUR.date) 
EUR.dow = EUR.dow[subset,2];
#EUR.dow = EUR.dow[,2];
EUR.dowdummies=matrix(rep(0,cDays*5),ncol=5);
for(d in 1:5){ EUR.dowdummies[EUR.dow==d,d]=1 }

EUR = as.matrix(EUR); vEUR = as.vector(EUR); nobs = length(vEUR)

rob.stdEUR= standardize(data=EUR , method="LM" )     ; rob.vstdEUR = as.vector(rob.stdEUR)

SD.seas.scale = seasonality(  stddata = rob.stdEUR, approach="scale" ,  method = "sd", standardize=T  );
AROWVar.seas.scale = seasonality(  stddata = rob.stdEUR, approach="scale" ,  method = "AROWVar", standardize=T  );

range = c( 0.9*min(AROWVar.seas.scale, SD.seas.scale) ,  1.1*max(AROWVar.seas.scale, SD.seas.scale)    )
cIntraDay = length(SD.seas.scale)

#range=c( 0.9*min(SD.seas.scale) , 1.1*max(SD.seas.scale) )


times = c(1,103,199,288)
names = c("16:00","24:00","8:30","15:55")

postscript("SDseasonality.eps")

par(mfrow=c(1,1),mar=c(2,2,3,2),cex=1.3 ,cex.axis=1)
par( mar=rep(2,4)  )
# plot robust seasonality estimates
plot(c(1:cIntraDay),SD.seas.scale,type="l",xlab="",ylab="" , xaxt="n", ylim=range,
     main="Intraday pattern in volatility of 5-min EUR/USD returns" )
axis(1, at=times , tick=T, labels= names)
dev.off()


AROWVar.seas.scale = seasonality(  stddata = rob.stdEUR, approach="scale" ,  method = "AROWVar", standardize=T  );

cIntraDay = length(AROWVar.seas.scale)
#range=c( 0.9*min(AROWVar.seas.scale) , 1.1*max(AROWVar.seas.scale) )


times = c(1,103,199,288)
names = c("16:00","24:00","8:30","15:55")

postscript("dailyscaleseasonality3.eps")

par(mfrow=c(1,1),mar=c(2,2,3,2),cex=1.3 ,cex.axis=1)
par( mar=rep(2,4)  )
# plot robust seasonality estimates
plot(c(1:cIntraDay),AROWVar.seas.scale,type="l",xlab="",ylab="" , xaxt="n", ylim=range,
     main="Intraday pattern in volatility of 5-min EUR/USD returns" )
axis(1, at=times , tick=T, labels= names)
dev.off()


# Time-varying volatility of Daily returns IBM

data=read.table("IBM.txt",skip=1,nrows=-1)
IBMdate = chron( as.character(data[,1]),format=c(dates="d/m/y") ); IBMreturns = data[,2];
start=5449; end=length(IBMdate);
IBMdate = IBMdate[start:end] ; IBMreturns = IBMreturns[start:end]; print(length(IBMdate));
z99 = qnorm(0.99)
band = mean(IBMreturns)+z99*sd(IBMreturns)
data=read.table("monthlyIBM.txt",skip=1,nrows=-1)

range = c(-maxvalue,maxvalue)
postscript("dailyreturns.eps")
par(mfrow=c(2,1),mar=c(2,2,3,2),cex=1.3)
maxvalue = 1.05*max( abs( min(IBMreturns) ) , IBMreturns );
plot(IBMdate,IBMreturns,type="h",main="Daily returns on IBM stock",xlab="",ylab="",ylim=range)
abline( h=band, lty=3 ); abline(h=-band , lty=3 )
plot(IBMdate, rnorm(length(IBMdate), mean=mean(IBMreturns),sd=sd(IBMreturns)),
          main="Simulated returns from a normal distribution" ,type="h",ylim=range)
abline( h=band, lty=3 ); abline(h=-band , lty=3 )
dev.off()

# Commonality daily returns IBM and MSFT


dataIBM=read.table("IBM.txt",skip=1,nrows=-1)
IBMdate = chron( as.character(dataIBM[,1]),format=c(dates="d/m/y") ); IBMreturns = dataIBM[,2];
start=5449; end=length(IBMdate);
IBMdate = IBMdate[start:end] ; IBMreturns = IBMreturns[start:end]; print(length(IBMdate));

dataGE=read.table("dailyGE.txt",skip=1,nrows=-1)
GEdate = chron( as.character(dataGE[,1]),format=c(dates="d/m/y") ); GEreturns = dataGE[,2];
start=5449; end=length(GEdate);
GEdate = GEdate[start:end] ; GEreturns = GEreturns[start:end]; print(length(GEdate));

z99 = qnorm(0.99)
band = mean(IBMreturns)+z99*sd(IBMreturns)
data=read.table("monthlyIBM.txt",skip=1,nrows=-1)

maxvalue = 1.05*max( abs( min(IBMreturns,GEreturns) ) , IBMreturns,GEreturns );range = c(-maxvalue,maxvalue)

postscript("commonality.eps")
par(mfrow=c(2,1),mar=c(2,2,3,2),cex=1.3)

plot(IBMdate,IBMreturns,type="h",main="Daily returns on IBM stock",xlab="",ylab="",ylim=range)
abline( h=band, lty=3 ); abline(h=-band , lty=3 )

band = mean(GEreturns)+z99*sd(GEreturns)
plot(GEdate,GEreturns,type="h",main="Daily returns on GE stock",xlab="",ylab="",ylim=range)

abline( h=band, lty=3 ); abline(h=-band , lty=3 )
dev.off()



# Price jumps

data=read.table("intraday.txt",skip=1,nrows=-1)
EUR = data[,1]; GBP = data[,2];

loc = c( 1 , 6*12 , 12*12 , 18*12 , 24*12 )
names = c( "00:05 GMT" , "06:00" , "12:00" , "18:00" , "24:00" ) 

postscript("intraday.eps")
par(mfrow=c(2,1),mar=c(2,2,3,2),cex=1.3)

plot(c(1:288),EUR,type="l",main="5-min EUR/USD FX rates on June 9, 2003",xlab="",ylab="", xaxt="n")
axis( side = 1 , at = loc , labels = names)

plot(c(1:288),GBP,type="l",main="5-min GBP/USD FX rates on June 9, 2003",xlab="",ylab="",xaxt="n")
axis( side = 1 , at = loc , labels = names)

dev.off()

# Effect on correlation

 RQCov=read.table(file="dailyRQCov_EURGBP.txt"); 
 vRQCov11=RQCov[,2]; vRQCov22=RQCov[,3]; vRQCov12=RQCov[,4]; vRQCor=RQCov[,5];
 RBPCov=read.table(file="dailyRBPCov_EURGBP.txt");
 vRBPCov11=RBPCov[,2]; vRBPCov22=RBPCov[,3]; vRBPCov12=RBPCov[,4]; vRBPCor= RBPCov[,5];
 ROWQCov=read.table(file="dailyMCDROWCov_EURGBP.txt"); 
 vROWQCov11=ROWQCov[,2]; vROWQCov22=ROWQCov[,3]; vROWQCov12=ROWQCov[,4]; vROWQCor=ROWQCov[,5]; 
 dailymaxoutlyingness = ROWQCov[,6];
 cDays=length(RQCov); days=read.csv2("days.csv",sep=";",skip=0,header=F);
 days=paste(as.character(days[[1]]),as.character(days[[2]]),as.character(days[[3]]),sep="/"); 
 days = chron(days,format=c(dates="y/m/d"))

day1 = 4014;

subset1 = c(3907:4152) # April - June 1998
subset1days = days[subset1] ; # cbind(subset1, as.character(subset1days)  )

times = c( 3907 , 4014 , 4152 );
names = c("Jan,3", "June,9", "Dec,30") # subset1days[times]
subset1days = 1:length (subset1) ; # 63 observation

size = 1.3

vRQCor[day1];vRBPCor[day1];vROWQCor[day1];

postscript("correlation.ps", family="Times", horizontal=FALSE, width=12,height=6)
   par(mfrow=c(3,1),cex.axis=1.3,cex.lab=1.3)
   par(mar=c(1.5,2.2,0.7,2)) # c(bottom, left, top, right)

   range=c( min(vRQCor[subset1],vRBPCor[subset1],vROWQCor[subset1]) , 
       1.1*max(vRQCor[subset1],vRBPCor[subset1],vROWQCor[subset1])  )

   plot( subset1, vRQCor[subset1]   , xlab="", type="l",lwd=3 ,ylab="" , ylim= range , xaxt="n"  )
   text(x=subset1[2],y=0.98*range[2],labels="Daily RQCor",adj=c(0,.75),cex=size)
   abline(h=0.2,lty=3) ; abline(h=0.4,lty=3);abline(h=0.6,lty=3);

   #range=c( min(vRBPCor[subset1]) , 1.1*max(vBPCor[subset1])  )
   plot( subset1 , vRBPCor[subset1]   , xlab="", type="l",lwd=3 ,ylab="" , ylim= range  , xaxt="n" )
   abline(h=0.2,lty=3) ; abline(h=0.4,lty=3);abline(h=0.6,lty=3);
   text(x=subset1[2] ,y=0.98*range[2],labels="Daily RBPCor",adj=c(0,.75),cex=size)

   par(mar=c(2.2,2.2,0,2)) # c(bottom, left, top, right)

   #range=c( min(vMCDOWCor[subset1]) , 1.1*max(vMCDOWCor[subset1])  )
   plot( subset1 , vROWQCor[subset1]   , xlab="", type="l",lwd=3 ,ylab="" , ylim= range , xaxt="n"  )
   axis(1, at=times , tick=T, labels= names ,cex.axis=size )
   text(x=subset1[2] ,y=0.98*range[2],labels="Daily ROWQCor",adj=c(0,.75),cex=size)
   abline(h=0.2,lty=3) ; abline(h=0.4,lty=3);abline(h=0.6,lty=3);

dev.off()

postscript("correlation2.eps")
par(mfrow=c(2,1),mar=c(2,2,3,2),cex=1.3)

plot( subset1 ,vRBPCor[subset1] , xlab="", type="l",lwd=3 , main=" 'robust' daily correlation", ylab="" ,
                 ylim= range  , xaxt="n" )
   axis(1, at=times , tick=T, labels= names ,cex.axis=size )
plot( subset1 ,vROWQCor[subset1] , xlab="", type="l",lwd=3 , main=" daily correlation using new method ", ylab="" ,
                 ylim= range  , xaxt="n" )
   axis(1, at=times , tick=T, labels= names ,cex.axis=size )
dev.off()
