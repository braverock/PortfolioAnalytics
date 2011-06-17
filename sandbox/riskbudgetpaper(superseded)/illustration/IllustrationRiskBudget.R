

# 1. Set working directory
setwd("C:/Documents and Settings/Administrator/Desktop/risk budget programs/illustration");

#      Load libraries  
library(chron); library("tseries"); library(zoo)
library(PerformanceAnalytics)

nameseries = c("IBM","WMT")
for( series in nameseries ){
   close <- get.hist.quote(instrument=series,retclass="zoo",quote="AdjClose", compression="m") 
   close = window( close , start =as.Date("1989-12-01") , end = as.Date("2009-12-31" ) )
    if(series==nameseries[1]){ P = close }else{ P = merge(P , close ) }
}
colnames(P ) = nameseries; 
P = na.omit(P)   ; head(P) ; plot(P) ;
R = diff(log(P)) ; head(R) ; plot(R) ;

pfreturns = rowSums(R)/2;
IBMdate = time(R)
pfVaR   = VaR(R=R,method="modified",weights=c(0.5,0.5))


postscript("IBMWMTexample.eps")

par(mfrow=c(3,1),mar=c(2,2,0,2),cex=1)

ylim=c( min(R)-0.01,max(R)+0.01)
plot(IBMdate,pfreturns,type="h",main="",xlab="",ylab="",ylim=ylim, xaxt="n")
text( x=IBMdate[5], y = ylim[2]-0.01, labels="Monthly returns on portfolio with 50% IBM and 50% WMT" ,pos=4)
abline( h=-pfVaR$MVaR, lty=1 ,lwd=3 , col="blue"); 
text( IBMdate[1],y = -pfVaR$MVaR-0.1, labels="95% Portfolio VaR",pos=4, col="blue",cex=0.8 )

par(mar=c(2,2,0,2),cex=1)

plot( IBMdate,R$IBM,type="h",main="",xlab="",ylab="",ylim=ylim, xaxt="n")
text( IBMdate[5],y = ylim[2]-0.01, labels="Monthly returns on IBM stock" ,pos=4 )
abline( h=-pfVaR$MVaR, lty=1 ,lwd=3 , col="blue"); 
#text( IBMdate[1],y = -pfVaR$MVaR-0.15, labels="95% Portfolio VaR",pos=4, col="blue",cex=0.8 )

par(mar=c(2,2,0,2),cex=1 )

plot(IBMdate,R$WMT,type="h",main="",xlab="",ylab="",ylim=ylim)
text( x=IBMdate[5], y = ylim[2]-0.01, labels="Monthly returns on WMT stock" ,pos=4 )
abline( h=-pfVaR$MVaR, lty=1 ,lwd=3 , col="blue"); 
#text( IBMdate[1],y = -pfVaR$MVaR-0.05, labels="95% Portfolio VaR",pos=4, col="blue",cex=0.8 )

dev.off()

VaR(R=R,method="modified",weights=c(0.5,0.5),portfolio_method="component")
ES(R=R,method="modified",weights=c(0.5,0.5))