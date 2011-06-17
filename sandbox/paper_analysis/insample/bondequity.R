setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs/insample")

# Equal risk portfolio
cAssets = 4; # number of assets influences the cleaning
p = priskbudget = 0.95;

mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("PortfolioAnalytics")
clean = TRUE; CC = T

#   Load the data
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste(getwd(),"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

monthlyR = zoo( data[,2:(1+cAssets)] , order.by = date )

set.seed(1234)
if(clean){ monthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]] }
monthlyR = monthlyR[,1:2]
mu = apply(monthlyR,2,'mean')
sigma = cov(monthlyR)
# N = 2 : no need for CC
M3 = PerformanceAnalytics:::M3.MM(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  ); 
M4 = PerformanceAnalytics:::M4.MM(monthlyR-matrix( rep(as.numeric(mu),nrow(monthlyR)) , nrow=nrow(monthlyR) , byrow=TRUE)  )

N = ncol(monthlyR)

# Summary stats individual assets

apply(monthlyR,2,'mean')*12
apply(monthlyR,2,'sd')*sqrt(12)
apply(monthlyR,2,'skewness')
apply(monthlyR,2,'kurtosis')



mESfun2 = function( w ){ return( operPortMES(w,mu=mu,alpha=0.05,sigma=sigma,M3=M3,M4=M4)[[1]]   ) }
assetCVaR = rep(0,2);
for( i in 1:2 ){
  w = rep(0,2); w[i]=1;
  assetCVaR[i] = mESfun2( as.matrix(w) )
}
assetCVaR


#################################################################################
# Make Exhibit 2 Risk budget paper: weight and CVaR allocation static portfolios
#################################################################################

indexes = monthlyR

# Equal-weight portfolio
w5050 <- c(0.5,0.5)
sum( w5050*mu*12 ) ;
#VaR(R=indexes[,1:2], weights=w5050, portfolio_method="component")
ES(R=indexes[,1:2], weights=w5050, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)

# 60/40 bond equity allocation
w6040 <- c(0.6,0.4); 
sum( w6040*mu*12 ) ; 
#VaR(R=indexes[,1:2], weights=w6040, portfolio_method="component")
ES(R=indexes[,1:2], weights=w6040, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)


# Min CVaR portfolio


library(DEoptim)
obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 ES(R=indexes[,1:2],weights = matrix(w,ncol=1), mu = mu, sigma = sigma, m3=M3, m4=M4,invert=FALSE)
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
wMinCVaR <- wstar / sum(wstar)
print(wMinCVaR)
#      par1       par2 
#0.96183602 0.03816398
# wMinCVaR = c( 0.96864323 , 0.03135677 )
print(sum(wMinCVaR*mu*12))
ES(R=indexes[,1:2], weights=matrix(wMinCVaR,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)


# Min CVaR Concentration portfolio

obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 CVaR <- ES(R=indexes[,1:2], weights=matrix(w,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)
 out <- max(CVaR$contribution)
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
wMinCVaRConc <- wstar / sum(wstar) 
print(wMinCVaRConc)
#> print(wMinCVaRConc)
#     par1      par2 
#0.7751015 0.2248985
# wMinCVaRConc = c( 00.7700542 , 0.2299458   )
print(sum(wMinCVaRConc*mu*12))
ES(R=indexes[,1:2], weights=wMinCVaRConc, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)



# 60/40 Risk allocation portfolio
obj <- function(w) {
 if (sum(w) == 0) { w <- w + 1e-2 }
 w <- w / sum(w)
 CVaR <- ES(R=indexes[,1:2], weights=matrix(w,ncol=1),portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)
 tmp1 <- CVaR$MES
 tmp2 <- max(CVaR$pct_contrib_MES - c(0.601,0.401 ) , 0)
 tmp3 <- max(c(0.599,0.399 ) - CVaR$pct_contrib_MES , 0)
 out  <- tmp1 + 1e3 * tmp2 + 1e3 * tmp3
}
out <- DEoptim(fn = obj, lower = rep(0, 2), upper = rep(1, 2), 
               DEoptim.control(itermax=100))                    
wstar <- out$optim$bestmem
w6040riskalloc <- wstar / sum(wstar) 
print(w6040riskalloc)
#      par1      par2 
#0.8193828 0.1806172  
print(sum(w6040riskalloc*mu*12))
# w6040riskalloc = c( 0.7290461 , 0.2709539 ) 
ES(R=indexes[,1:2], weights=w6040riskalloc, portfolio_method="component", mu = mu, sigma = sigma, m3=M3, m4=M4)










