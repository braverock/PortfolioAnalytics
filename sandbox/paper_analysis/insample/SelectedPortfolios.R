setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs/insample")
#setwd("c:/Documents and Settings/n06054/Desktop/risk budget programs")
cAssets = 4; p = priskbudget = 0.95;
mincriterion = "mES" ; percriskcontribcriterion = "mES";

# Load programs

source("Risk_budget_functions.R"); 
library(zoo); library(fGarch); library("PerformanceAnalytics"); library("PortfolioAnalytics")
clean = TRUE
CC = TRUE

#   Load the data
firstyear = 1976 ; firstquarter = 1; lastyear = 2010; lastquarter = 2; 
data = read.table( file= paste(getwd(),"/data.txt",sep="") ,header=T)
date = as.Date(data[,1],format="%Y-%m-%d")

monthlyR = zoo( data[,2:(1+cAssets)] , order.by = date )

###############################################
# Difference min var vs min cvar for alpha
###############################################

clmonthlyR = clean.boudt2(monthlyR,alpha=0.05)[[1]] 
mu = apply(clmonthlyR,2,'mean')
sigma = cov(clmonthlyR)
if(!CC){
  M3 = PerformanceAnalytics:::M3.MM(clmonthlyR) 
  M4 = PerformanceAnalytics:::M4.MM(clmonthlyR) 
}else{
  source( paste( getwd(),"/R_allocation/coskewkurtosis.R" ,sep="") ) 
  M3 = coskewCC(clmonthlyR)  ; M4 = cokurtCC(clmonthlyR) ;
}

eps = 0.025
rpconstraint<-constraint(assets=N, min_sum=(1-eps), max_sum=(1+eps), 
      min=rep(0,N), max=rep(1,N), weight_seq=generatesequence(),by=.001,rounding=3)
rp<- random_portfolios(rpconstraints=rpconstraint,permutations=200)
rp <-rp/rowSums(rp)
controlDE <- list(reltol=1e-6,steptol=150, itermax = 5000,trace = 100, strategy=6, c=.4,
                NP=as.numeric(nrow(rp)),initialpop=rp) 
sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                Riskupper = Inf ,  mu = mu, sigma = sigma, M3=M3, M4=M4, 
                alpha = 0.05, alphariskbudget = 0.05, controlDE = controlDE )

print(sol) # [[1]] weights, [[2]] mean , [[3]] risk, [[4]] perc risk allocation [[5]] absolute risk alloc

[1] "out = list( minw , sum( minw*mu ) , prisk(minw) , percriskcontrib(minw)"
[[1]]
        par1         par2         par3         par4 
0.7910335681 0.0001698627 0.0622884156 0.1465081536 

[[2]]
[1] 0.006331481

[[3]]
           [,1]
[1,] 0.01049456

[[4]]
[1]  1.2092963044  0.0003311551  0.0242432012 -0.2338706607

[[5]]
[1]  1.269103e-02  3.475327e-06  2.544218e-04 -2.454370e-03

sol = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = T, percriskcontribcriterion = "mES" , 
                Riskupper = Inf ,  mu = mu, sigma = sigma, M3=M3, M4=M4, 
                alpha = 0.05, alphariskbudget = 0.05, controlDE = controlDE )

[[1]]
      par1       par2       par3       par4 
0.71887893 0.06307400 0.07585579 0.14219128 

[[2]]
[1] 0.006542418

[[3]]
           [,1]
[1,] 0.01220722

[[4]]
[1] 0.2501506 0.2497638 0.2496753 0.2504103

[[5]]
[1] 0.003053645 0.003048923 0.003047842 0.003056814

sol = PortfolioOptim(  minriskcriterion = "StdDev" , MinMaxComp = F, percriskcontribcriterion = "StdDev" , 
                Riskupper = Inf ,  mu = mu, sigma = sigma, M3=M3, M4=M4 , optimize_method = "quadprog" )

[1] "out = list( minw , sum( minw*mu ) , prisk(minw) , percriskcontrib(minw)"
[[1]]
[1] 0.84043528 0.03862133 0.03643427 0.08450912

[[2]]
[1] 0.006421218
# > 0.006421218*12
# [1] 0.07705462
[[3]]
           [,1]
[1,] 0.01433945

[[4]]
[1] 0.84043528 0.03862133 0.03643427 0.08450912

[[5]]
[1] 0.012051380 0.000553809 0.000522447 0.001211814



