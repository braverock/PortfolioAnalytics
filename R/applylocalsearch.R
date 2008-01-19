setwd("Y:/VaR/Cadiz/programs")

library("PerformanceAnalytics")
library("Rdonlp2");
source("localsearch.R")

# 1. Load return data

R=as.matrix(edhec[,c(2:12)]);
summary(R);

# 2. Load the weight vectors

weightgrid = read.csv( file = "weightingvectors_11_instr_5to50.csv" ,
               header = TRUE,  sep = ",", na.strings = "NA", dec = ".")

# For the 11 instrument portfolios, the portfolios are ordered in increasing concentration.
# The breakdown for the grid search is like this:
# Max Weight  First Row  Last Row   Num of Portfolios
# Equal Wt          1          1         1
# 10% max wt        2         56        55
# 15% max wt       57      19856     19800
# 20% max wt    19857      59951     40095
# 25% max wt    59952      81368     21417
# 30% max wt    81369      89233      7865
# 35% max wt    89234      91543      2310
# 40% max wt    91544      92148       605
# 45% max wt    92149      92258       110
# 50% max wt    92259      92269        11

# Based on the distribution of these portfolios, I suggest we constrain our portfolios to be 5% to 35%.  
# The small number of possible portfolios with 40%-50% max weights would most likely lead to unstable, 
# overly concentrated "corner" portfolios in the optimization, 
# while the larger numbers of portfolios available with lower weights should lead to more balanced and favorable results.

weightgrid = weightgrid[c(1:91543),]
weightgrid = read.table("weights10.txt",sep=",");
weightgrid = as.matrix(weightgrid);

lowerbound = rep(0.05,11);
upperbound = rep(0.35,11);

# 3. Specify the estimation period

# Because we require a training sample of at least 3 years,
# and the data is availaible from 1997,
# the first year we can calculate mean/risk analytics for is the year 2000

from = rep(1,2);
to = c(96, 108);

# 4. Specify the optimization criteria and in which column they are
# Available criteria =c( "StdDev" , "SR.StdDev" ,"GVaR", "SR.GVaR", "mVaR", "SR.mVaR", "GES", "SR.GES", "mES", "SR.mES",   ... )
criteria = c("mVaR","GES")
columns.crit = c(3,5);

# 5. Specify the names of the input and output files

names.input = c( "2004" , "2005" )
# 2004.csv is the input file containing for that year in column the criteria

names.output = c("mVaR_inception", "GES_inception")
# mVaR_inception.csv will be the output file containing for that criterion, for each year the optimal weights


# 6. Optimization: number of starting values to try

cMin = 5;

localsearch(R=R, weightgrid=weightgrid, from=from, to=to, names.input=names.input, names.output=names.output, cMin=cMin,
               criteria=criteria, columns.crit=columns.crit, p=0.95,
               lowerbound = lowerbound, upperbound = upperbound)



