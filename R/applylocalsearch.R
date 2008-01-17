setwd("Y:/VaR/Cadiz/programs")

library("PerformanceAnalytics")
library("Rdonlp2");
source("localsearch.R")

R=edhec[,c(2,4:12)]
summary(R);

weightgrid = read.table("weights10.txt",sep=",")
names = c( "2004" , "2005" )
# Because we require a training sample of at least 3 years,
# and the data is availaible from 1997,
# the first year we can calculate mean/risk analytics for is the year 2000

from = rep(1,2);
to = c(96, 108);

criteria = c("mVaR","GES")
columns.crit = c(3,4);
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 4 should be 5 but strangely enough there is a data problem when I take 5

lowerbound = rep(0.05,10);
upperbound = rep(0.25,10);

Y = read.csv( file = paste( names[1],".csv",sep=""),
            header = TRUE,  sep = ",", na.strings = "NA", dec = ".")

# Available criteria =c( "StdDev" , "SR.StdDev" ,"GVaR", "SR.GVaR", "mVaR", "SR.mVaR", "GES", "SR.GES", "mES", "SR.mES",   ... )

localsearch(R=R, weightgrid=weightgrid, from=from, to=to, names=names, cMin=2,
               criteria=criteria, columns.crit=columns.crit, p=0.95,
               lowerbound = lowerbound, upperbound = upperbound)



