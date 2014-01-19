
library("PerformanceAnalytics")
#library("Rdonlp2");
warning("this code requires package Rdonlp2, which is no longer on CRAN due to licencing issues")
#source("localsearch.R")
detach(package:fEcofin)
data("edhec")
# 1. Load return data

R=edhec[,1:11]
summary(R);

names.assets =colnames(edhec[,1:11]);

# 2. Load the weight vectors

weightgrid = read.csv( file = "weightingvectors_11_instr_5to50.csv" ,
               header = FALSE,  sep = ",", na.strings = "NA", dec = ".")
colnames(weightgrid)=colnames(edhec[,2:12])

# header is FALSE, otherwise you skip 1 weight vector

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

weightgrid = weightgrid[c(1:91543),]  # 30\%
#weightgrid = weightgrid[c(1:89233),]

lowerbound = rep(0.025,11);
upperbound = rep(0.35,11);

# 3. Specify the estimation period

# Because we require a training sample of at least 3 years,
# and the data is availaible from 1997,
# the first year we can calculate mean/risk analytics for is the year 2000

to = seq( from= 3*12 , to = 10*12 , by=12 );

ind3yr=T
if(ind3yr){
   from = to-35
   # 4. Specify the names of the input and output files
   names.input = c( "1999.3yr","2000.3yr","2001.3yr","2002.3yr","2003.3yr","2004.3yr","2005.3yr","2006.3yr"  )
   # 2004.3yr.csv is the input file containing for that year in column the criteria
   names.output = c("GVaR.3yr","SR.GVaR.3yr","modVaR.3yr","SR.modVaR.3yr",
      "GES.3yr","SR.GES.3yr","modES.3yr","SR.modES.3yr","StdDev.3yr","SR.StdDev.3yr")
}else{
   from=rep(1,8)
   # 4. Specify the names of the input and output files
   names.input = c( "1999","2000","2001","2002","2003","2004","2005","2006"  )
   # 2004.csv is the input file containing for that year in column the criteria
   names.output = c("GVaR.inception","SR.GVaR.inception","modVaR.inception","SR.modVaR.inception",
      "GES.inception","SR.GES.inception","modES.inception","SR.modES.inception","StdDev.inception","SR.StdDev.inception")
   # mVaR_inception.csv will be the output file containing for that criterion, for each year the optimal weights
}


# 5. Specify the optimization criteria and in which column they are
# Available criteria =c( "StdDev" , "SR.StdDev" ,"GVaR", "SR.GVaR", "mVaR", "SR.mVaR", "GES", "SR.GES", "mES", "SR.mES",   ... )

criteria = c( "GVaR", "SR.GVaR", "mVaR", "SR.mVaR", "GES", "SR.GES", "mES", "SR.mES" ,  "StdDev" , "SR.StdDev")

columns.crit = c(1:10);

# output = read.csv( file = paste(names.input[1],".csv",sep=""), header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
# summary(output)
# summary(output[columns.crit])




# 6. Optimization: number of starting values to try

cMin = 10;  # at least 2

localsearch(R=R, weightgrid=weightgrid, from=from, to=to, names.input=names.input, names.output=names.output, names.assets = names.assets, 
            cMin=cMin, criteria=criteria, columns.crit=columns.crit, p=0.95, lowerbound = lowerbound, upperbound = upperbound, EW=F)



# create equalweighted
cAssets = 11; cYears=length(names.input)
output = as.data.frame( matrix( rep(1/cAssets, cAssets*cYears) , nrow=cYears)  );
rownames(output) = names.input;
colnames(output) = names.assets;
write.table( output , file = "equalweighted.csv", append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = "escape")

###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.9  2009-09-22 21:24:14  peter
# - applied cvs log and licensing details
#
###############################################################################
