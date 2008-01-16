###############################################################################
# Functions to use the Rdonlp2 library to perform local maxima/minima search
# to refine the optimal criteria identified by brute force search.
#
# These methods take output from the functions in optimizer.R and then
# perform sequential quadratic programming to find better optimal portfolios.
#
# Copyright (c) 2008 Kris boudt and Brian G. Peterson
# Kindly contact the authors for permission to use these functions
###############################################################################
# $Id: localsearch.R,v 1.3 2008-01-16 13:40:38 brian Exp $
###############################################################################


# setwd("Y:/VaR/Cadiz/programs")
library("PerformanceAnalytics")
library("Rdonlp2");

# The dataset is edhec
# It contains from January 1997 on, the monthly returns on the following
# hedge fund investment style indices:
# edhec[,1]: Convertible.Arbitrage ; edhec[,2]: CTA.Global
# edhec[,3]: Distresses.Securities ; edhec [,4]: Emerging.Markets
# edhec[,5]: Equity.Market.Neutral ; edhec[,6]: Event.Driven
# edhec[,7]: Fixed.Income.Arbitrage ; edhec[,8]: Global.Macro
# edhec[,9]: Long.Short.Equity ; edhec[,10]: Merger.Arbitrage
# edhec[,11]: Relative.Value ; edhec[,12]: Short.Selling
# edhec[,13]: Funds.of.Funds


names = c( "2000", "2001", "2002","2003","2004","2005","2006")

# Because we require a training sample of at least 3 years,
# and the data is availaible from 1997,
# the first year we can calculate mean/risk analytics for is the year 2000


localsearch = function(R, weightgrid, from, to, names, cMin,
               criteria=c( "StdDev" , "SR.StdDev" ,
                        "GVaR", "SR.GVaR", "mVaR", "SR.mVaR",
                        "GES", "SR.GES", "mES", "SR.mES",   ... ),p=0.95,
               lowerbound = NA, upperbound = NA)
{ # @author Kris Boudt and Brian G. Peterson

    # Description:
    #
    # Performs a loop over the names.csv files (as many files as there are strings in names)
    # These files must be such that each row corresponds to the portfolio weight vector in the
    # respective row of weightgrid. Each column corresponds to an optimization criterion given
    # in correct order in methods
    #
    # @todo
    #
    # R                 data frame of historical returns
    #
    # weightgrid        each row contains one weighting vector, same number of columns as your returns
    #
    # names             vector holding the names of the .csv files to be read
    #
    # criteria	      the criterion to be optimized
    #
    # minormax          indicates whether the criterion should be minimized or maximized
    #
    # cMin              the number of local minima to be computed
    #
    # lowerbound, upperbound   a vector giving for each asset the lower and upper bounds
    # Return:
    # For each criterion a .csv file is saved holding for each period in `names' the best weight vector



    # Load the function donlp2
    library("Rdonlp2")

    # Create a matrix that will hold for each method and each vector the best weights

    cAssets = columns(weightgrid);
    cPeriods = length(names);
    cCriteria = length(criteria);

    out = matrix(  rep(0, cCriteria*cPeriods*cAssets) , ncol= cAssets );

    if(is.na(lowerbound)){ lowerbound = rep(-Inf,cAssets) }
    if(is.na(upperbound)){ upperbound = rep(Inf,cAssets) }

    # downside risk
    alpha = 1-p;

    # Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix

    fullR = R[1:to,];
    R = R[from:to,] ; in.sample.T = dim(R)[1];

    mu = apply( R , 2 , 'mean' )
    sigma = cov(R)
    invSigma = solve(sigma)
    M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
    M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3)
    for(t in c(1:in.sample.T))
    {
       centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
       M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
       M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
    }
    M3 = 1/in.sample.T*M3
    M4 = 1/in.sample.T*M4


    for( i in c(1:length(names)) ){
        Y = read.csv( file = paste( names[i],".csv",sep=""),
            header = TRUE,  sep = ",", na.strings = "NA", dec = ".")

        c=0;
        for( criterion in criteria ){
            c = c+1;
            switch( criterion,
                StdDev = {
                   # to be minimized
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   StdDevfun = function(w){
                       return(  sqrt( t(w)%*%sigma%*%w  ))
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=StdDevfun  , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = localoptim$fx ;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 SR.StdDev = {
                   # to be maximized
                   best=sort( Y[,c],index.return=T,decreasing=T)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   NegSR.StdDevfun = function(w){
                       return( - (t(w)%*%mu) / sqrt( t(w)%*%sigma%*%w  )    )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.StdDevfun , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx ;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                GVaR = {
                   # minimization criterion
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   GVaRfun = function(w){
                      return (- (t(w)%*%mu) - qnorm(p)*sqrt( t(w)%*%sigma%*%w  ) )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=GVaRfun  , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 SR.GVaR = {
                   # to be maximized
                   best=sort( Y[,c],index.return=T,decreasing=T)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   NegSR.GVaRfun = function(w){
                       GVaR = - (t(w)%*%mu) - qnorm(p)*sqrt( t(w)%*%sigma%*%w  )
                       return( - mean( R%*%weights ) / GVaR    )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GVaRfun , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                mVaR = {
                   # minimization criterion
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   mVaRfun = function(w){
                      pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                      skew = pm3 / pm2^(3/2);
                      exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                      h = z + (1/6)*(z^2 -1)*skew
                      h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
                      return (- (t(w)%*%mu) - h*sqrt( pm2  ) )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=mVaRfun  , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 SR.mVaR = {
                   # to be maximized
                   best=sort( Y[,c],index.return=T,decreasing=T)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   NegSR.mVaRfun = function(w){
                      pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                      skew = pm3 / pm2^(3/2);
                      exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                      h = z + (1/6)*(z^2 -1)*skew
                      h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
                      mVaR =  (- (t(w)%*%mu) - h*sqrt( pm2  ) )
                      return( - mean( R%*%weights ) / mVaR    )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mVaRfun , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                GES = {
                   # minimization criterion
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   GESfun = function(w){
                      return (- (t(w)%*%mu) + dnorm(qnorm(alpha))*sqrt(t(w)%*%sigma%*%w)/alpha )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=GESfun  , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = local.min;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 SR.GES = {
                   # to be maximized
                   best=sort( Y[,c],index.return=T,decreasing=T)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   NegSR.GESfun = function(w){
                       GES = - (t(w)%*%mu) + dnorm(qnorm(alpha))*sqrt(t(w)%*%sigma%*%w)/alpha
                       return( - (t(w)%*%mu) / GES    )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GESfun , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 mES = {
                   # minimization criterion
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];


                   Ipower = function(power,h){
                     fullprod = 1;
                     if( (power%%2)==0 ) #even number: number mod is zero
                     {
                        pstar = power/2;
                        for(j in c(1:pstar)){
                           fullprod = fullprod*(2*j)  }
                        I = fullprod*dnorm(h);

                        for(i in c(1:pstar) )
                        {
                           prod = 1;
                           for(j in c(1:i) ){
                              prod = prod*(2*j)  }
                           I = I + (fullprod/prod)*(h^(2*i))*dnorm(h)
                        }
                     }else{
                        pstar = (power-1)/2
                        for(j in c(0:pstar) ) {
                             fullprod = fullprod*( (2*j)+1 ) }
                        I = -fullprod*pnorm(h);
                        for(i in c(0:pstar) ){
                           prod = 1;
                           for(j in c(0:i) ){
                              prod = prod*( (2*j) + 1 )}
                          I = I + (fullprod/prod)*(h^(  (2*i) + 1))*dnorm(h) }
                       }
                       return(I) }

                     mESfun = function(w){
                       pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                       skew = pm3 / pm2^(3/2);
                       exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                       h = z + (1/6)*(z^2 -1)*skew
                       h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

                       E = dnorm(h)
                       E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
                       E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
                       E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
                       E = E/alpha

                       return (- (t(w)%*%mu) - sqrt(pm2)*min(-E,h) )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=mESfun  , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 },
                 SR.mES = {
                   # to be maximized
                   best=sort( Y[,c],index.return=T,decreasing=T)$ix;
                   bestweights = weightgrid[1:cMin , ];
                   global.best = Y[best[1],c] ; global.best.weight = bestweights[1,];

                   Ipower = function(power,h){
                     fullprod = 1;
                     if( (power%%2)==0 ) #even number: number mod is zero
                     {
                        pstar = power/2;
                        for(j in c(1:pstar)){
                           fullprod = fullprod*(2*j)  }
                        I = fullprod*dnorm(h);

                        for(i in c(1:pstar) )
                        {
                           prod = 1;
                           for(j in c(1:i) ){
                              prod = prod*(2*j)  }
                           I = I + (fullprod/prod)*(h^(2*i))*dnorm(h)
                        }
                     }else{
                        pstar = (power-1)/2
                        for(j in c(0:pstar) ) {
                             fullprod = fullprod*( (2*j)+1 ) }
                        I = -fullprod*pnorm(h);
                        for(i in c(0:pstar) ){
                           prod = 1;
                           for(j in c(0:i) ){
                              prod = prod*( (2*j) + 1 )}
                          I = I + (fullprod/prod)*(h^(  (2*i) + 1))*dnorm(h) }
                       }
                       return(I) }

                     NegSR.mESfun = function(w){
                       pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                       skew = pm3 / pm2^(3/2);
                       exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                       h = z + (1/6)*(z^2 -1)*skew
                       h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

                       E = dnorm(h)
                       E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
                       E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
                       E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
                       E = E/alpha
                       mES = - (t(w)%*%mu) - sqrt(pm2)*min(-E,h)
                       return ( - (t(w)%*%mu) / mES )
                   }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mESfun , lin.upper = upperbound , lin.lower = lowerbound )
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 }
             )#end function that finds out which criterion to optimize and does the optimization
             out[ (c-1)*cPeriods + i, ] = globalbest;
        }#end loop over optimization criteria
    }#end loop over .csv files containing the years


    # Output save
    for( i in c(1:cCriteria) ){
        criterion = criteria[i];
        write.table( out[c( ((i-1)*cPeriods+1)  : (i*cPeriods)  ),] , file = paste(criterion,".csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = "escape")
    }
}

###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################