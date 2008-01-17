###############################################################################
# Functions to use the Rdonlp2 library to perform local maxima/minima search
# to refine the optimal criteria identified by brute force search.
#
# These methods take output from the functions in optimizer.R and then
# perform sequential quadratic programming to find better optimal portfolios.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# Kindly contact the authors for permission to use these functions
###############################################################################
# $Id: localsearch.R,v 1.4 2008-01-17 21:24:59 kris Exp $
###############################################################################


localsearch = function(R, weightgrid, from, to, names, cMin,
               criteria=c( "StdDev" , "SR.StdDev" ,
                        "GVaR", "SR.GVaR", "mVaR", "SR.mVaR",
                        "GES", "SR.GES", "mES", "SR.mES",   ...), columns.crit, p=0.95,
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
    # from, to          define the estimation sample
    #
    # criteria	      the criterion to be optimized
    #
    # columns.crit      the columns of R in which the criteria are located
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

    cAssets = ncol(weightgrid);
    cPeriods = length(names);
    cCriteria = length(criteria);

    out = matrix(  rep(0, cCriteria*cPeriods*cAssets) , ncol= cAssets );
    # first cPeriods rows correspond to cCriteria[1] and so on

    if(any(is.na(lowerbound))){ lowerbound = rep(-Inf,cAssets) }
    if(any(is.na(upperbound))){ upperbound = rep(Inf,cAssets) }

    # downside risk
    alpha = 1-p;

    # Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix




    for( i in c(1:cPeriods) ){

       print(names[i]);

       in.sample.R = R[from[i]:to[i],] ; in.sample.T = dim(in.sample.R)[1];
       mu = apply( in.sample.R , 2 , 'mean' )
       sigma = cov(in.sample.R)
       M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
       M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3)
       for(t in c(1:in.sample.T))
       {
          centret = as.numeric(matrix(in.sample.R[t,]-mu,nrow=cAssets,ncol=1))
          M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
          M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
       }
       M3 = 1/in.sample.T*M3
       M4 = 1/in.sample.T*M4

        Y = read.csv( file = paste( names[i],".csv",sep=""),
            header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
        Y = Y[,columns.crit]
        c=0;
        for( criterion in criteria ){
            print(criterion);
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
                       localoptim = donlp2( par=bestweights[k,] , fn=StdDevfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.StdDevfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=GVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GVaRfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=mVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1))
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
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mVaRfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1))
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
                       localoptim = donlp2( par=bestweights[k,] , fn=GESfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( localoptim$fx < global.best){
                            global.best = localoptim$fx;  global.best.weight = localoptim$par };
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
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GESfun, par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=mESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets),lin.lower=c(1),lin.upper=c(1)) 
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
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mESfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=matrix(rep(1,6),nrow=1,ncol=6),lin.lower=c(1),lin.upper=c(1))
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima
                 }
             )#end function that finds out which criterion to optimize and does the optimization
             print(((c-1)*cPeriods + i));
             out[ ((c-1)*cPeriods + i), ] = global.best.weight;
             print(out);
             # first cPeriods rows correspond to cCriteria[1] and so on
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

# Revision 1.4  2008/01/17 13:40:38  Kris
# - fix some bugs
# - check on real data
# Revision 1.3  2008/01/16 13:40:38  brian
# - add CVS header and footer
# - add introduction and Copyright notice
#
###############################################################################