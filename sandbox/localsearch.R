###############################################################################
# Functions to use the Rdonlp2 library to perform local maxima/minima search
# to refine the optimal criteria identified by brute force search.
#
# These methods take output from the functions in optimizer.R and then
# perform sequential quadratic programming to find better optimal portfolios.
###############################################################################


localsearch = function(R, weightgrid, from, to, names.input, names.output, names.assets, cMin,
               criteria=c( "StdDev" , "SR.StdDev" ,
                        "GVaR", "SR.GVaR", "mVaR", "SR.mVaR",
                        "GES", "SR.GES", "mES", "SR.mES",   ...), columns.crit, p=0.95,
               lowerbound = NA, upperbound = NA, EW=F)
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
    # R                 matrix holding historical returns
    #
    # weightgrid        matrix each row contains one weighting vector, same number of columns as your returns
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
    #
    # EW                  optimization conditional on optimized portfolio return > EW return
    # intuition: max modSharpe s.t. Return>EW is a good objective function because it says that an investor will prefer an optimizer portfolio 
        #  only when they have a view that is better than the neutral (EW) view
    # Return:
    # For each criterion a .csv file is saved holding for each period in `names' the best weight vector


    print("Local optimization of portfolio risk using a SQP solver")
    print("Constraints: sum of weights=1 and user can specify bound constraints")
    print("--------------------------------------------------------------------")
    print("");

    R = checkData(R, method="matrix");
    weightgrid = checkData(weightgrid, method="matrix");


    # Load the function donlp2
    library("Rdonlp2")

    # Create a matrix that will hold for each method and each vector the best weights

    cAssets = ncol(weightgrid);
    cPeriods = length(names.input);
    cCriteria = length(criteria);

    out = matrix(  rep(0, cCriteria*cPeriods*cAssets) , ncol= cAssets );
    # first cPeriods rows correspond to cCriteria[1] and so on

    if(any(is.na(lowerbound))){ lowerbound = rep(-Inf,cAssets) }
    if(any(is.na(upperbound))){ upperbound = rep(Inf,cAssets) }

    # downside risk
    alpha = 1-p;

    # Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix


    
    if(EW){EWweights = matrix(rep(1,cAssets)/cAssets,ncol=1)}else{
        A=matrix(rep(1,cAssets),nrow=1,ncol=cAssets);lin.lower=c(1);lin.upper=c(1);}



    for( per in c(1:cPeriods) ){

       print("-----------New estimation period ends on------------------------")
       print(names.input[per]);
       print("----------------------------------------------------------------")
       print("")
       in.sample.R = R[from[per]:to[per],] ; in.sample.T = dim(in.sample.R)[1];
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

        Y = read.csv( file = paste( names.input[per],".csv",sep=""),
            header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
        Y = Y[1:dim(weightgrid)[1],columns.crit]
        c=0;

       if(EW){ 
            A=matrix( rbind( rep(1,cAssets), mu) ,nrow=2,ncol=cAssets);
            lin.lower=c(1,t(EWweights)%*%mu);lin.upper=c(1,Inf);}

        for( c in c(1:cCriteria) ){
             criterion = criteria[c]
             print("-----------New criterion----------------------")
             print(criterion);
             print("----------------------------------------------")
             print("")
             switch( criterion,
                StdDev = {
                   # to be minimized
                   best=sort( Y[,c],index.return=T,decreasing=F)$ix;
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   StdDevfun = function(w){
                       return(  sqrt( t(w)%*%sigma%*%w  ))
                   }
                   if( abs( ( global.best - StdDevfun(global.best.weight) )/StdDevfun(global.best.weight)    )>0.05    ){
                       print("error StdDev definition"); break;}
                   if(EW){ 
                       localoptim = donlp2( par=EWweights , fn=StdDevfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                                if( localoptim$fx < global.best){ global.best = localoptim$fx ;  global.best.weight = localoptim$par } } }
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=StdDevfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = -Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   NegSR.StdDevfun = function(w){
                       return( - (t(w)%*%mu) / sqrt( t(w)%*%sigma%*%w  )    )
                   }
                   if( abs( ( global.best - NegSR.StdDevfun(global.best.weight) )/NegSR.StdDevfun(global.best.weight)    )>0.05    ){
                        print("error SR.StdDev definition"); break; }
                   if(EW){
                      localoptim = donlp2( par=EWweights , fn=NegSR.StdDevfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                          if( localoptim$fx < global.best){
                               global.best = localoptim$fx ;  global.best.weight = localoptim$par }}}
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.StdDevfun , par.upper = upperbound , par.lower = lowerbound,
                                           A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   GVaRfun = function(w){
                      return (- (t(w)%*%mu) - qnorm(alpha)*sqrt( t(w)%*%sigma%*%w  ) )
                   }
                   if( abs( ( global.best - GVaRfun(global.best.weight) )/GVaRfun(global.best.weight)    )>0.05    ){
                       print("error GVaR definition");break;}
                   if(EW){
                      localoptim = donlp2( par=EWweights , fn=GVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                              if( localoptim$fx < global.best){ 
                                  global.best = localoptim$fx ;  global.best.weight = localoptim$par }}}
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=GVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                           A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = -Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   NegSR.GVaRfun = function(w){
                       GVaR = - (t(w)%*%mu) - qnorm(alpha)*sqrt( t(w)%*%sigma%*%w  )
                       return( - (t(w)%*%mu) / GVaR    )
                   }
                   if( abs( ( global.best - NegSR.GVaRfun(global.best.weight) )/NegSR.GVaRfun(global.best.weight)    )>0.05    ){
                        print("error SR GVaR definition");break;}
                   if(EW){
                      localoptim = donlp2( par=EWweights , fn=NegSR.GVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                      if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                          if( localoptim$fx < global.best){
                              global.best = localoptim$fx ;  global.best.weight = localoptim$par } }}
                    for( k in c(1:cMin) ){
                        localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GVaRfun , par.upper = upperbound , par.lower = lowerbound,
                                           A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   mVaRfun = function(w){
                      pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                      skew = pm3 / pm2^(3/2);
                      exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                      h = z + (1/6)*(z^2 -1)*skew
                      h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
                      return (- (t(w)%*%mu) - h*sqrt( pm2  ) )
                   }
                   if( abs( ( global.best - mVaRfun(global.best.weight) )/mVaRfun(global.best.weight)    )>0.05    ){
                      print("error mVaR definition");break;}
                   if(EW){
                     localoptim = donlp2( par=EWweights , fn=mVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                     if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                        if( localoptim$fx < global.best){
                               global.best = localoptim$fx ;  global.best.weight = localoptim$par } } }
                    for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=mVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                           A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = -Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   NegSR.mVaRfun = function(w){
                      pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                      skew = pm3 / pm2^(3/2);
                      exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
                      h = z + (1/6)*(z^2 -1)*skew
                      h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
                      mVaR =  (- (t(w)%*%mu) - h*sqrt( pm2  ) )
                      return( - (t(w)%*%mu) / mVaR    )
                   }
                   if( abs( ( global.best - NegSR.mVaRfun(global.best.weight) )/NegSR.mVaRfun(global.best.weight)    )>0.05    ){
                        print("error SR mVaR definition");break}
                   if(EW){
                      localoptim = donlp2( par=EWweights , fn=NegSR.mVaRfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                      if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                          if( localoptim$fx < global.best){
                               global.best = localoptim$fx ;  global.best.weight = localoptim$par }}}
                  
                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mVaRfun , par.upper = upperbound , par.lower = lowerbound,
                                           A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin], ];
                   global.best = Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   GESfun = function(w){
                      return (- (t(w)%*%mu) + dnorm(qnorm(alpha))*sqrt(t(w)%*%sigma%*%w)/alpha )
                   }
                   if( abs( ( global.best - GESfun(global.best.weight) )/GESfun(global.best.weight)    )>0.05    ){
                       print("error GES definition"); break; }
                   if(EW){
                       localoptim = donlp2( par=EWweights , fn=GESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                           if( localoptim$fx < global.best){
                            global.best = localoptim$fx ;  global.best.weight = localoptim$par }}}
                     for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=GESfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = -Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

                   NegSR.GESfun = function(w){
                       GES = - (t(w)%*%mu) + dnorm(qnorm(alpha))*sqrt(t(w)%*%sigma%*%w)/alpha
                       return( - (t(w)%*%mu) / GES    )
                   }
                   if( abs( ( global.best - NegSR.GESfun(global.best.weight) )/NegSR.GESfun(global.best.weight)    )>0.05    ){
                        print("error SR GES definition"); break; }
                   if(EW){
                      localoptim = donlp2( par=EWweights , fn=NegSR.GESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                          if( localoptim$fx < global.best){
                            global.best = localoptim$fx ;  global.best.weight = localoptim$par }}}
                      for( k in c(1:cMin) ){
                          localoptim = donlp2( par=bestweights[k,] , fn=NegSR.GESfun, par.upper = upperbound , par.lower = lowerbound,
                                        A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));


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
                   if( abs( ( global.best - mESfun(global.best.weight) )/mESfun(global.best.weight)    )>0.05    ){
                        print("error mES definition"); break; }
                   if(EW){
                     localoptim = donlp2( par=EWweights , fn=mESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                     if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){ 
                        if( localoptim$fx < global.best){
                            global.best = localoptim$fx ;  global.best.weight = localoptim$par }}   }
                     for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=mESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
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
                   bestweights = weightgrid[best[1:cMin] , ];
                   global.best = -Y[best[1],c] ; global.best.weight = as.numeric(matrix(bestweights[1,],ncol=1));

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
                       return(I) 
                   }

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
                   if( abs( ( global.best - NegSR.mESfun(global.best.weight) )/NegSR.mESfun(global.best.weight)    )>0.05    ){
                     print("error SR mES definition"); break; }
                   if(EW){
                     localoptim = donlp2( par=EWweights , fn=NegSR.mESfun  , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                     if(  (localoptim$message == "KT-conditions satisfied, no further correction computed")  |
                            (localoptim$message == "computed correction small, regular case") |
                            (localoptim$message == "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){
                        if( localoptim$fx < global.best){
                               global.best = localoptim$fx ;  global.best.weight = localoptim$par }}  }


                   for( k in c(1:cMin) ){
                       localoptim = donlp2( par=bestweights[k,] , fn=NegSR.mESfun , par.upper = upperbound , par.lower = lowerbound,
                                          A=A,lin.lower=lin.lower,lin.upper=lin.upper)
                       if(  (localoptim$message != "KT-conditions satisfied, no further correction computed")  &
                            (localoptim$message != "computed correction small, regular case") &
                            (localoptim$message != "stepsizeselection: x (almost) feasible, dir. deriv. very small" ) ){next;}
                       if( -localoptim$fx > global.best){
                            global.best = -localoptim$fx;  global.best.weight = localoptim$par };
                   }#end loop over local minima, indexed by k=1,...,cMin
                 }
             )#end function that finds out which criterion to optimize and does the optimization

             print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
             print(c("end optimization of criterion",criterion,"for period",names.input[per]))
             print("Local search has improved the objective function from");
             print(Y[best[1],c] );
             print("to");
             print( global.best );

             out[ ((c-1)*cPeriods + per), ] = as.vector(global.best.weight)

             if ( EW  ){ 
               if ( (t(global.best.weight)%*%mu) <= (t(EWweights)%*%mu) ){ out[ ((c-1)*cPeriods + per), ] = as.vector(EWweights) } }
             # first cPeriods rows correspond to cCriteria[1] and so on

        }#end loop over optimization criteria; indexed by c=1,...,cCriteria

    }#end loop over .csv files containing the years; indexed by per=1,...,cPeriods


    # Output save
    for( i in c(1:cCriteria) ){
        criterion = criteria[i];
        output = as.data.frame( out[c( ((i-1)*cPeriods+1)  : (i*cPeriods)  ),] );
        rownames(output) = names.input;
        colnames(output) = names.assets;
        write.table( output , file = paste(names.output[i],".csv",sep=""),
            append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = "escape")
    }
}

MonthlyReturns.LocalSearch =
function (R, criteria, from =37, to=120, by=12, method = c("compound") )
{ # @author Brian G. Peterson, Kris Boudt

    # R                 data structure of component returns
    #
    # criteria           vector containing the names of the csv files holding the portfolio weights 
    #
    # from, to          Monthly returns are computed for R[from:to,]  
    #
    # by                Frequency of changing portfolio weights
    #

    # Setup:
    result=NULL
    resultcols=NULL

    # data type conditionals
    # cut the return series for from:to
    if (class(R) == "timeSeries") {
        R = R@Data
    } else {
        R = R
    }
    R=checkData(R,method="zoo")
    cRebalancing = ceiling( (to-from+1)/12);



    result.compound=c()
    result.simple=c()
     # Loop over the different optimisation criteria

     for(criterion in criteria){
        pcompoundreturn=psimplereturn=c();
        weights = read.csv( file = paste( criterion,".csv",sep=""),
                 header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
        if (ncol(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")
        portfoliovalue = 1;
        for (row in 1:cRebalancing){
               start = from+(row-1)*by;
               end = min(from + row*by - 1,to);
               wealthindex = cumvalue( R[start:end,], weights=weights[row,] )
               compoundreturns = portfoliovalue*wealthindex - 1;
               pcompoundreturn=c(pcompoundreturn, compoundreturns )
               portfoliovalue = portfoliovalue*wealthindex[12]
               simplereturns = c(wealthindex[1],wealthindex[2:12]/wealthindex[1:11] ) - 1;
               psimplereturn = c( psimplereturn , simplereturns ); 
        }
        result.compound = cbind(result.compound, pcompoundreturn)
        result.simple = cbind(result.simple, psimplereturn)
     }
     colnames(result.compound)=  colnames(result.simple) = criteria
     rownames(result.compound)=  rownames(result.simple) = rownames(R)[from:to]
     LSmonthlyportcompoundreturns = result.compound
     LSmonthlyportsimplereturns = result.simple

     save(LSmonthlyportcompoundreturns, file="compoundreturns.Rdata"   )

     save(LSmonthlyportcompoundreturns, file="simplereturns.Rdata"  )
}

cumvalue <- function (R, weights=NULL)
{   # @author Brian G. Peterson,Kris Boudt
    # Setup:
    R=checkData(R,method="zoo")

    if (is.null(weights)){
        # set up an equal weighted portfolio
        weights = t(rep(1/ncol(R), ncol(R)))
    }

    if (ncol(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    wealthindex.assets= cumprod( 1+R )  

    # build a structure for our weighted results
    wealthindex.weighted = matrix(nrow=nrow(R),ncol=ncol(R))
    colnames(wealthindex.weighted)=colnames(wealthindex.assets)
    rownames(wealthindex.weighted)=rownames(wealthindex.assets)

    # weight the results
    for (col in 1:ncol(weights)){
        wealthindex.weighted[,col]=weights[,col]*wealthindex.assets[,col]
    }
    wealthindex=apply(wealthindex.weighted,1,sum)

    return(wealthindex)
} # end 

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
# Revision 1.14  2009-09-22 21:22:48  peter
# - added licensing details
#
# Revision 1.13  2008-01-31 13:51:11  kris
# Corrected portfolio return calculations
#
# Revision 1.10  2008/01/22 18:51:15  kris
# Corrected mistake in GVaR definition of localsearch.R and added row and columnnames to output
#
# Revision 1.9  2008/01/21 11:13:39  kris
# Fixed mistakes, built in checks that verify compatibility between grid of weights, the criteria values obtained by the grid search and the local search code
#
# Revision 1.7  2008/01/19 23:53:33  brian
# - fix checkData for weightgrid
#
# Revision 1.6  2008/01/19 16:00:05  kris
# *** empty log message ***
#
# Revision 1.4  2008/01/19 13:40:38  Kris
# - add checkData functionality
# Revision 1.4  2008/01/17 13:40:38  Kris
# - fix some bugs
# - check on real data
# Revision 1.3  2008/01/16 13:40:38  brian
# - add CVS header and footer
# - add introduction and Copyright notice
#
###############################################################################