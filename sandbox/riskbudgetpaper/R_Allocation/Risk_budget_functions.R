
PortfolioOptim = function( minriskcriterion = "mES" , MinMaxComp = F, percriskcontribcriterion = "mES" , 
                     R = NULL, mu = NULL , sigma = NULL, M3=NULL,M4=NULL, alpha = 0.05, alphariskbudget = 0.05,
                     lower = NULL , upper = NULL, Riskupper = NULL, Returnlower = NULL, RBlower = NULL , RBupper = NULL, precision = 1e-3 , 
                     controlDE = list( VTR = 0 , NP=200, trace = FALSE ) , heuristic=TRUE , penalty = NULL ){

     # Description: 
     # This function produces the portfolio that minimimizes 
     # either the portfolio risk (when MinMaxComp = F) or the portfolio concentration (when MinMaxComp = T) 
     # subject to
     # lower <= weights <= upper
     # risk <= Riskupper
     # expected return >= Returnlower 
     # RBlower <= percentage risk <= RBupper

     # Input: 
     # Either the multivariate return series is given or estimates of the mean, covariance, coskewness or cokurtosis
     require(zoo); require(PerformanceAnalytics);   require(DEoptim)

     if( !is.null(R) ){
          R = clean.boudt2( R , alpha = alphariskbudget )[[1]];
          T = nrow(R);N=ncol(R)
          mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
          sigma = cov(R);
          M3 = PerformanceAnalytics:::M3.MM(R)
          M4 = PerformanceAnalytics:::M4.MM(R)
     }else{ N = length(mu) }

     if( is.null(lower)     ){ lower = rep(0,N)      }  ; if( is.null(upper)       ){ upper = rep(1,N)     }
     if( is.null(RBlower)   ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper)     ){ RBupper = rep(Inf,N) }
     if( is.null(Riskupper) ){ Riskupper = Inf       }  ; if( is.null(Returnlower) ){ Returnlower = -Inf   }  

     switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ return( Portsd(w,mu=mu,sigma=sigma)                                           ) }},
                GVaR   = { percriskcontrib = function(w){ return( PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)                ) }},
                GES    = { percriskcontrib = function(w){ return( PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)                 ) }},
                mVaR   = { percriskcontrib = function(w){ return( PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)       ) }},
                mES    = { percriskcontrib = function(w){ return( operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)    ) }}
     ) #end function that finds out which percentage risk contribution criterion to use
     switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( stddevfun(w,mu=mu,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( MVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4) )}},
                mES    = { prisk = function(w){ return( operMESfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4)) }}
     ) #end function that finds out which risk function to minimize

    if( is.null(penalty) ){ 
       abspenalty = 1e6; relpenalty = 100*N 
    }else{
       abspenalty = relpenalty = penalty; 
    }; 
    if( heuristic ){
        objective = function( w ){
           w = w/sum(w)  
           cont = percriskcontrib( w );  percrisk = cont[[3]]; crisk = cont[[2]] ; 
           if(MinMaxComp){ out = max( crisk ) }else{ out = prisk(w) }
           # add weight constraints
           out_con = out + out*abspenalty*sum( 1*( w < lower )+1*( w > upper ) )
           # add risk budget constraint 
           con1 = sum(  (percrisk-RBupper)*( percrisk > RBupper  ),na.rm=TRUE ) ; if( is.na(con1) ){ con1 = 0 } # because of Inf*0
           con2 = sum(  (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  ); if( is.na(con2) ){ con2 = 0 }  
           out_con = out_con + out*relpenalty*(con1+con2)
           # add minimum risk constraint
           con = ( prisk(w) - Riskupper)*( prisk(w) > Riskupper ); if( is.na(con) ){ con = 0 }  
           out_con = out_con + out*relpenalty*con
           # add minimum return constraint 
           # portfolio return and risk are in the same unit, but portfolio return is typically some orders of magnitude smaller
           # say: as a very conservative choice: 100 
           preturn = sum( w*mu ) ;
           con = ( Returnlower - preturn)*( preturn < Returnlower ); if( is.na(con) ){ con = 0 }
           out_con = out_con + out*100*relpenalty*con
           return(out_con)
         }

         # initial population is generated with random_portfolios function
         require('PortfolioAnalytics')
         eps = 0.01
         rpconstraint<-constraint(assets=length(lower), min_sum=(1-eps), max_sum=(1+eps), 
             min=lower, max=upper, weight_seq=generatesequence())
         rp<- random_portfolios(rpconstraints=rpconstraint,permutations=controlDE$NP)
         controlDE = list(  NP=as.numeric(nrow(rp)) , initialpop=rp,trace=F ) 
         # print(controlDE)
         minw = DEoptim( fn = objective ,  lower = lower , upper = upper , control = controlDE)
         fvalues = minw$member$bestval
         diff = as.vector( quantile(fvalues,0.10) - min(fvalues) )
         print(c("diff",diff))
         best = min( fvalues ) ; print(best)
         bestsol = minw ; 

         while( diff>1e-6 ){
            pop = as.matrix(minw$member$pop)
            pop[1,] = minw$optim$bestmem;
            minw = DEoptim( fn = objective ,  lower = lower , upper = upper , 
                control = list( itermax = 150, NP=as.numeric(nrow(pop)) , initialpop=pop,trace=F ) )
            fvalues = minw$member$bestval
            diff = best -  min(fvalues)  ; 
            if( diff > 0 ){ best = min( fvalues ) ; bestsol = minw ; print(best) }
          }
          minw = bestsol$optim$bestmem/sum(bestsol$optim$bestmem)  ; #full investment constraint
    }else{
        objective = function( w ){
           if(sum(w)==0){w=w+0.001}
           w = w/sum(w)  
           cont = percriskcontrib( w );  percrisk = cont[[3]]; crisk = cont[[2]] ; 
           if(MinMaxComp){ out = max( crisk ) }else{ out = prisk(w) }
           # add risk budget constraint 
           con1 = sum(  (percrisk-RBupper)*( percrisk > RBupper  ),na.rm=TRUE ) ; if( is.na(con1) ){ con1 = 0 } # because of Inf*0
           con2 = sum(  (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  ); if( is.na(con2) ){ con2 = 0 }  
           out_con = out + out*relpenalty*(con1+con2)
           # add minimum risk constraint
           con = ( prisk(w) - Riskupper)*( prisk(w) > Riskupper ); if( is.na(con) ){ con = 0 }  
           out_con = out_con + out*relpenalty*con
           return(out_con)
         }

         if(Returnlower==-Inf){ 
            inittheta = rep(1,N)/N 
            out = optim( par=inittheta, f = objective, lower = lower, upper = upper ) 
         }else{
            Amat = rbind(diag(x =1,nrow=N,ncol=N), diag(x =-1,nrow=N,ncol=N), rep(1,N), rep(-1,N),as.vector(mu))
            inittheta = rep(0.001,N);
            inittheta[mu==max(mu)] = 1; inittheta = 1-sum(inittheta[mu!=max(mu)]  );   
            out = constrOptim( theta=inittheta, f = objective, grad=NULL,ui=Amat,
                            ci = c(rep(0,N),rep(-1,N),0.99999,-1.0001,Returnlower) ) 
         }

         minw = out$par/sum(out$par)
    }
    cont = percriskcontrib( minw );  percrisk = cont[[3]];    crisk = cont[[2]] ;    
    # check 
    print( "out = list( minw , sum( minw*mu ) , prisk(minw) , percriskcontrib(minw)" )
    out = list( minw , sum( minw*mu ) , prisk(minw) , percrisk , crisk )
    print( out )
    return(out)
}

findportfolio.dynamic = function(R, from, to, names.input = NA,  names.assets, 
              	p = 0.95 , priskbudget = 0.95,  mincriterion = "mES" , percriskcontribcriterion = "mES"  ,
                  strategy , controlDE = list( VTR = 0 , NP=200 ) )
{ # @author Kris Boudt and Brian G. Peterson

    # Description:
    #
    # Performs a loop over the reallocation periods with estimation samples given by from:to
    # It calls the function RBconportfolio to obtain the optimal weights of the strategy.
    #
    # @todo
    #
    # R                 matrix/zoo holding historical returns on risky assets
    #
    # names             vector holding the names of the .csv files to be read
    #
    # from, to          define the estimation sample
    #
    # criteria	      the criterion to be optimized
    #
    # columns.crit      the columns of R in which the criteria are located
    #
    # percriskcontribcriterion risk measure used for the risk budget constraints
    #
    # strategy = c( "EqualRisk" , "EqualWeight" , "MinRisk" , "MinRiskConc" , 
    #                "MinRisk_PositionLimit"     , "MinRisk_RiskLimit"     , "MinRisk_ReturnTarget",
    #                "MinRiskConc_PositionLimit" , "MinRiskConc_RiskLimit" , "MinRiskConc_ReturnTarget")

    # Return:
    # List with first element optimal weights per reallocation period and associated percentage CVaR contributions. 

    # Create a matrix that will hold for each method and each vector the best weights

    cPeriods = length(from);    

    out = matrix(  rep(0, cPeriods*(cAssets)) , ncol= (cAssets) );
    RCout = matrix(  rep(0, cPeriods*(cAssets)) , ncol= (cAssets) );
    RiskReturnout = matrix(  rep(0, cPeriods*2 ) , ncol= 2 );
 
    # first cPeriods rows correspond to cCriteria[1] and so on

    # downside risk
    alpha = 1 - p;
    alphariskbudget = 1-priskbudget;

    # Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix


    if(strategy=="EqualRisk"){
       lower = rep(0,cAssets); upper=rep(1,cAssets) 
       RBlower = rep(1/cAssets,cAssets) ; RBupper = rep(1/cAssets,cAssets)  ;
    }

    if(strategy=="EqualWeight"){
       lower = rep(1/cAssets,cAssets); upper=rep(1/cAssets,cAssets) 
       RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
    }

    if(strategy=="MinRisk" | strategy=="MinRiskConc" | strategy=="MinRisk_ReturnTarget" | strategy=="MinRiskConc_ReturnTarget"){
       lower = rep(0,cAssets); upper=rep(1,cAssets) 
       RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
    }

    MinMaxComp = F; mutarget = -Inf; 
    if( strategy=="MinRiskConc" | strategy=="MinRiskConc_PositionLimit" | strategy=="MinRiskConc_RiskLimit" | strategy=="MinRiskConc_ReturnTarget"  ){
        MinMaxComp = T;
    }

   if(strategy=="MinRisk_PositionLimit" | strategy=="MinRiskConc_PositionLimit"){
      lower = rep(0,cAssets); upper=rep(0.4,cAssets) 
      RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
   }

   if(strategy=="MinRisk_RiskLimit" | strategy=="MinRiskConc_RiskLimit"){
      lower = rep(0,cAssets); upper=rep(1,cAssets) 
      RBlower = rep(-Inf,cAssets) ; RBupper = rep(0.40,cAssets)  ;
   }

    for( per in c(1:cPeriods) ){

       print("-----------New estimation period ends on observation------------------")
       print( paste(to[per],"out of total number of obs equal to", max(to) ));
       print("----------------------------------------------------------------")

       # Estimate GARCH model with data from inception

       inception.R = window(R, start = as.Date(from[1]) , end = as.Date(to[per]) );

       # Estimate comoments of innovations with rolling estimation windows
       in.sample.R = window(R, start = as.Date(from[per]) , end = as.Date(to[per]) );
       in.sample.R = checkData(in.sample.R, method="matrix"); 

       # Estimation of mean return
       M = c();
       library(TTR)
       Tmean = 47 # monthly returns: 4 year exponentially weighted moving average
       for( i in 1:cAssets ){
         M = cbind( M , as.vector( EMA(x=inception.R[,i],n=Tmean) ) ) #2/(n+1)
       }
       M = zoo( M , order.by=time(inception.R) )

       # Center returns (shift by one observations since M[t,] is rolling mean t-Tmean+1,...,t; otherwise lookahead bias)
       inception.R.cent = inception.R;
       ZZ = matrix( rep(as.vector( apply( inception.R[1:Tmean, ] , 2 , 'mean' )),Tmean),byrow=T,nrow=Tmean);
       inception.R.cent[1:Tmean,] = inception.R[1:Tmean, ] - ZZ;
       if( nrow(inception.R)>(Tmean+1) ){ 
                 A = M[Tmean:(nrow(inception.R)-1),];
                 A = zoo( A , order.by = time(inception.R[(Tmean+1):nrow(inception.R), ])) ; #shift dates; otherwise zoo poses problem
                 inception.R.cent[(Tmean+1):nrow(inception.R), ] = inception.R[(Tmean+1):nrow(inception.R), ] - A}
       
       # Garch estimation 
       S = c();
       for( i in 1:cAssets ){
            gout =  garchFit(formula ~ garch(1,1), data = inception.R.cent[,i],include.mean = F, cond.dist="QMLE", trace = FALSE )
            if( as.vector(gout@fit$coef["alpha1"]) < 0.01 ){
               sigmat = rep( sd( as.vector(inception.R.cent[,i])), length(inception.R.cent[,i]) ); 
            }else{
               sigmat = gout@sigma.t
            }
            S = cbind( S , sigmat)
       }
       S = zoo( S , order.by=time(inception.R.cent) )

       # Estimate correlation, coskewness and cokurtosis matrix locally using cleaned innovation series in three year estimation window
       selectU = window(inception.R.cent, start = as.Date(from[per]) , end = as.Date(to[per]) )
       selectU = selectU/window(S, start = as.Date(from[per]) , end = as.Date(to[per]) );
       selectU = clean.boudt2(selectU , alpha = 0.05 )[[1]];
       Rcor = cor(selectU)
       D = diag( as.vector(tail(S,n=1)  ),ncol=cAssets )
       sigma = D%*%Rcor%*%D

       # we only need mean and conditional covariance matrix of last observation
       mu = matrix(tail(M,n=1),ncol=1 ) ;
       D = diag( as.vector(as.vector(tail(S,n=1) ) ),ncol=cAssets )
       sigma = D%*%Rcor%*%D
       in.sample.T = nrow(selectU);
       # set volatility of all U to last observation, such that cov(rescaled U)=sigma 
       selectU = selectU*matrix( rep(as.vector(tail(S,n=1)),in.sample.T  ) , ncol = cAssets , byrow = T )
       M3 = PerformanceAnalytics:::M3.MM(selectU)
       M4 = PerformanceAnalytics:::M4.MM(selectU)

       
       mESfun = function(series){ return( operMES(series,alpha=alpha,2) ) }
       
 
       if(strategy=="MinRisk_ReturnTarget" | strategy=="MinRiskConc_ReturnTarget"){
           mutarget = mean( mu );
           print( c("Minimum return requirement is" , mutarget) )
       }

       if(strategy=="EqualWeight"){
           sol1 = rep(1/cAssets,cAssets);
           switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,mu=mu,sigma=sigma) ; return( cont  ) }},
                GVaR = {percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma) ; return( cont ) }},
                GES = {percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma) ; return( cont  ) }},
                mVaR = {percriskcontrib = function(w){ cont = PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4) ; return( cont  ) }},
                mES = {percriskcontrib = function(w){ cont = operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4) ; return( cont ) }
                }
           )
           sol2 = percriskcontrib( sol1 )
           solution = list( sol1 , sol2[[3]] , sol2[[1]] , mean(mu) ) ;   
       }else{
           solution = PortfolioOptim(  minriskcriterion = "mES" , MinMaxComp = MinMaxComp, percriskcontribcriterion = "mES" , 
                                       mu = mu , sigma = sigma, M3=M3 , M4=M4 , alpha = alpha , alphariskbudget = alphariskbudget , 
                                       lower = lower , upper = upper , Riskupper = Inf , Returnlower= mutarget , RBlower = RBlower, RBupper = RBupper , 
                                       controlDE = controlDE ) 
           # [[1]] : weights ; [[2]] : expected portfolio return ; [[3]] : portfolio CVaR
           # [[4]] : percentage CVaR ; [[5]] : component CVaR
           solution = list( solution[[1]] , solution[[4]] , solution[[3]] , solution[[2]] ); 
       }
       out[ per, ]    = as.vector( solution[[1]] ) 
       RCout[per,  ]  = as.vector( solution[[2]] )   
       RiskReturnout[per,] = c( as.numeric(solution[[3]]) , as.numeric(solution[[4]]) )

 }#end loop over the rebalancing periods; indexed by per=1,...,cPeriods

  # Output save
  rownames(out) = rownames(RCout) = rownames(RiskReturnout) =  names.input; 
  colnames(out) = colnames(RCout) = names.assets;
  colnames(RiskReturnout) = c( "risk" , "expreturn" )

  EWweights = c( rep(1/cAssets,cAssets) )
  EWweights = matrix ( rep(EWweights,cPeriods) , ncol=(cAssets) , byrow = TRUE )
  rownames(EWweights) = names.input; colnames(EWweights) = names.assets;

  return( list( out, RCout, RiskReturnout) ) 
}

clean.boudt2 = 
function (R, alpha = 0.01, trim = 0.001) 
{
    # @author Kris Boudt and Brian Peterson
    # Cleaning method as described in 
    #     Boudt, Peterson and Croux. 2009. Estimation and decomposition of downside risk for portfolios with non-normal returns. Journal of Risk.

    stopifnot("package:robustbase" %in% search() || require("robustbase", 
        quietly = TRUE))
    R = checkData(R, method = "zoo")
    T = dim(R)[1]
    date = c(1:T)
    N = dim(R)[2]
    MCD = covMcd(as.matrix(R), alpha = 1 - alpha)
    # mu = as.matrix(MCD$raw.center)
    mu = MCD$raw.center
    sigma = MCD$raw.cov
    invSigma = solve(sigma)
    vd2t = c()
    cleaneddata = R
    outlierdate = c()
    for (t in c(1:T)) {
        d2t = as.matrix(R[t, ] - mu) %*% invSigma %*% t(as.matrix(R[t,] - mu))
        vd2t = c(vd2t, d2t)
    }
    out = sort(vd2t, index.return = TRUE)
    sortvd2t = out$x
    sortt = out$ix
    empirical.threshold = sortvd2t[floor((1 - alpha) * T)]
    T.alpha = floor(T * (1 - alpha)) + 1
    cleanedt = sortt[c(T.alpha:T)]
    for (t in cleanedt) {
        if (vd2t[t] > qchisq(1 - trim, N)) {
            cleaneddata[t, ] = sqrt(max(empirical.threshold, 
                qchisq(1 - trim, N))/vd2t[t]) * R[t, ]
            outlierdate = c(outlierdate, date[t])
        }
    }
    return(list(cleaneddata, outlierdate))
}


Ipower = function(power,h)
{

   fullprod = 1;

   if( (power%%2)==0 ) #even number: number mod is zero
   {
      pstar = power/2;
      for(j in c(1:pstar))
      {
         fullprod = fullprod*(2*j)
      }
      I = fullprod*dnorm(h);

      for(i in c(1:pstar) )
      { 
         prod = 1;
         for(j in c(1:i) )
         {
            prod = prod*(2*j)
         }
         I = I + (fullprod/prod)*(h^(2*i))*dnorm(h)
      }
   }
   else{
      pstar = (power-1)/2
      for(j in c(0:pstar) )
      {
         fullprod = fullprod*( (2*j)+1 )
      }
      I = -fullprod*pnorm(h);

      for(i in c(0:pstar) )
      { 
         prod = 1;
         for(j in c(0:i) )
         {
            prod = prod*( (2*j) + 1 )
         }
         I = I + (fullprod/prod)*(h^(  (2*i) + 1))*dnorm(h)
      }
   }
   return(I)
}


# Definition of statistics needed to compute Gaussian and modified VaR and ES for the return series of portfolios
# and to compute the contributions to portfolio downside risk, made by the different positions in the portfolio.
#----------------------------------------------------------------------------------------------------------------



m2 = function(w,sigma)
{
   return(t(w)%*%sigma%*%w)
}
derm2 = function(w,sigma)
{
   return(2*sigma%*%w)
}
m3 = function(w,M3)
{
   return(t(w)%*%M3%*%(w%x%w))
}
derm3 = function(w,M3)
{
   return(3*M3%*%(w%x%w))
}
m4 = function(w,M4)
{
   return(t(w)%*%M4%*%(w%x%w%x%w))
}
derm4 = function(w,M4)
{
   return(4*M4%*%(w%x%w%x%w))
}

StdDevfun = function(w,sigma){ return(  sqrt( t(w)%*%sigma%*%w  )) }

GVaRfun = function(w,alpha,mu,sigma){ return (- (t(w)%*%mu) - qnorm(alpha)*sqrt( t(w)%*%sigma%*%w  ) ) }

mVaRfun = function(w,alpha,mu,sigma,M3,M4){
    pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ; 
    skew = pm3 / pm2^(3/2);
    exkurt = pm4 / pm2^(2) - 3; z = qnorm(alpha);
    h = z + (1/6)*(z^2 -1)*skew
    h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
    return (- (t(w)%*%mu) - h*sqrt( pm2  ) ) }

resmVaRfun = function(w,alpha,mu,sigma,ressigma,M3,M4){
    pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ; respm2 =  t(w)%*%resSigma%*%w ;
    skew = pm3 / respm2^(3/2);
    exkurt = pm4 / respm2^(2) - 3; z = qnorm(alpha);
    h = z + (1/6)*(z^2 -1)*skew
    h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
    return (- (t(w)%*%mu) - h*sqrt( pm2  ) ) }

GESfun = function(w,alpha,mu,sigma,M3,M4){
    return (- (t(w)%*%mu) + dnorm(qnorm(alpha))*sqrt(t(w)%*%sigma%*%w)/alpha ) }

operMESfun = function(w,alpha,mu,sigma,M3,M4){
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
    return (- (t(w)%*%mu) - sqrt(pm2)*min(-E,h) ) }

precision = 4;

Portmean = function(w,mu,precision=4)
{
   return( list(  round( t(w)%*%mu , precision) , round ( as.vector(w)*as.vector(mu) , precision ) , round( as.vector(w)*as.vector(mu)/t(w)%*%mu) , precision) )
}

Portsd =  function(w,sigma,precision=4)
{
   pm2 = m2(w,sigma)
   dpm2 = derm2(w,sigma)
   dersd = (0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = dersd*as.vector(w)
   return(list(  round( sqrt(pm2) , precision ) , round( contrib , precision ) , round ( contrib/sqrt(pm2) , precision) )) 
}


PortgausVaR =  function(alpha,w,mu,sigma,precision=4){
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = derm2(w,sigma)
   VaR = - location - qnorm(alpha)*sqrt(pm2)
   derVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = derVaR*as.vector(w) 
   return(list( round( VaR , precision ) , round ( contrib , precision ) , round( contrib/VaR , precision) )) 
}

PortgausES =  function(alpha,w,mu,sigma,precision=4){
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = derm2(w,sigma)
   ES = - location + dnorm(qnorm(alpha))*sqrt(pm2)/alpha
   derES = - as.vector(mu) + (1/alpha)*dnorm(qnorm(alpha))*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = as.vector(w)*derES;
   return(list( round( ES , precision ) , round( contrib , precision) , round( contrib/ES , precision) )) 
}

PortSkew =  function(w,sigma,M3)
{
   pm2 = m2(w,sigma)
   pm3 = m3(w,M3)
   skew = pm3 / pm2^(3/2);
   return( skew )
}

PortKurt =  function(w,sigma,M4)
{
   pm2 = m2(w,sigma)
   pm4 = m4(w,M4)
   kurt = pm4 / pm2^(2) ;
   return( kurt )
}

PortMVaR =  function(alpha,w,mu,sigma,M3,M4,precision=4)
{
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = as.vector( derm2(w,sigma) )
   pm3 = m3(w,M3)
   dpm3 = as.vector( derm3(w,M3) )
   pm4 = m4(w,M4)
   dpm4 = as.vector( derm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew 
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2; 

   MVaR = - location - h*sqrt(pm2)

   derGausVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   derMVaR = derGausVaR + (0.5*dpm2/sqrt(pm2))*( -(1/6)*(z^2 -1)*skew  - (1/24)*(z^3 - 3*z)*exkurt + (1/36)*(2*z^3 - 5*z)*skew^2 )
   derMVaR = derMVaR + sqrt(pm2)*( -(1/6)*(z^2 -1)*derskew  - (1/24)*(z^3 - 3*z)*derexkurt + (1/36)*(2*z^3 - 5*z)*2*skew*derskew  )
   contrib = as.vector(w)*as.vector(derMVaR)
   return(list(  round( MVaR , precision) , round( contrib , precision ), round (contrib/MVaR , precision ) ) ) 
}

derIpower = function(power,h)
{

   fullprod = 1;

   if( (power%%2)==0 ) #even number: number mod is zero
   {
      pstar = power/2;
      for(j in c(1:pstar))
      {
         fullprod = fullprod*(2*j)
      }
      I = -fullprod*h*dnorm(h);

      for(i in c(1:pstar) )
      { 
         prod = 1;
         for(j in c(1:i) )
         {
            prod = prod*(2*j)
         }
         I = I + (fullprod/prod)*(h^(2*i-1))*(2*i-h^2)*dnorm(h)
      }
   }else{
      pstar = (power-1)/2
      for(j in c(0:pstar) )
      {
         fullprod = fullprod*( (2*j)+1 )
      }
      I = -fullprod*dnorm(h);

      for(i in c(0:pstar) )
      { 
         prod = 1;
         for(j in c(0:i) )
         {
            prod = prod*( (2*j) + 1 )
         }
         I = I + (fullprod/prod)*(h^(2*i)*(2*i+1-h^2) )*dnorm(h)
      }
   }
   return(I)
}


PortMES =  function(alpha,w,mu,sigma,M3,M4,precision=4)
{
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = as.vector( derm2(w,sigma) )
   pm3 = m3(w,M3)
   dpm3 = as.vector( derm3(w,M3) )
   pm4 = m4(w,M4)
   dpm4 = as.vector( derm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew 
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2; 

   derh = (1/6)*(z^2 -1)*derskew + (1/24)*(z^3 - 3*z)*derexkurt - (1/18)*(2*z^3 - 5*z)*skew*derskew

   E = dnorm(h)
   E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   E = E/alpha
   MES = - location + sqrt(pm2)*E

   derMES = -mu + 0.5*(dpm2/sqrt(pm2))*E
   derE = (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*derexkurt 
   derE = derE +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*derskew
   derE = derE + (1/36)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*skew*derskew
   X = -h*dnorm(h) + (1/24)*(  derIpower(4,h) - 6*derIpower(2,h) -3*h*dnorm(h)  )*exkurt 
   X = X + (1/6)*( derIpower(3,h) - 3*derIpower(1,h) )*skew 
   X = X + (1/72)*( derIpower(6,h) - 15*derIpower(4,h) + 45*derIpower(2,h) + 15*h*dnorm(h)  )*skew^2
   derE = derE+derh*X  # X is a scalar
   derE = derE/alpha
   derMES = derMES + sqrt(pm2)*derE
   contrib = as.vector(w)*as.vector(derMES)
   return(list(  round( MES , precision ) , round( contrib , precision ), round( contrib/MES, precision )) ) 
}


operPortMES =  function(alpha,w,mu,sigma,M3,M4,precision=4)
{
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = as.vector( derm2(w,sigma) )
   pm3 = m3(w,M3)
   dpm3 = as.vector( derm3(w,M3) )
   pm4 = m4(w,M4)
   dpm4 = as.vector( derm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew 
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2; 
   I1 = Ipower(1,h); I2 = Ipower(2,h); I3 = Ipower(3,h); I4 = Ipower(4,h);  I6 = Ipower(6,h); 

   derh = (1/6)*(z^2 -1)*derskew + (1/24)*(z^3 - 3*z)*derexkurt - (1/18)*(2*z^3 - 5*z)*skew*derskew

   E = dnorm(h)
   E = E + (1/24)*(   I4 - 6*I2 + 3*dnorm(h)   )*exkurt
   E = E +  (1/6)*(   I3 - 3*I1   )*skew;
   E = E + (1/72)*( I6 -15*I4+ 45*I2 - 15*dnorm(h) )*(skew^2)
   E = E/alpha

   MES = - location - sqrt(pm2)*min(-E,h)

   if(-E<=h){
      derMES = -mu + 0.5*(dpm2/sqrt(pm2))*E
      derE = (1/24)*(   I4 - 6*I2 + 3*dnorm(h)   )*derexkurt 
      derE = derE +  (1/6)*(   I3 - 3*I1   )*derskew
      derE = derE + (1/36)*(  I6 -15*I4 + 45*I2 - 15*dnorm(h) )*skew*derskew
      X = -h*dnorm(h) + (1/24)*(  derIpower(4,h) - 6*derIpower(2,h) -3*h*dnorm(h)  )*exkurt 
      X = X + (1/6)*( derIpower(3,h) - 3*derIpower(1,h) )*skew 
      X = X + (1/72)*( derIpower(6,h) - 15*derIpower(4,h) + 45*derIpower(2,h) + 15*h*dnorm(h)  )*skew^2
      derE = derE+derh*X  # X is a scalar
      derE = derE/alpha
      derMES = derMES + sqrt(pm2)*derE }else{
      derMES = -mu - 0.5*(dpm2/sqrt(pm2))*h - sqrt(pm2)*derh ;  }
   contrib = as.vector(w)*as.vector(derMES)
   return(list( round( MES, precision) , round( contrib , precision ) , round(contrib/MES,precision) ) ) 
}


centeredmoment = function(series,power)
{
   location = mean(series);
   out = sum( (series-location)^power  )/length(series);
   return(out);
}

operMES =  function(series,alpha,r)
{
   z = qnorm(alpha)
   location = mean(series);
   m2 = centeredmoment(series,2)
   m3 = centeredmoment(series,3)
   m4 = centeredmoment(series,4)
   skew = m3 / m2^(3/2);
   exkurt = m4 / m2^(2) - 3;

   h = z + (1/6)*(z^2 -1)*skew 
   if(r==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2};

   MES = dnorm(h)
   MES = MES + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   MES = MES +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   MES = MES + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   MES = - location - (sqrt(m2))*min( -MES/alpha , h )
   return(MES)
}

TwoVarPlot <- function(xvar, y1var, y2var, labels, noincs = 5,marks=c(1,2), legpos, leglabs, title)
{ 
   # https://stat.ethz.ch/pipermail/r-help/2000-September/008182.html

   # plots an x y1 y2 using left and right axes for the different y's
   # rescales y2 to fit in the same space as y1

   # noincs - no of divisions in the axis labels
   # marks - type of marker for each y
   # legpos - legend position
   # leglabs - legend labels
  
   # rescale to fit on same axis
   scaledy2var <- (y2var - min(y2var)) / (max(y2var) - min(y2var))
   scaledy2var <- (scaledy2var * (max(y1var) - min(y1var))) + min(y1var)

   # plot it up and add the points
   plot(xvar, y1var, xlab=labels[1], ylab="", axes=F, pch=marks[1],main=title,type="l")
   lines(xvar, scaledy2var, lty=3 )

   # make up some labels and positions
   y1labs <- round(seq(min(y1var), max(y1var), length=noincs),2)

   # convert these to the y2 axis scaling
   y2labs <- (y1labs - min(y1var)) / (max(y1var) - min(y1var))
   y2labs <- (y2labs * (max(y2var) - min(y2var))) + min(y2var)
   y2labs <- round(y2labs, 2)

   axis(1)
   axis(2, at=y1labs, labels=y1labs)
   axis(4, at=y1labs, labels=y2labs)
   mtext(labels[3], side=4, line=2)
   mtext(labels[2], side=2, line=2)
   box()

   legend( legend=leglabs, lty = c(1,3), bty="o", x=legpos)
}


