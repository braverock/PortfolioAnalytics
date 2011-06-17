
#-----------------------------------------------------------
# General functions
#-----------------------------------------------------------

vech <- function(x){
    t(x[!upper.tri(x)])
}

lvech <- function(x){
    t(x[!upper.tri(x, diag = T)])
}

#-----------------------------------------------------------
# Functions for the GARCH estimation
#-----------------------------------------------------------

hatsigma <- function( par , x ,uncH){
  # see: code underlying fGarch package of Chalaby and Wuertz
  alpha = par[1] ; beta = par[2];
  omega = uncH*(1-alpha-beta)
  s2 = rep( uncH , T )
  e2 <- x^2 
  e2t <- omega + alpha*c(mean(e2),e2[-length(x)])
  s2 <- filter(e2t, beta, "recursive", init = uncH )
  return(s2)
}

llhGarch11N <- function(par, x, uncH){
  # see: code underlying fGarch package of Chalaby and Wuertz
   x = as.numeric(x)
   s2 = hatsigma( par = par , x = x ,uncH=uncH)
   return( sum( log(s2) + x^2/s2 ))
}

weightedvariance = function( u ){
    k = qchisq(0.99,df=1) ; 
    jumpIndicator = ( (u/mad(u))^2<=k  ) 
    ufilt = u[jumpIndicator]
    uncH = mean(ufilt^2,na.rm=TRUE)*(0.99/pchisq(k,df=3))
    return(uncH)  
    # check: U = matrix( rnorm(500000),ncol=100) ; mean(apply(U,2,'weightedvariance'))
}


garchVol = function( mR.centered , parN  ){
      S = c();
      S_forecast = c(); 
      N = ncol(mR.centered)
      vuncH = apply( mR.centered , 2 , 'weightedvariance' )
      for( i in 1:N ){
        uncH = vuncH[i]
        # do the garch estimation
        # fitN <- try( nlminb(start=parN, objective=llhGarch11N , x = mR.centered[,i], lower=lowN, upper=upN,
        #           control=list(x.tol = 1e-8,trace=0)) )
        # do the garch estimation with constraints to unsure covariance stationarity ie alpha + beta < 1
        fitN <- try(  constrOptim( theta = parN , f = llhGarch11N , ui=rbind(c(1,0),c(0,1),c(-1,0),c(0,-1),c(-1,-1) ), 
            ci=c(0,0,-1,-1,-1), grad=NULL, x = mR.centered[,i], uncH=uncH) ) 

        LLH_constant = llhGarch11N( par=c(0,0), x=mR.centered[,i], uncH = uncH ) 
        if( !inherits(fitN, "try-error") ){  LLH_garch    = -fitN$value }else{  LLH_garch = LLH_constant }
        # Likelihood ratio test statistic for the case of time-varying garch effects
        # cases where garch volatility is probably not reliable or the volatility forecasts are so erratic that a constant garch works fine
        useGARCHd = TRUE
        if( LLH_garch=="NaN" | (fitN$par[1]>= 0.1&fitN$par[2]<=0.7) | sum(fitN$par)>=0.999 ){ 
            LLH_garch = LLH_constant ; 
            useGARCHd = F; 
        } #volatility is too erratic
        # Data is from inception: once we start using garch, we continue to ensure stability of the forecasts
        # Likelihood ratio test for a time-varying vol model
        if( 2*(LLH_garch-LLH_constant) <= qchisq(0.999,df=2) & (!useGARCHd) ){ 
             sigmat = rep( sd( as.vector(mR.centered[,i])), length(mR.centered[,i]) ); 
             S_forecast = c( S_forecast ,sqrt(uncH))
        }else{
             sigmat = sqrt(hatsigma( par = fitN$par , x = mR.centered[,i] , uncH=uncH ) )
             lastsigmat = tail(sigmat,1)
             if( lastsigmat=="NaN" |  is.na(lastsigmat)  ){
                print(  c("NaN day", d , "for asset" , i)  ); 
                sigmat = rep( sqrt(uncH) , length(mR.centered[,i]) ); 
                # print(c("unc sd",sqrt(uncH) ))
                S_forecast = c( S_forecast , sqrt(uncH) )
             }else{
                e2 = as.numeric(tail(mR.centered[,i],1)^2)
                S_forecast = c( S_forecast , 
                   sqrt(  uncH*(1-sum(fitN$par))+ fitN$par[1]*e2+ fitN$par[2]*lastsigmat^2  )  )
              }
        }
        S = cbind( S , sigmat )
      }
      return( list(S,S_forecast) )
}

#-----------------------------------------------------------
# Fuction for the shrinkage correlation
#-----------------------------------------------------------


shrinkagecorrelation = function( mDevolR.centered ){
   N = ncol( mDevolR.centered )
   # Unconditional correlation estimate is shrinkage between sample (spearman) correlation and equicorrelation
   sample = cor(mDevolR.centered,method="spearman") #make it robust by using ranks
   # Elton and Gruber: same correlation across all assets
   rho = mean( lvech(sample) )
   I = diag( rep(1,N) ) ; J = matrix( rep(1,N^2) , ncol=N )
   target = (1-rho)*I + rho*J
   # The shrinkage estimate = delta*target F + (1-delta)* sample S
   kappahat = shrinkage.intensity( mDevolR.centered , prior = target , sample = sample ) 
   delta = max( 0 , min(kappahat/T,1) )
   C = delta*target +(1-delta)*sample
   return(C)
}


#-----------------------------------------------------------
# Functions for the DCC estimation
#-----------------------------------------------------------


DCCestimation = function( mDevolR.centered , parN , uncR ){ 
    #fitN <- try( nlminb(start=parN, objective=loglik.dcc2.DECO , lower=lowN, upper=upN,
    #           control=list(x.tol = 1e-8,trace=0)) )
    # Ensure covariance stationarity
    N = ncol( mDevolR.centered )
    I = diag( rep(1,N) ) ; J = matrix( rep(1,N^2) , ncol=N )
    rho = mean( lvech(uncR) )
    fitN <- try(  constrOptim( theta = parN , f = loglik.dcc2.DECO , ui=rbind(c(1,0),c(0,1),c(-1,0),c(0,-1),c(-1,-1) ), 
            ci=c(lowN,-upN,-1), grad=NULL, uncR= uncR , mDevolR.centered=mDevolR.centered )  ) 
    # compare with constant correlation case
    LLH_constant = 0; 
    invtarget    =  (1/(1-rho))*( I - rho/( 1+(N-1)*rho )*J );
    for(t in 2:nrow(mDevolR.centered)){
        st = matrix( mDevolR.centered[t,] , ncol = 1 );
        LLH_constant = LLH_constant + log( det(uncR) ) + t(st)%*%invtarget%*%st
    }
    LLH_constant = - LLH_constant
    if( !inherits(fitN, "try-error") ){  LLH_DCC    = -fitN$value }else{  LLH_DCC = -LLH_constant }
    # Likelihood ratio test statistic for the case of time-varying garch effects
    # cases where garch volatility is probably not reliable or the volatility forecasts are so erratic that a constant garch works fine
    if( LLH_DCC=="NaN" | (fitN$par[1]>= 0.1&fitN$par[2]<=0.7) | sum(fitN$par)>=0.999 ){ 
         LLH_DCC = LLH_constant ; 
         useDCC = F; 
    } #volatility is too erratic     
    if( 2*(LLH_DCC-LLH_constant) <= qchisq(0.999,df=2)  ){ 
        Rt = uncR;
    }else{
        print( c("use DCC with parameters",fitN$par) )
        Rt = Rforecast(fitN$par,mR=mDevolR.centered,uncR=C)
    }
    return(Rt)
}

Rforecast <- function(par,mR,uncR){
  T = nrow(mR); N = ncol(mR)
  alpha = par[1] ; beta = par[2];
  omega =  (1-par[1]-par[2])*uncR
  Qt = diag( rep(1,N) )
  k = 1
  for( i in 2:N ){
     for(j in 1:(i-1)){
         e2 =  mR[,i]*mR[,j];
         e2[is.na(e2)] = mean( na.omit(e2 ) )
         e2t <- omega[i,j] + alpha*c(mean(e2),e2[-length(e2)], tail(e2,1) )
         Qt[i,j] = Qt[j,i] =  tail(filter(e2t, beta, "recursive", init = uncR[i,j] ),1)
     }
  }
  
  return( diag(diag(Qt)^(-0.5))%*%Qt%*%diag(diag(Qt)^(-0.5)) )
}
# test; uncR=diag(c(1,1)) ; Rforecast( c(0.05,0.4) , matrix(rnorm(400),ncol=2),uncR)

loglik.dcc2.DECO = function (param,uncR,mDevolR.centered) 
{
    require('ccgarch')
    dvar = mDevolR.centered
    nobs <- dim(dvar)[1]
    ndim <- dim(dvar)[2]
    out <- .Call("dcc_est", dvar, uncR, param[1], param[2])
    DCC <- dcc.est(dvar, param)$DCC
    lf <- numeric(ndim)
    I = diag( rep(1,ndim) ) ; J = matrix( rep(1,ndim^2) , ncol=ndim )
    for (i in 1:nobs) {
        R <- matrix(DCC[i, ], ndim, ndim)
        rho = mean( lvech(R) )
        DECO = (1-rho)*I + rho*J
        invDECO = (1/(1-rho))*( I - rho/( 1+(ndim-1)*rho )*J )
        lf[i] <- log(det(DECO)) + sum(dvar[i, ] * crossprod(invDECO, 
            dvar[i, ]))
    }
    sum(lf)
}

