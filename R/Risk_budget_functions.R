###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2009 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

MaxReturnRBconportfolio = function( minriskcriterion = "mES" , percriskcontribcriterion = "mES" , R = NULL,
                     mu = NULL , sigma = NULL, resSigma = NULL ,M3=NULL,M4=NULL, return_target=NULL,
                     alpha = 0.05, alphariskbudget = 0.05,
                     lower = NULL , upper = NULL, Riskupper = NULL, RBlower = NULL , RBupper = NULL, precision = 1e-3 , weights=NULL,
                     controlDE = list( VTR = 0 , NP=200 ) , includeDEoutput=FALSE, ... ){

     start_t<-Sys.time()
     # This function produces the portfolio that minimimizes "mincriterion" subject to mincriterion < maxvalu
     # Either the multivariate return series is given or estimates of the mean, covariance, coskewness or cokurtosis
     require(zoo);
     # require(PerformanceAnalytics);
     require(DEoptim)
     # Example:
     # MaxReturnRBconportfolio(R=zoo(matrix( rnorm(400) , ncol=4 ),order.by=c(1:100)),RBupper=rep(0.3,4))
     # library(PerformanceAnalytics); data(edhec); MaxReturnRBconportfolio(R=edhec[,c(5,6,7,9)], Riskupper = 0.0065, RBlower=rep(0.225,4),RBupper=rep(0.275,4) )

     NP = controlDE$NP
     if( !is.null(R) ){
          cleanR = clean.boudt2( R , alpha = alphariskbudget )[[1]];
          T = nrow(R);N=ncol(R)
          mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
          sigma = cov(R);
          M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
          M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)

          for(t in c(1:T)){
             centret = as.vector(R[t,])-mu;
             M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
             M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
         }
         M3 = M3/T; M4 = M4/T;
     }else{ N = length(mu) }

     if (is.null(weights)) {
         w = rep(1/N,N)
     } else {
         w = weights
     }

     if( is.null(lower) ){ lower = rep(0,N) }         ; if( is.null(upper) ){ upper = rep(1,N) }
     if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }
     if( is.null(Riskupper) ){ Riskupper = Inf }

     if( is.null(resSigma) ){
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( mVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4) )}},
                mES    = { prisk = function(w){ return( operMESfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
     }else{ # when garch model is used
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( resmVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }},
                mES    = { prisk = function(w){ return( resoperMESfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
    }
    objective = function( w ){
        penalty = 1e4;

        #        w = matrix( c( w , 1-sum(w) ) , ncol=1) # assume a cash asset

        # add weight constraint penalty, turn this into configuration from constraints
        # force weights to 1
        w <- (1/sum(w))*w
        N = length(w);
        percrisk = percriskcontrib( w );
        out = -sum( w*mu ) #needs to be maximized
        if(!is.null(return_target)) {
          if(ret<return_target) { out = out + penalty*100*(return_target-ret) } # hack to penalize returns below target, we'll fix in constrained_objective
        }
        # add full investment constraint:
        #out = out + penalty*( ((1-sum(w))>upper[N]) | ((1-sum(w))<lower[N]) )
        # penalize weights outside my constraints
        out = out + sum(w[which(w>upper[1:N])]-upper[which(w>upper[1:N])])*penalty
        out = out + sum(lower[which(w<lower[1:N])]-w[which(w<lower[1:N])])*penalty

        ##########
        # add portfolio risk constraint
        prw=prisk(w)
        # full penalty for violating risk upper limit
        if(  prw > Riskupper ){ out = out + penalty*( prw - Riskupper) }
        # half penalty for risk lower than target
        if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
        
        # add risk budget constraint
        out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
        #print(paste("output of objective function",out))

        #return
        return(out)
    }

#     print( "lower weight constraints:" ); print(lower) ; print( "upper weight constraints:" ); print( upper ) ;
#     print( "upper constraint on risk:" ); print( Riskupper ) ;
#     print( "lower perc risk budgets constraints:"); print(RBlower); print( "upper risk budget constraints:" ); print( RBupper) ;

    ## loop made obsolete? by Katharine's changes to DEoptim??
#    minw=NULL
#    i=1
#    while (is.null(minw) & i<10) {
      minw = try(DEoptim( objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, ...)) # add ,silent=TRUE here?
      if(inherits(minw,"try-error")) { minw=NULL }
#      i<-i+1
#      minw
#    }
    if(is.null(minw)){
        print(paste("Optimizer was unable to find a solution for target",Riskupper))
        return(paste("Optimizer was unable to find a solution for target",Riskupper))
    }

    fvalues = minw$member$bestvalit
    diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
## Kris: is this necessary?
#     if( diff>0 ){
#          print(c("diff",diff))
#          #browser()
#          pop = minw$member$pop
# #         minw = DEoptim( objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = list( itermax = 150, initial=pop ),... )
#          minw = DEoptim( objective ,  lower = lower[1:N] , upper = upper[1:N] , control = list( itermax = 150, initial=pop, NP=NP ),... )
#          fvalues = minw$member$bestvalit
#          diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
#     }
    print(c("combining results for target",Riskupper ))
    outw = as.vector( minw$optim$bestmem)
    outw = (1/sum(outw))*outw # normalize weights to 1
    names(outw) = colnames(R)
    # outw = as.vector(c( minw$optim$bestmem , 1-sum(minw$optim$bestmem) )) ; #full investment constraint
    # names(outw) = c(colnames(R),"cash")
    # check
    out = list(weights=outw , perc_risk_contr= percriskcontrib(outw) )
    names(out$perc_risk_contr)<-colnames(R)

    # include some standard measures
    out$stats=c(sum( outw*mu ) , StdDevfun(outw,sigma=sigma), mVaRfun(outw,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4), operMESfun(outw,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4))
    names(out$stats)<-c("mean_ret","Std. Dev.","mVaR","mES")
    # @TODO: change these to use PerformanceAnalytics functions
    out$targets<-c(minriskcriterion,Riskupper,percriskcontribcriterion)
    names(out$targets)<-c("Risk Fn","Risk Target","Risk Contribution Fn")
    
    if(includeDEoutput){out$DEoutput=minw}
    end_t<-Sys.time()
    print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
    return(out)
}

###############################################################################

MinMaxPercCVaRconportfolio = function( minriskcriterion = "mES" , percriskcontribcriterion = "mES" ,
                     R = NULL, mu = NULL , sigma = NULL, resSigma = NULL ,M3=NULL,M4=NULL, alpha = 0.05, alphariskbudget = 0.05,
                     lower = NULL , upper = NULL, Riskupper = NULL, Returnlower = NULL, RBlower = NULL , RBupper = NULL, precision = 1e-3 ,
                     controlDE = list( VTR = 0 , NP=200 )  ){

     # This function produces the portfolio that minimimizes "mincriterion" subject to preturns > Returnlower
     # Either the multivariate return series is given or estimates of the mean, covariance, coskewness or cokurtosis
     require(zoo); require(PerformanceAnalytics);   require(DEoptim)
     # Example:
     # MinMaxPercCVaRconportfolio(R=zoo(matrix( rnorm(400) , ncol=4 ),order.by=c(1:100)),Returnlower= 0.05)
     # library(PerformanceAnalytics); data(edhec);
     # MinMaxPercCVaRconportfolio(R=edhec[,c(5,6,7,9)], Riskupper = Inf ,Returnlower= 0.10 )
     if( !is.null(R) ){
          cleanR = clean.boudt2( R , alpha = alphariskbudget )[[1]];
          T = nrow(R);N=ncol(R)
          mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
          sigma = cov(R);
          M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
          M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)

          for(t in c(1:T)){
             centret = as.vector(R[t,])-mu;
             M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
             M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
         }
         M3 = M3/T; M4 = M4/T;
     }else{ N = length(mu) }

     if( is.null(lower) ){ lower = rep(0,N) }         ; if( is.null(upper) ){ upper = rep(1,N) }
     if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }
     if( is.null(Riskupper) ){ Riskupper = Inf }
     if( is.null(Returnlower) ){ Returnlower = -Inf }

     if( is.null(resSigma) ){
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( mVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4) )}},
                mES    = { prisk = function(w){ return( operMESfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
     }else{ # when garch model is used
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( resmVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }},
                mES    = { prisk = function(w){ return( resoperMESfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
    }
    objective = function( w ){
        w = matrix( c( w , 1-sum(w) ) , ncol=1)
        N = length(w);
        percrisk = percriskcontrib( w );
        out = max( percrisk ) #needs to be maximized
        # add full investment constraint:
        penalty = 1e6;
        out = out + penalty*( (w[N]>upper[N]) | (w[N]<lower[N]) )
        # add minimum risk constraint
        if(  prisk(w) > Riskupper ){ out = out + penalty*( prisk(w) - Riskupper) }
        # add minimu return constraint
        preturn = sum( w*mu ) ;
        if(  preturn < Returnlower ){ out = out + penalty*( Returnlower - preturn) }
        return(out)
    }

    print( "lower weight constraints:" ); print(lower) ; print( "upper weight constraints:" ); print( upper ) ;
    print( "upper constraint on risk:" ); print( Riskupper ) ;
    print( "lower constraint on return:" ); print( Returnlower ) ;
    print( "lower perc risk budgets constraints:"); print(RBlower); print( "upper risk budget constraints:" ); print( RBupper) ;

    minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = controlDE)
    fvalues = minw$member$bestvalit
    diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    while( diff>0 ){
         print(c("diff",diff))
         pop = minw$member$pop
         minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = list( itermax = 150, initial=pop ) )
         fvalues = minw$member$bestvalit
         diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    }
    minw = as.vector(c( minw$optim$bestmem , 1-sum(minw$optim$bestmem) )) ; #full investment constraint
    
    # check
    print( "out = list( minw , sum( minw*mu*12 ) , prisk(minw) , percriskcontrib(minw)" )
    out = list( minw , sum( minw*mu ) , prisk(minw) , percriskcontrib(minw) )
    return(out)
}



MaxReturnRBconportfolio.original = function( minriskcriterion = "mES" , percriskcontribcriterion = "mES" , R = NULL, mu = NULL , sigma = NULL, resSigma = NULL ,M3=NULL,M4=NULL,
                     alpha = 0.05, alphariskbudget = 0.05,
                     lower = NULL , upper = NULL, Riskupper = NULL, RBlower = NULL , RBupper = NULL, precision = 1e-3 ,
                     controlDE = list( VTR = 0 , NP=200 )  ){

     # This function produces the portfolio that minimimizes "mincriterion" subject to mincriterion < maxvalu
     # Either the multivariate return series is given or estimates of the mean, covariance, coskewness or cokurtosis
     require(zoo); require(PerformanceAnalytics);   require(DEoptim)
     # Example:
     # MaxReturnRBconportfolio(R=zoo(matrix( rnorm(400) , ncol=4 ),order.by=c(1:100)),RBupper=rep(0.3,4))
     # library(PerformanceAnalytics); data(edhec); MaxReturnRBconportfolio(R=edhec[,c(5,6,7,9)], Riskupper = 0.0065, RBlower=rep(0.225,4),RBupper=rep(0.275,4) )
     if( !is.null(R) ){
          cleanR = clean.boudt2( R , alpha = alphariskbudget )[[1]];
          T = nrow(R);N=ncol(R)
          mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
          sigma = cov(R);
          M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
          M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)

          for(t in c(1:T)){
             centret = as.vector(R[t,])-mu;
             M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
             M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
         }
         M3 = M3/T; M4 = M4/T;
     }else{ N = length(mu) }

     if( is.null(lower) ){ lower = rep(0,N) }         ; if( is.null(upper) ){ upper = rep(1,N) }
     if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }
     if( is.null(Riskupper) ){ Riskupper = Inf }

     if( is.null(resSigma) ){
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( mVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4) )}},
                mES    = { prisk = function(w){ return( operMESfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
     }else{ # when garch model is used
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR   = { percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES    = { percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR   = { percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES    = { percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( minriskcriterion ,
                StdDev = { prisk = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { prisk = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { prisk = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { prisk = function(w){ return( resmVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }},
                mES    = { prisk = function(w){ return( resoperMESfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
    }
    w = rep(1/4,3)
    objective = function( w ){
        w = matrix( c( w , 1-sum(w) ) , ncol=1)
        N = length(w);
        percrisk = percriskcontrib( w );
        out = -sum( w*mu ) #needs to be maximized
        # add full investment constraint:
        penalty = 1e6;
        out = out + penalty*( (w[N]>upper[N]) | (w[N]<lower[N]) )
        # add minimum risk constraint
        if(  prisk(w) > Riskupper ){ out = out + penalty*( prisk(w) - Riskupper) }
        # add risk budget constraint
         out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
        return(out)
    }

    print( "lower weight constraints:" ); print(lower) ; print( "upper weight constraints:" ); print( upper ) ;
    print( "upper constraint on risk:" ); print( Riskupper ) ;
    print( "lower perc risk budgets constraints:"); print(RBlower); print( "upper risk budget constraints:" ); print( RBupper) ;

    minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = controlDE)
    fvalues = minw$member$bestvalit
    diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    while( diff>0 ){
         print(c("diff",diff))
         pop = minw$member$pop
         minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = list( itermax = 150, initial=pop ) )
         fvalues = minw$member$bestvalit
         diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    }
    minw = as.vector(c( minw$optim$bestmem , 1-sum(minw$optim$bestmem) )) ; #full investment constraint
    
    # check
    out = list(minw , sum( minw*mu ) , prisk(minw) , percriskcontrib(minw) )
    return(out)
}




MinRiskRBconportfolio = function( mincriterion = "mES" , percriskcontribcriterion = "mES" , R = NULL, mu = NULL , sigma = NULL, resSigma = NULL ,M3=NULL,M4=NULL,
                     alpha = 0.05, alphariskbudget = 0.05,lower = NULL , upper = NULL, RBlower = NULL , RBupper = NULL, precision = 1e-3 ,
                     controlDE = list( VTR = 0 , NP=200 )  ){

     # This function produces the portfolio that minimimizes "mincriterion" subject to mincriterion < maxvalu
     # Either the multivariate return series is given or estimates of the mean, covariance, coskewness or cokurtosis
   
     # Example:
     # riskbudgetconstrainedportfolio(R=zoo(matrix( rnorm(400) , ncol=4 ),order.by=c(1:100)),RBupper=rep(0.3,4))

     if( !is.null(R) ){
          cleanR = clean.boudt2( R , alpha = alphariskbudget )[[1]];
          T = nrow(R);N=ncol(R)
          mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
          sigma = cov(R);
          M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
          M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)

          for(t in c(1:T)){
             centret = as.vector(R[t,])-mu;
             M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
             M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
         }
         M3 = M3/T; M4 = M4/T;
     }else{ N = length(mu) }

     if( is.null(lower) ){ lower = rep(0,N) }         ; if( is.null(upper) ){ upper = rep(1,N) }
     if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }

     if( is.null(resSigma) ){
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR = {percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES = {percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR = {percriskcontrib = function(w){ cont = PortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES = {percriskcontrib = function(w){ cont = operPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( mincriterion ,
                StdDev = { obj = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { obj = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { obj = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { obj = function(w){ return( mVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4) )}},
                mES    = { obj = function(w){ return( operMESfun(w,mu=mu,alpha=alpha,sigma=sigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
     }else{ # when garch model is used
         switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR = {percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES = {percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR = {percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES = {percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
        ) #end function that finds out which percentage risk contribution criterion to use
         switch( mincriterion ,
                StdDev = { obj = function(w){ return( StdDevfun(w,sigma=sigma) ) }},
                GVaR   = { obj = function(w){ return( gausVaRfun(w,alpha=alpha,mu=mu,sigma=sigma) ) }},
                GES    = { obj = function(w){ return( gausESfun(w,mu=mu,alpha=alpha,sigma=sigma) )  }},
                mVaR   = { obj = function(w){ return( resmVaRfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }},
                mES    = { obj = function(w){ return( resoperMESfun(w,mu=mu,alpha=alpha,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)) }}
        ) #end function that finds out which risk function to minimize
    }
    w = rep(1/4,3)
    objective = function( w ){
        w = matrix( c( w , 1-sum(w) ) , ncol=1)
        N = length(w);
        percrisk = percriskcontrib( w ); penalty = 1e6;
        out = obj( w ) + penalty*( (w[N]>upper[N]) | (w[N]<lower[N]) )
        out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
        return(out)
    }
    library(DEoptim)
    print( "lower weight constraints:" ); print(lower) ; print( "upper weight constraints:" ); print( upper ) ;
    print( "lower perc risk budgets constraints:"); print(RBlower); print( "upper risk budget constraints:" ); print( RBupper) ;

    minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = controlDE)
    fvalues = minw$member$bestvalit
    diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    while( diff>0 ){
         print(c("diff",diff))
         pop = minw$member$pop
         minw = DEoptim( FUN = objective ,  lower = lower[1:(N-1)] , upper = upper[1:(N-1)] , control = list( itermax = 50, initial=pop ) )
         fvalues = minw$member$bestvalit
         diff = as.vector( quantile(fvalues,0.05) - quantile(fvalues,1/length(fvalues)) )
    }
    minw = as.vector(c( minw$optim$bestmem , 1-sum(minw$optim$bestmem) )) ; #full investment constraint
    
    # check
    out = list(minw , percriskcontrib(minw) )
    return(out)
}


findportfolio.dynamic = function(R, from, to, names.input = NA,  names.assets,
              	p = 0.95 , priskbudget = 0.95,  mincriterion = "mES" , percriskcontribcriterion = "mES"  ,
                  strategy , controlDE = list( VTR = 0 , NP=200 ) , kfactor = 0.9)
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
    # strategy             c( "equalrisk" , "equalweighted" , "unconstrained" , "maxw40" , "maxRBcon40" , "riskaverse" , "riskpicking" )

    # Return:
    # List with first element optimal weights per reallocation period and associated percentage CVaR contributions.

    # Create a matrix that will hold for each method and each vector the best weights

    cPeriods = length(from);

    out = matrix(  rep(0, cPeriods*(cAssets)) , ncol= (cAssets) );
    RCout = matrix(  rep(0, cPeriods*(cAssets)) , ncol= (cAssets) );
    # first cPeriods rows correspond to cCriteria[1] and so on

    # downside risk
    alpha = 1 - p;
    alphariskbudget = 1-priskbudget;

    # Estimation of the return mean vector, covariance, coskewness and cokurtosis matrix


    if(strategy=="equalrisk"){
       lower = rep(0,cAssets); upper=rep(1,cAssets)
       RBlower = rep(1/cAssets,cAssets) ; RBupper = rep(1/cAssets,cAssets)  ;
    }

    if(strategy=="equalweighted"){
       lower = rep(1/cAssets,cAssets); upper=rep(1/cAssets,cAssets)
       RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
    }

    if(strategy=="unconstrained"){
       lower = rep(0,cAssets); upper=rep(1,cAssets)
       RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
    }

   if(strategy=="maxw40"){
      lower = rep(0,cAssets); upper=rep(0.4,cAssets)
      RBlower = rep(-Inf,cAssets) ; RBupper = rep(Inf,cAssets)  ;
   }

   if(strategy=="maxRBcon40"){
      lower = rep(0,cAssets); upper=rep(1,cAssets)
      RBlower = rep(-Inf,cAssets) ; RBupper = rep(0.40,cAssets)  ;
   }



    for( per in c(1:cPeriods) ){

       print("-----------New estimation period ends on observation------------------")
       print( paste(to[per],"out of total number of obs equal to", max(to) ));
       print("----------------------------------------------------------------")

       ########################  GARCH Volatility Estimator ####################

       ## Estimate GARCH model with data from inception

       inception.R = window(R, start = as.Date(from[1]) , end = as.Date(to[per]) );

       ## Estimate comoments of innovations with rolling estimation windows
       in.sample.R = window(R, start = as.Date(from[per]) , end = as.Date(to[per]) );
       in.sample.R = checkData(in.sample.R, method="matrix");


       ## Estimation of mean return
       M = c();
       library(TTR)
       Tmean = 47 # monthly returns: 4 year exponentially weighted moving average
       for( i in 1:cAssets ){
         M = cbind( M , as.vector( EMA(x=inception.R[,i],n=Tmean) ) ) #2/(n+1)
       }
       M = zoo( M , order.by=time(inception.R) )

       ## Center returns (shift by one observations since M[t,] is rolling mean t-Tmean+1,...,t; otherwise lookahead bias)
       inception.R.cent = inception.R;
       ZZ = matrix( rep(as.vector( apply( inception.R[1:Tmean, ] , 2 , 'mean' )),Tmean),byrow=T,nrow=Tmean);
       inception.R.cent[1:Tmean,] = inception.R[1:Tmean, ] - ZZ;
       if( nrow(inception.R)>(Tmean+1) ){
                 A = M[Tmean:(nrow(inception.R)-1),];
                 A = zoo( A , order.by = time(inception.R[(Tmean+1):nrow(inception.R), ])) ; #shift dates; otherwise zoo poses problem
                 inception.R.cent[(Tmean+1):nrow(inception.R), ] = inception.R[(Tmean+1):nrow(inception.R), ] - A}

       
       ## Garch estimation
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
       in.sample.T = nrow(selectU);
       Rcor = matrix(rep(0,cAssets^2),nrow=cAssets,ncol=cAssets)
       M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
       M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3)

       for(t in c(1:in.sample.T)){
          centret = as.vector(selectU[t,]);
          Rcor = Rcor +  centret%*%t(centret)
          M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
          M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
       }
       resSigma = Rcor =  1/in.sample.T*Rcor
       sdzero = function(data){ return( sqrt(mean(data^2)) ) }
       D = diag( apply(selectU,2,'sdzero')^(-1),ncol=cAssets )
       Rcor = D%*%Rcor%*%D
       M3 = 1/in.sample.T*M3
       M4 = 1/in.sample.T*M4

       # we only need mean and conditional covariance matrix of last observation
       mu = matrix(tail(M,n=1),ncol=1 ) ;
       D = diag( as.vector(as.vector(tail(S,n=1) ) ),ncol=cAssets )
       sigma = D%*%Rcor%*%D

       ####### End GARCH estimation

       mESfun = function(series){ return( operMES(series,alpha=alpha,2) ) }
       
 
       if(strategy=="riskpicking"){
          vMES = apply( in.sample.R , 2 , 'mESfun' )
          lower = rep(0,cAssets); upper=rep(1,cAssets)
          RBlower = rep(-Inf,cAssets) ; RBupper = as.vector(vMES/sum(vMES))   ;
       }

       if(strategy=="riskaverse"){
          vMES = apply( in.sample.R , 2 , 'mESfun' )
          lower = rep(0,cAssets); upper=rep(1,cAssets)
          RBlower = as.vector(1-kfactor*vMES/sum(vMES))  ; RBupper = rep(Inf,cAssets)  ;
       }

       if(strategy=="equalweighted"){
           sol1 = rep(1/cAssets,cAssets);
           switch( percriskcontribcriterion ,
                StdDev = { percriskcontrib = function(w){ cont = Portsd(w,sigma=sigma)[[3]] ; return( cont  ) }},
                GVaR = {percriskcontrib = function(w){ cont =  PortgausVaR(w,alpha=alphariskbudget,mu=mu,sigma=sigma)[[3]] ; return( cont ) }},
                GES = {percriskcontrib = function(w){ cont =  PortgausES(w,mu=mu,alpha=alphariskbudget,sigma=sigma)[[3]] ; return( cont  ) }},
                mVaR = {percriskcontrib = function(w){ cont = resPortMVaR(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont  ) }},
                mES = {percriskcontrib = function(w){ cont = resoperPortMES(w,mu=mu,alpha=alphariskbudget,sigma=sigma,resSigma=resSigma,M3=M3,M4=M4)[[3]] ; return( cont ) }}
           )
           sol2 = percriskcontrib( sol1 )
           solution = list( sol1 , sol2 ) ;
       }else{
          solution = RBconportfolio( mincriterion = "mES" , percriskcontribcriterion = "mES" , mu = mu , sigma = sigma, resSigma = resSigma ,M3=M3 , M4=M4 ,
                        alpha = alpha , alphariskbudget = alphariskbudget , lower = lower , upper = upper , RBlower = RBlower, RBupper = RBupper ,
                        controlDE = controlDE )
       }
       out[ per, ]    = as.vector( solution[[1]] )
       RCout[per,  ]  = as.vector( solution[[2]] )

 }#end loop over the rebalancing periods; indexed by per=1,...,cPeriods


  # Output save
  rownames(out) = rownames(RCout) =  names.input; colnames(out) = colnames(RCout) = names.assets;

  EWweights = c( rep(1/cAssets,cAssets) )
  EWweights = matrix ( rep(EWweights,cPeriods) , ncol=(cAssets) , byrow = TRUE )
  rownames(EWweights) = names.input; colnames(EWweights) = names.assets;

  return( list( out, RCout) )
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

resoperMESfun = function(w,alpha,mu,sigma,resSigma,M3,M4){
    pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ; respm2 =  t(w)%*%resSigma%*%w ;
    skew = pm3 / respm2^(3/2);
    exkurt = pm4 / respm2^(2) - 3; z = qnorm(alpha);
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

resPortMVaR =  function(alpha,w,mu,sigma,resSigma,M3,M4,precision=4)
{
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   respm2 = m2(w,resSigma)
   dpm2 = as.vector( derm2(w,sigma) )
   drespm2 = as.vector( derm2(w,resSigma) )
   pm3 = m3(w,M3)
   dpm3 = as.vector( derm3(w,M3) )
   pm4 = m4(w,M4)
   dpm4 = as.vector( derm4(w,M4) )

   skew = pm3 / respm2^(3/2);
   exkurt = pm4 / respm2^(2) - 3;

   derskew = ( 2*(respm2^(3/2))*dpm3 - 3*pm3*sqrt(respm2)*drespm2 )/(2*respm2^3)
   derexkurt = ( (respm2)*dpm4 - 2*pm4*drespm2    )/(respm2^3)

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


resoperPortMES =  function(alpha,w,mu,sigma,resSigma,M3,M4,precision=4)
{
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   respm2 = m2(w,resSigma)
   dpm2 = as.vector( derm2(w,sigma) )
   drespm2 = as.vector( derm2(w,resSigma) )
   pm3 = m3(w,M3)
   dpm3 = as.vector( derm3(w,M3) )
   pm4 = m4(w,M4)
   dpm4 = as.vector( derm4(w,M4) )

   skew = pm3 / respm2^(3/2);
   exkurt = pm4 / respm2^(2) - 3;

   derskew = ( 2*(respm2^(3/2))*dpm3 - 3*pm3*sqrt(respm2)*drespm2 )/(2*respm2^3)
   derexkurt = ( (respm2)*dpm4 - 2*pm4*drespm2    )/(respm2^3)

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
   plot(xvar, y1var, xlab=labels[1], ylab="", axes=F, pch=marks[1],main=title)
   points(xvar, scaledy2var, pch=marks[2])

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

   legend(legpos[1], legpos[2], legend=leglabs, pch=marks, bty="n")
}

###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2009 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
# $Log: Risk_budget_functions.R,v $
# Revision 1.6  2009-10-27 15:53:45  brian
# - reorganize the output for ease of use, per Peter's request
#
# Revision 1.5  2009-10-27 00:45:20  brian
# - fix mVaR and StdDev objectives
#
# Revision 1.4  2009-10-26 20:58:23  brian
# - add standard measures to output
#
# Revision 1.3  2009-10-26 20:35:14  brian
# - add footer
#
# revision 1.2
# date: 2009-10-26 14:59:18 -0500;  author: brian;  state: Exp;  lines: +130 -1;  commitid: UA16jzVbLRP9a59u;
# - add new version of MaxReturnRBconportfolio modified to
#   - work with C version of DEoptim
#   - not have a cash instrument assumption
#   - optionally include DEoptim intermediate results
# ----------------------------
# revision 1.1
# date: 2009-10-26 12:22:03 -0500;  author: peter;  state: Exp;  commitid: Aq1plzcy283Ii49u;
# - Kris' first draft code 2009-08-15
# - based on code from Brian from ~2006-2008
###############################################################################