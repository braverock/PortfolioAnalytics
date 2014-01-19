
TAA = function( R , minreturn = 0.05, 
                lowerweights =  NA, upperweights = NA, alpha = 0.05,clean = F,
                lowerESriskbudgetweights = -Inf, upperESriskbudgetweights = +Inf ,
                 nrestarts = 60, nrounds = 10 , nsteps = 500 ,increment = 0.01)
{
   # Author: Kris Boudt 
   # This function uses the threshold accepting algorithm in Gilli, Kellezi and Hysi (Journal of Risk, 2006) 
   # to find the portfolio with highes ES sharpe ratio and that respect the ES risk budget constraints 

   T = nrow(R); N=ncol(R);
   if( is.na(lowerweights) ){ lowerweights = rep(0.0,N) } # no short sales
   if( is.na(upperweights) ){ upperweights = rep(0.3,N) } # no concentration

   if(clean){ R = cleaning(R,alpha=alpha,trim=1e-3)[[1]] };
   mu =  apply(R,2,'mean' ) 
   sigma = cov(R)
   invSigma = solve(sigma)
   M3 = matrix(rep(0,N^3),nrow=N,ncol=N^2)
   M4 = matrix(rep(0,N^4),nrow=N,ncol=N^3)
   for(t in c(1:T)){
      centret = as.numeric(matrix(R[t,]-as.vector(mu),nrow=N,ncol=1))
      M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
      M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
   }
   M3 = 1/T*M3;
   M4 = 1/T*M4;

   # Define function to be minimized 
   NegSRMES = function( w )
   {
      largevalue = 100;
      MES = operPortMES(alpha,w,mu,sigma,M3,M4)
      MESi = MES[[1]];
      portreturni = sum( w*mu)
      return( -portreturni/MESi  ) 
   }

   # Define functions that will generate random solutions that respect the constraints
   ransol.h = function(N)
   {
     w = rep(0,N); vassets = c(1:N);
     while( sum(w) <1 ){
        i = vassets[ceiling( runif(1,0.01,length(vassets)) ) ]; 
        w[i] = round( min( runif( 1 , lower[i] , upper[i] ) , 1-sum(w) )  , digits= 2);
        vassets = setdiff( vassets , i) ; 
        if(length(vassets)==1){w[vassets[1]] = 1-sum(w) ;  } 
     }
     return(w)
   }

  ransol = function(N)
  {
      viol = T;
      while( viol ){
               neww = ransol.h(N);
               MES = operPortMES(alpha,neww,mu,sigma,M3,M4)
               MESi = MES[[1]];
               portreturni = sum( neww*mu*12 )
               percentcontrib = MES[[3]]
               viol= ( any( (percentcontrib > upperESriskbudgetweights) | (percentcontrib < lowerESriskbudgetweights) ) | (portreturni<minreturn) )
       }
       return(neww)     
  }

  # Function that generates neigbors 
  # This can take some time because the neigborhood is constrained not only by the weight constraints but also by the ES risk budget constraints

  neighbor = function( w )
  {
      viol = T;
      while( viol ){
           # exchange incr between two positions, ensures that if initial value satisfies budget constraint, it will remain satisfied
           partners = ceiling( runif(2,0,N)  ); 
                # ceiling( runif(2*npartners,0,N)  );
           neww = w ; neww[partners ] = neww[partners]+ increment*c(+1,-1) ;
           viol = ( any( neww < lower ) | any( neww > upper) )
           if(!viol){ 
               MES = operPortMES(alpha,neww,mu,sigma,M3,M4)
               MESi = MES[[1]];
               portreturni = sum( w*mu*12 )
               percentcontrib = MES[[3]]
               viol= ( any( (percentcontrib > upperESriskbudgetweights) | (percentcontrib < lowerESriskbudgetweights) ) | (portreturni<minreturn) )
           }
       }
       return(neww)
  }

  initw = ransol( N);

  # Generation of threshold sequence (Algorithm 3 in Gilli, Kellezi and Hysi (Journal of Risk, 2006)  )
  vdelta = rep(0,nsteps)
  for( i in 1:nsteps){
      neww = neighbor(initw);
      vdelta[i] = abs( NegSRMES(initw) - NegSRMES(neww) );
  }
  qthreshold = quantile( x = vdelta , probs = seq(0.01,1,0.01) ) ;
  plot(seq(0.01,1,0.01) , qthreshold , main = "Quantile function of Delta used for setting threshold"  )
 
  bestw = initw ; bestf = NegSRMES( initw ); 

  for( k in 1:nrestarts ){ 
         print( c("restart",k,"out of", nrestarts) ); 
         initw = ransol( N) ; localbestw = initw ; initf = localbestf = NegSRMES( initw );  
         for( r in 1:nrounds ){
            # the threshold tau > 0 : "if we want to escape local minima, the algorithm must accept uphill moves"
            # the threshold is gradually reduced to zero in a deterministic way
            taur = round( 0.8*(  nrounds - r )/nrounds , digits=2 )*100;
            for( i in 1:nsteps ){
               neww = neighbor(initw); newf = NegSRMES(neww);
               delta = newf-initf ;
               if( (delta < qthreshold[ taur +1 ]) ){ localbestw = initw ; localbestf = newf; } 
            }
         }
         if( localbestf < bestf ){ bestw = localbestw ; bestf = localbestf ; }
  }
 
   return( bestw  )
}

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
###############################################################################
