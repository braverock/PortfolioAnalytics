

setwd("C:\\Documents and Settings\\Administrator\\Desktop\\risk budget programs\\illustration")
# Bivariate normal example of the effect of constraints on percentage CVaR on the feasible space

showweightlines = T;
srt = 0 ; 

CVaR = function( w , mu , sigma , alpha )
{
   out = -t(w)%*%mu + sqrt( t(w)%*%sigma%*%w )*dnorm(qnorm(alpha))/alpha
   return( out );
}

CompCVaR = function( w , mu , sigma , alpha )
{
   out = -mu + as.vector(sigma%*%w)*(1/sqrt( t(w)%*%sigma%*%w))*dnorm(qnorm(alpha))/alpha
   return( w*out );
} 

N = 2; rho1 = -0.5 ; rho2 = 0; rho3 = 0.5; alpha = 0.05
mu1 = as.matrix( rep(0,N) ) ; mu2 = as.matrix( c(1, 0 ) ) ; 
sigma1 = sigma2 = matrix ( rep(rho1,N^2) , ncol = N )
sigma3 = sigma4 = matrix ( rep(rho2,N^2) , ncol = N )
sigma5 = sigma6 = matrix ( rep(rho3,N^2) , ncol = N )
diag(sigma1) = diag(sigma2) = diag(sigma3) = diag(sigma4) = diag(sigma5) = diag(sigma6) =1;
sigma2[2,2] = sigma4[2,2] = sigma6[2,2] = 4;

w = as.matrix( rep(0.5,2) ) 

percCompCVaR = function( w = w , mu = mu , sigma = sigma , alpha = alpha ){
   out1 = CompCVaR( w = w , mu = mu , sigma = sigma , alpha = alpha )
   out2 = CVaR( w = w , mu = mu , sigma = sigma , alpha = alpha )
   return(out1/as.numeric(out2))
}

vw1 = seq(0,1,0.01)
constraint = rep(0,length(vw1));
i=0;
vpccvar1 = vpccvar2 = vpccvar3 = vpccvar4 = vpccvar5 = vpccvar6 = vpccvar7 = vpccvar8 = vpccvar9 = c();
for( w1 in vw1 )
{
   w2 = 1-w1; i = i+1;
   w = as.matrix( c(w1,w2) )
   vpccvar1 = rbind( vpccvar1 , percCompCVaR( w = w , mu = mu1 , sigma = sigma1 , alpha = alpha )[1])
   vpccvar2 = rbind( vpccvar2 , percCompCVaR( w = w , mu = mu2 , sigma = sigma1 , alpha = alpha )[1])
   vpccvar3 = rbind( vpccvar3 , percCompCVaR( w = w , mu = mu1 , sigma = sigma2 , alpha = alpha )[1])
   vpccvar4 = rbind( vpccvar4 , percCompCVaR( w = w , mu = mu1 , sigma = sigma3 , alpha = alpha )[1])
   vpccvar5 = rbind( vpccvar5 , percCompCVaR( w = w , mu = mu2 , sigma = sigma3 , alpha = alpha )[1])
   vpccvar6 = rbind( vpccvar6 , percCompCVaR( w = w , mu = mu1 , sigma = sigma4 , alpha = alpha )[1])
   vpccvar7 = rbind( vpccvar7 , percCompCVaR( w = w , mu = mu1 , sigma = sigma5 , alpha = alpha )[1])
   vpccvar8 = rbind( vpccvar8 , percCompCVaR( w = w , mu = mu2 , sigma = sigma5 , alpha = alpha )[1])
   vpccvar9 = rbind( vpccvar9 , percCompCVaR( w = w , mu = mu1 , sigma = sigma6 , alpha = alpha )[1])
}

# Plot 9 x 9

if(showweightlines){ postscript('sensitivity.eps')  }else{ postscript('sensitivity_noweightlines.eps')}

par( mfrow = c(3,3) , las = 1 , mar=c(2.2,2.5,2,1.5) , cex=1 , cex.main=1)

# !!!! Setup 1: mu1, sigma1
plot( vw1 , vpccvar1 , main = expression( mu[1]*"="*mu[2]*"=0 and "*sigma[1]*"="*sigma[2]*"=1") ,
         type="l",ylab="" )
text( x = 0.1 , y = 0.95*max(vpccvar1) , labels=expression( rho*"="*-0.5) )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
if(showweightlines){ abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )}
# Indicate ERC portfolio
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) ); return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma1 , alpha = alpha )[1] ) }
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
print(ERC); print( 1/2 ) ; 
text( x = ERC , y = 0.5 , labels="ERC" , srt = srt )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }

# !!!! Setup 3: mu1, sigma2
plot( vw1 , vpccvar3 , main = expression( mu[1]*"="*mu[2]*"=0 and "*sigma[1]*"=1, "*sigma[2]*"=2   "), type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar3) , labels=expression( rho*"="*-0.5) )
if(showweightlines){  abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma2 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
print(ERC); print( 2/(1+2) ) ; 
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }




# !!!! Setup 2: mu2, sigma1
plot( vw1 , vpccvar2 , main = expression( mu[1]*"=1, "*mu[2]*"=0 and "*sigma[1]*"="*sigma[2]*"=1") , type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar2) , labels=expression( rho*"="*-0.5) )
if(showweightlines){ abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 ) }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu2 , sigma = sigma1 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }


#---------------------------------------------------------------------

# !!!! Setup 4: mu1, sigma3
plot( vw1 , vpccvar4 , main = "" , type="l",ylab="" )
text( x = 0.1 , y = 0.95*max(vpccvar4)  , labels=expression( rho*"="*0) )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
if(showweightlines){  abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )  }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma3 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }

# !!!! Setup 6: mu1, sigma4
plot( vw1 , vpccvar6 , main = "", type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar6)  , labels=expression( rho*"="*0) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )  }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma4 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }

# !!!! Setup 5: mu2, sigma3
plot( vw1 , vpccvar5 , main = "" , type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar5)  , labels=expression( rho*"="*0) )
if(showweightlines){  abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu2 , sigma = sigma3 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }



#----

# !!!! Setup 7: mu1, sigma5
plot( vw1 , vpccvar1 , main = expression( mu[1]*"="*mu[2]*"=0 and "*sigma[1]*"="*sigma[2]*"=1") ,
         type="l",ylab="" )
text( x = 0.1 , y = 0.95*max(vpccvar1) , labels=expression( rho*"="*0.5) )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
if(showweightlines){ abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )}
# Indicate ERC portfolio
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) ); return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma1 , alpha = alpha )[1] ) }
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
print(ERC); print( 1/2 ) ; 
text( x = ERC , y = 0.5 , labels="ERC" , srt = srt )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }


# !!!! Setup 9: mu1, sigma6
plot( vw1 , vpccvar9 , main = "" , type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar9)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma6 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }


# !!!! Setup 8: mu2, sigma5
plot( vw1 , vpccvar8 , main = "", type="l" ) 
text( x = 0.1 , y = 0.95*max(vpccvar8)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu2 , sigma = sigma5 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }



#----------------------------------------------------------------------------------------



dev.off()



####################### Focus on effect of mu and sigma



postscript('sensitivity_rho50.eps') 

par( mfrow = c(3,1) , las = 1 , mar=c(2.2,6.5,2,1.5)  , cex=1 , cex.main=1.2)

#---------------------------------------------------------------------

showweightlines = TRUE;


# !!!! Setup 7: mu1, sigma5
plot( vw1 , vpccvar7 , main = expression( mu[1]*"="*mu[2]*"=0 and "*sigma[1]*"="*sigma[2]*"=1") , type="l",
   ylab="Perc CVaR asset 1 \n in function of its weight" )
#text( x = 0.1 , y = 0.95*max(vpccvar7) , labels=expression( rho*"="*0.5) )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
if(showweightlines){ abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )}
# Indicate ERC portfolio
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) ); return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma5 , alpha = alpha )[1] ) }
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
print(ERC); print( 1/2 ) ; 
#text( x = ERC , y = 0.5 , labels="ERC" , srt = srt )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }


# !!!! Setup 9: mu1, sigma6
plot( vw1 , vpccvar9 , main = expression( mu[1]*"="*mu[2]*"=0, "*rho*"=0, "* sigma[1]*"=1, "*sigma[2]*"=2   ") , type="l" ,
    ylab="Perc CVaR asset 1 \n in function of its weight" )
#text( x = 0.1 , y = 0.95*max(vpccvar9)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma6 , alpha = alpha )[1] )
}
#h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
#text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }


# !!!! Setup 8: mu2, sigma5
plot( vw1 , vpccvar8 , main = expression( mu[1]*"=1, "*mu[2]*"=0, "*sigma[1]*"="*sigma[2]*"=1"), type="l" , 
 ylab="Perc CVaR asset 1 \n in function of its weight") 
#text( x = 0.1 , y = 0.95*max(vpccvar8)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu2 , sigma = sigma5 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
#text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) }

dev.off()


















# Plot 1 x 3

postscript('sensitivity_rho50.eps') 
showweightlines = TRUE
par( mfrow = c(1,3) , las = 1 , mar=c(2.2,2.5,2,1.5) , cex=1 , cex.main=1)

# !!!! Setup 7: mu1, sigma5
plot( vw1 , vpccvar7 , main = expression( mu[1]*"="*mu[2]*"=0, "*rho*"="*0*", "*sigma[1]*"="*sigma[2]*"=1") , type="l" )
text( x = 0.1 , y = 0.95*max(vpccvar7)  , labels=expression( rho*"="*0.5) )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma5 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) ;
                     abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 ) }

# !!!! Setup 9: mu1, sigma6
plot( vw1 , vpccvar9 , main = main = expression( mu[1]*"=1, "*mu[2]*"=0, "*rho*"="*0*", "*sigma[1]*"="*sigma[2]*"=1") , type="l"  )
text( x = 0.1 , y = 0.95*max(vpccvar9)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu1 , sigma = sigma6 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) ;
                     abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 ) }


# !!!! Setup 8: mu2, sigma5
plot( vw1 , vpccvar8 , main = "", type="l" ) 
text( x = 0.1 , y = 0.95*max(vpccvar8)  , labels=expression( rho*"="*0.5) )
if(showweightlines){   abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )   }
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( h - percCompCVaR( w = w , mu = mu2 , sigma = sigma5 , alpha = alpha )[1] )
}
h=0.5; ERC = uniroot( objective , lower = 0 , upper = 1)$root
text( x = ERC , y = 0.5 , labels="ERC" )
# uniroot: find value for which it is 0.4 and 0.6
h=0.4 ; v1 = uniroot( objective , lower = 0 , upper = 1)$root
h=0.6; v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){ abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) ;
                     abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 ) }

# !!!! Setup 4: mu1, sigma3
plot( vw1 , vpccvar4 , main = expression( mu[1]*"="*mu[2]*"=0, "*rho*"="*0*", "*sigma[1]*"="*sigma[2]*"=1") , 
      type="l",ylab="Perc CVaR asset 1 \n in function of its weight" )
# %C[2]CVaR <= 60% implies %C[1]CVaR >= 40%
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu1 , sigma = sigma3 , alpha = alpha )[1] )
}
v1 = uniroot( objective , lower = 0 , upper = 1)$root
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu1 , sigma = sigma3 , alpha = alpha )[1] )
}
v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){  abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 ) ;
                    abline( h = 0.4 , lty = 3) ; abline( h = 0.6 , lty = 3 )  }
abline( h=0.5 , lty = 3 )

# !!!! Setup 5: mu2, sigma3
plot( vw1 , vpccvar5 , main = expression( mu[1]*"=1, "*mu[2]*"=0, "*rho*"="*0*", "*sigma[1]*"="*sigma[2]*"=1") , 
     type="l" , ylab = "Perc CVaR asset 1 \n in function of its weight" )
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu2 , sigma = sigma3 , alpha = alpha )[1] )
}
v1 = uniroot( objective , lower = 0 , upper = 1)$root
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu2 , sigma = sigma3 , alpha = alpha )[1] )
}
v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){   abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 )   }
abline( h=0.5 , lty = 3 )

# !!!! Setup 6: mu1, sigma4
plot( vw1 , vpccvar6 , main = expression( mu[1]*"="*mu[2]*"=0, "*rho*"=0, "* sigma[1]*"=1, "*sigma[2]*"=2   "), 
    type="l" , ylab = "Perc CVaR asset 1 \n in function of its weight" )
# uniroot: find value for which it is 0.4 and 0.6
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu1 , sigma = sigma4 , alpha = alpha )[1] )
}
v1 = uniroot( objective , lower = 0 , upper = 1)$root
objective = function( w1 ){
   w = as.matrix( c(w1,1-w1) );
   return( 0.5 - percCompCVaR( w = w , mu = mu1 , sigma = sigma4 , alpha = alpha )[1] )
}
v2 = uniroot( objective , lower = 0 , upper = 1)$root
if(showweightlines){   abline( v = v1 , lty = 3) ; abline( v = v2 , lty = 3 )   }
abline( h=0.5 , lty = 3 )

dev.off()
