
# Code that produces the graph showing the difference between VaR and CVaR 
# For a Skewed Student t distribution
#---------------------------------------------------------------------------

# skewedstudentt : sst

# a student has variance nu/(nu-2)
# ! standardized to have variance 1

# It has as a basis distribution the standardized student t 

dstd = 
function(x, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz
    # Compute the density for the standardized Student-t distribution.
    # Density function can be found in Bollerslev (1987)

    if(nu!=Inf){
       s = sqrt(nu/(nu-2))
       z = (x - mean) / sd
       result = dt(x = z*s, df = nu) * s / sd
    }else{ result = dnorm(x)}
    return(result)
}

pstd =
function (q, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz
    # Description:  Compute the probability for the standardized Student-t distribution.

    if(nu!=Inf){    
       s = sqrt(nu/(nu-2))
       z = (q - mean) / sd
       result = pt(q = z*s, df = nu)
    }else{ result = pnorm(q) }
    return(result)
}

qstd =
function (p, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz
    # Description: compute the quantiles for the standardized Student-t distribution.

    if(nu!=Inf){
       s = sqrt(nu/(nu-2))
       result = qt(p = p, df = nu) * sd / s + mean
    }else{ result = qnorm(p=p)  }
    return(result)
}

m = function(nu)
{
   m = gamma( (nu-1)/2 )*sqrt(nu-2) 
   m = m / ( sqrt(pi)*gamma(nu/2) )
   return(m)
}
#converges to 0.8


sstVAR=function(alpha,nu,xi) # computes VaR using the quantile function in Lambert and Laurent (2001), Giot and Laurent (2003)
{
   # the quantile function depends on the value of alpha
   nonstsstq = ifelse( alpha < 1/(1+xi^2)  , ( 1/(xi) )*qstd(  p=   (alpha/2)*(1+xi^2)  ,nu =nu ),
                                             -(xi)*qstd( p= 0.5*(1-alpha)*(1+ 1/xi^2) , nu =nu  ) )
   # compute mean and standard deviation of non standardized skewed student
   if(nu<150){
      m = gamma( (nu-1)/2 )*sqrt(nu-2) 
      m = m / ( sqrt(pi)*gamma(nu/2) )}else{m=0.8}
   m = m*( xi - 1/xi )
   s = sqrt( xi^2 + (1/xi^2) - 1  - m^2 )
   # compute skewed student t quantile
   stsstq = (nonstsstq-m)/s
   # VAR is the negative quantile
   return(-stsstq) 
}

sstES=function(alpha,nu,xi) # computes ES as the integred VaR for a skewed student t
{
   # compute mean and standard deviation of non standardized skewed student
   if(nu<150){
         m = gamma( (nu-1)/2 )*sqrt(nu-2) 
         m = m / ( sqrt(pi)*gamma(nu/2) )}else{m=0.8}
   m = m*( xi - 1/xi )
   s = sqrt( xi^2 + (1/xi^2) - 1  - m^2 )

   # the quantile function depends on the value of alpha

   # For alpha in [0,1/(1+xi^2)]:
   vint = c()
   for ( i in c(1:length(alpha)) ) 
   {
      # For alpha in 0,1/(1+xi^2)
      sstVAR=function(alpha) # computes VaR using the quantile function in Lambert and Laurent (2001), Giot and Laurent (2003)
      {
         nonstsstq =  ( 1/(xi) )*qstd(  p=   (alpha/2)*(1+xi^2)  ,nu =nu )
         stsstq = (nonstsstq-rep(m,length(alpha)))/s
         return(-stsstq) # VAR is the negative quantile
      }
      int = integrate(sstVAR,lower=0,upper= min(alpha[i],1/(1+xi^2)) )$value

      # For alpha in [1/(1+xi^2),1]:

      sstVAR=function(alpha) # computes VaR using the quantile function in Lambert and Laurent (2001), Giot and Laurent (2003)
      {
         nonstsstq =  -(xi)*qstd( p= 0.5*(1-alpha)*(1+ 1/xi^2) , df=nu  ) 
         stsstq = (nonstsstq-rep(m,length(alpha)))/s
         return(-stsstq) # VAR is the negative quantile
     }
     int = int + ifelse( alpha[i] >  1/(1+xi^2) , integrate(sstVAR,lower=1/(1+xi^2),upper=alpha[i])$value ,0)
     vint = c(vint,int)
   }
   ES = vint/alpha
   return(ES) 
}