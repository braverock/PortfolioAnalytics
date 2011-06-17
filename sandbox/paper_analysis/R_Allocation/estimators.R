# Function for data cleaning
#-------------------------------------------

cleaning = function(mData, alpha , trim=1e-3)
{
   T=dim(mData)[1];
   N=dim(mData)[2];
   MCD = covMcd(mData,alpha=1-alpha)
   mu = as.matrix(MCD$raw.center) #no reweighting
   sigma = MCD$raw.cov 
   invSigma = solve(sigma);
   vd2t = c();
   cleaneddata = mData
   outlierdate = c()

   # 1. Sort the data in function of their extremeness
   # Extremeness is proxied by the robustly estimated squared Mahalanbobis distance

   for(t in c(1:T) )
   {
      d2t = as.matrix(mData[t,]-mu)%*%invSigma%*%t(as.matrix(mData[t,]-mu));      
      vd2t = c(vd2t,d2t);
   }
   sortt = sort(vd2t,index.return=TRUE)$ix

   # 2. Outlier detection
   # 2.1. Only the alpha most extreme observations can be qualified as outliers

   T.alpha = floor(T * (1-alpha))+1
   cleanedt=sortt[c(T.alpha:T)]  

   # 2.2. multivariate winsorization (Khan et al, 2007) : 
   # bound the MD of the most exteme observations to a quantile of the chi squared distribution, N df

   for(t in cleanedt ){
        if(vd2t[t]>qchisq(1-trim,N)){
              # print(c("Observation",as.character(date[t]),"is detected as outlier and cleaned") );
               cleaneddata[t,] = sqrt(qchisq(1-trim,N)/vd2t[t])*mData[t,]; 
               outlierdate = c(outlierdate,date[t]) } }
   return(list(cleaneddata,outlierdate))
}

# Definition of statistics needed to compute Gaussian and modified VaR and ES for univariate time series
#---------------------------------------------------------------------------------------------------------

centeredmoment = function(series,power)
{
   location = mean(series);
   out = sum( (series-location)^power  )/length(series);
   return(out);
}

skew = function(series)
{
   m2 = centeredmoment(series,2)
   m3 = centeredmoment(series,3)
   out = m3 / m2^(3/2)
   return(out)
}

exkur =  function(series)
{
   m2 = centeredmoment(series,2)
   m4 = centeredmoment(series,4)
   out = m4 / m2^(2) - 3
   return(out)
}

pvalJB = function(series)
{
   m2 = centeredmoment(series,2)
   m3 = centeredmoment(series,3)
   m4 = centeredmoment(series,4)
   skew = m3 / m2^(3/2);
   exkur = m4 / m2^(2) - 3;
   JB = ( length(series)/6 )*( skew^2 + (1/4)*(exkur^2) )
   out = 1-pchisq(JB,df=2)
}

gausVaR =  function(series,alpha)
{
   location = mean(series);
   m2 = centeredmoment(series,2)
   out = - location - qnorm(alpha)*sqrt(m2)
   return(out)
}

gausES =  function(series,alpha)
{
   location = mean(series);
   m2 = centeredmoment(series,2)
   out = - location + dnorm(qnorm(alpha))*sqrt(m2)/alpha
   return(out)
}

MVaR =  function(series,alpha,r)
{
   z = qnorm(alpha)
   location = mean(series);
   m2 = centeredmoment(series,2)
   m3 = centeredmoment(series,3)
   m4 = centeredmoment(series,4)
   skew = m3 / m2^(3/2);
   exkurt = m4 / m2^(2) - 3;

   h = z + (1/6)*(z^2 -1)*skew 
   if(r==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2; }

   out = - location - h*sqrt(m2)
   return(out)
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

MES =  function(series,alpha,r=2)
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
   MES = - location + (sqrt(m2)/alpha)*MES
   return(MES)
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

PortgausVaR =  function(alpha,w,mu,sigma,precision=4)
{
   location = t(w)%*%mu
   pm2 = m2(w,sigma)
   dpm2 = derm2(w,sigma)
   VaR = - location - qnorm(alpha)*sqrt(pm2)
   derVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = derVaR*as.vector(w) 
   return(list( round( VaR , precision ) , round ( contrib , precision ) , round( contrib/VaR , precision) )) 
}

PortgausES =  function(alpha,w,mu,sigma,precision=4)
{
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

realportES = function(data,w,VaR,precision=4)
{
    T = dim(data)[1]
    N = dim(data)[2]
    c_exceed = 0;
    r_exceed = 0;
    realizedcontrib = rep(0,N);
    for(t in c(1:T) )
    {
       rt = as.vector(data[t,])
       rp = sum(w*rt)
       if(rp<=-VaR){
          c_exceed = c_exceed + 1;
          r_exceed = r_exceed + rp;
          for( i in c(1:N) ){
             realizedcontrib[i] =realizedcontrib[i] + w[i]*rt[i] }
       }
    }
    realizedcontrib=as.numeric(realizedcontrib)/r_exceed ; 
    return( list(round(-r_exceed/c_exceed,precision),c_exceed,round(realizedcontrib,precision)) )
}

realportVaR = function(data,w,alpha,precision=4)
{
    portret = c();
    T = dim(data)[1]
    N = dim(data)[2]
    for( t in c(1:T) ){
       portret = c(portret,sum(w*as.numeric(data[t,])))
    }
    VaR = sort(portret)[floor(alpha*T)]
    return(-VaR)
}