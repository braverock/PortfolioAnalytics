

nreplications = 10000
samplesize = 52
Y = matrix( rnorm(samplesize*nreplications ) , ncol=nreplications ) ;
vskewnessoneyear = apply( Y , 2 , 'skewness' ) ; vkurtosisoneyear = apply( Y , 2 , 'kurtosis' ) 

samplesize = 104
Y = matrix( rnorm(samplesize*nreplications ) , ncol=nreplications ) ;
vskewnesstwoyear = apply( Y , 2 , 'skewness' ) ; vkurtosistwoyear = apply( Y , 2 , 'kurtosis' ) 


samplesize = 156
Y = matrix( rnorm(samplesize*nreplications ) , ncol=nreplications ) ;
vskewnessthreeyear = apply( Y , 2 , 'skewness' ) ; vkurtosisthreeyear = apply( Y , 2 , 'kurtosis' ) 


par(mfrow=c(2,1),mar=rep(2,4) )
boxplot(cbind(vskewnessoneyear,vskewnesstwoyear,vskewnessthreeyear))
boxplot(cbind(vkurtosisoneyear,vkurtosistwoyear,vkurtosisthreeyear))

summary(cbind(vskewnessoneyear,vskewnesstwoyear,vskewnessthreeyear))
