

setwd( "C:/Documents and Settings/Administrator/Desktop/risk budget programs/illustration" )

library( skewt )
n      = 5000 ;
df     = 5   ;
gamma  = 0.8 ;
simret = rskt( n = n , df = df , gamma = gamma  ) ;

postscript( "CVaR.eps" )

par( cex = 1.2 , cex.main = 1.2) 
hist( simret , freq = F , breaks = 20 , ylim =c(0,0.4), col="gray" , 
      main = "95% Value at Risk and 95% Conditional VaR \n for a Skewed Student t" , xlab = "Portfolio returns" , ylab = "Density" ) 
mi = min( simret ) ; ma = max( simret );
lines( seq(mi,ma,0.01) , dskt( seq(mi,ma,0.01), df = df , gamma = gamma ),lwd=2 )

abline ( v = qskt( 0.05 , df = df , gamma = gamma ) , lty = 3, lwd = 2 , col = "red"  )
text( x = qskt( 0.05 , df = df , gamma = gamma )  , y = 0.38 , labels = "95% VaR" , col = "red" , lwd = 2 , cex = 1.1)

library(graphics)
arrows( x0 =  min(simret) , x1 = qskt( 0.05 , df = df , gamma = gamma )  ,
        y0 =  0.1           , y1 = 0.1  , code = 3 , col = "blue" , lwd = 2 )
text( x = 0.5*(min(simret)+qskt( 0.05 , df = df , gamma = gamma ) ) , y = 0.12 , 
       labels = "95% CVaR" , col = "blue" , lwd = 2 , cex = 1.1)

dev.off()
