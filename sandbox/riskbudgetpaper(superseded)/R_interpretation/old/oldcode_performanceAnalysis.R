

R=checkData(monthlyR,method="zoo")
oosR = R[ ( index(R)>=head(from,1)& index(R)<=tail(from,1)),]


oospercCVaR = function( weightedR , seriesVaR ){ series = rowSums(weightedR) ; q = as.numeric( seriesVaR  ) ; 
               out = -weightedR[series<(-q),]; #print(out)  # Maal gewichten doen
               return( apply(out,1,'max')/apply(out,1,'sum') )
}

out1 = out2 = out3 = out4 = out5 = out6 = out7 = out8 = out9 = out10 = c();

for( strat in 1:10 ){
    criterion = criteria[strat];
    weightedR = c(); portfolioVaR = c();
    weights = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
    for (row in 1:length(from)){
          Rrebalperiod = window(R, start = as.Date(from[row]) , end = as.Date(to[row])) ;
          weightedR = rbind( weightedR , matrix( rep( as.numeric(weights[row,]),nrow(Rrebalperiod)) , nrow = nrow(Rrebalperiod) )*Rrebalperiod   );  
          pfoosR = rowSums( matrix( rep( as.numeric(weights[row,]),nrow(oosR)) , nrow = nrow(oosR) )*oosR )
          portfolioVaR = c( portfolioVaR , histVaR( pfoosR ) ) ;
    }
    if( strat==1 ){ out1 = rbind( out1 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==2 ){ out2 = rbind( out2 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==3 ){ out3 = rbind( out3 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==4 ){ out4 = rbind( out4 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==5 ){ out5 = rbind( out5 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==6 ){ out6 = rbind( out6 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==7 ){ out7 = rbind( out7 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==8 ){ out8 = rbind( out8 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==9 ){ out9 = rbind( out9 , oospercCVaR( weightedR , portfolioVaR ) ) }
    if( strat==10){ out10= rbind( out10 , oospercCVaR( weightedR , portfolioVaR ) ) }
}

out = cbind( namelabels , 
    round( c( mean(out1) , mean(out2) , mean(out3) , mean(out4) , mean(out5) , mean(out6) , mean(out7) , mean(out8) , mean(out9) , mean(out10) ) , 2 ) ,
    round( c( median(out1) , median(out2) , median(out3) , median(out4) , median(out5) , median(out6) , median(out7) , median(out8) , median(out9) , median(out10) ) , 2 ) ,
    round( c( min(out1) , min(out2) , min(out3) , min(out4) , min(out5) , min(out6) , min(out7) , min(out8) , min(out9) , min(out10) ) , 2 ) ,
    round( c( max(out1) , max(out2) , max(out3) , max(out4) , max(out5) , max(out6) , max(out7) , max(out8) , max(out9) , max(out10) ), 2 ) 
    round( c( max(out1) , max(out2) , max(out3) , max(out4) , max(out5) , max(out6) , max(out7) , max(out8) , max(out9) , max(out10) ), 2 ) 

)
colnames( out ) = c( "Strategies" , "mean" , "median" , "min" , "max" )
print( out )

oospercCVaR = function( weightedR , seriesVaR ){ series = rowSums(weightedR) ; q = as.numeric( seriesVaR  ) ; 
               out = -weightedR[series<(-q),]; #print(out)  # Maal gewichten doen
               return( out/apply(out,1,'sum') )
}

oosCVaR = function( weightedR , seriesVaR ){ series = rowSums(weightedR) ; q = as.numeric( seriesVaR  ) ; 
               out = -weightedR[series<(-q),]; #print(out)  # Maal gewichten doen
               return( out )
}


# Sharpe and Sortino ratios with respect to risk free return

excessoosreturns = oosreturns - monthlyRTBill;
sqrt(12)*apply( excessoosreturns , 2 , 'mean' )/apply( oosreturns , 2 , 'sd' )

sqrt(12)*apply( excessoosreturns , 2 , 'mean' )/apply( oosreturns , 2 , 'histCVaR' )
