

Returns.rebalancing =
function (R, criteria, from , to , method = c("compound"), folder="" )
{ # @author Brian G. Peterson, Kris Boudt

    # R                 data structure of component returns
    #
    # criteria           vector containing the names of the csv files holding the portfolio weights 
    #
    # from, to          from[i]:to[i] indicates the i-th rebalancing period   
    #

    # Setup:
    result=NULL
    resultcols=NULL

    # data type conditionals
    # cut the return series for from:to
    if (class(R) == "timeSeries") {
        R = R@Data
    } else {
        R = R
    }
    R=checkData(R,method="zoo")
    cRebalancing = length(from);

    result.compound=c()
    result.simple=c()
     # Loop over the different optimisation criteria

     for(criterion in criteria){
        pcompoundreturn=psimplereturn=c();
        weights = read.csv( file = paste( criterion,".csv",sep=""),header = TRUE,  sep = ",", na.strings = "NA", dec = ".")
        if (ncol(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")
        portfoliovalue = 1;
        for (row in 1:cRebalancing){
               Rrebalperiod = window(R, start = as.Date(from[row]) , end = as.Date(to[row])) ;
               wealthindex = cumvalue( Rrebalperiod , weights=weights[row,] )
               compoundreturns = portfoliovalue*wealthindex - 1;
               pcompoundreturn=c(pcompoundreturn, compoundreturns )
               portfoliovalue = portfoliovalue*tail(wealthindex,n=1)  # portfolio value at end of rebalancing period i
               simplereturns = c(wealthindex[1],wealthindex[2:length(wealthindex)]/wealthindex[1:( length(wealthindex)-1 )] ) - 1;
               psimplereturn = c( psimplereturn , simplereturns ); 
        }
        result.compound = cbind(result.compound, pcompoundreturn)
        result.simple = cbind(result.simple, psimplereturn)
     }
     colnames(result.compound)=  colnames(result.simple) = criteria
     dates = time(window(R, start = as.Date(from[1]) , end = as.Date(tail(to,n=1))))

     compoundreturns = zoo( result.compound , order.by = dates );
     simplereturns = zoo( result.simple , order.by = dates ) ;

     save( compoundreturns , file = paste(  getwd(),"/", folder , "compoundreturns.Rdata" , sep="")   )
     save( simplereturns   , file = paste( getwd(),"/", folder , "simplereturns.Rdata" , sep="")     )

     write.zoo( compoundreturns , file = paste(  getwd(),"/", folder , "compoundreturns.txt" , sep="")   , row.names=F )
     write.zoo( simplereturns   , file = paste( getwd(),"/", folder , "simplereturns.txt" , sep="")     , row.names=F )
}

cumvalue <- function (R, weights=NULL)
{   # @author Brian G. Peterson,Kris Boudt
    # Setup:
    R=checkData(R,method="zoo")

    if (is.null(weights)){
        # set up an equal weighted portfolio
        weights = t(rep(1/ncol(R), ncol(R)))
    }

    if (ncol(weights) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    wealthindex.assets= cumprod( 1+R )  

    # build a structure for our weighted results
    wealthindex.weighted = matrix(nrow=nrow(R),ncol=ncol(R))
    colnames(wealthindex.weighted)=colnames(wealthindex.assets)
    rownames(wealthindex.weighted)=rownames(wealthindex.assets)

    # weight the results
    for (col in 1:ncol(weights)){
        wealthindex.weighted[,col]=weights[,col]*wealthindex.assets[,col]
    }
    wealthindex=apply(wealthindex.weighted,1,sum)

    return(wealthindex)
} # end 
