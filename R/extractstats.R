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

extractstats <- function(resultlist) {
    l = length(resultlist)
    result=matrix(nrow=l,ncol=49)
    colnames(result)<-c("mean_ret","sd","mVaR","mES",names(resultlist[[1]]$weights))
    for (i in 1:l) {
        if(!is.atomic(resultlist[[i]])){
            if(!is.null(resultlist[[i]]$stats)){
                result[i,1:4]<-t(resultlist[[i]]$stats)
            } else if(!is.null(resultlist[[i]]$mean_ret)){
                result[i,1]<-resultlist[[i]]$mean_ret
                result[i,2]<-resultlist[[i]]$sd
                result[i,3]<-resultlist[[i]]$VaR
                result[i,4]<-resultlist[[i]]$ES
            }
            result[i,]<-t(c(result[i,1:4],resultlist[[i]]$weights))
        }
    }
    return(result)
}