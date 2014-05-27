###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2014 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

#' compute comoments for use by lower level optimization functions when the conditional covariance matrix is a CCC GARCH model
#' 
#' it first estimates the conditional GARCH variances, then filters out the 
#' time-varying volatility and estimates the higher order comoments on the innovations 
#' rescaled such that their unconditional covariance matrix is the conditional covariance matrix forecast
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
#' @param \dots any other passthru parameters
#' @export
CCCgarch.MM = function(R, momentargs = NULL , ... )
{
    stopifnot("package:fGarch" %in% search() || require("fGarch",quietly=TRUE))
    if (!hasArg(momentargs) | is.null(momentargs)) 
        momentargs <- list()
    cAssets = ncol(R)
    T = nrow(R)
    if (!hasArg(mu)){ 
        mu = apply(R, 2, "mean")
    }else{ mu = match.call(expand.dots = TRUE)$mu }
    R = R - matrix( rep(mu,T) , nrow = T , byrow = TRUE )
    momentargs$mu = mu
    S = nextS = c();
    for( i in 1:cAssets ){
       gout =  garchFit(formula ~ garch(1,1), data = R[,i],include.mean = F, cond.dist="QMLE", trace = FALSE )
       if( as.vector(gout@fit$coef["alpha1"]) < 0.01 ){
               sigmat = rep( sd( as.vector(R[,i])), length(R[,i]) ); nextSt = sd( as.vector(R[,i]))
        }else{
               sigmat = gout@sigma.t; nextSt = predict(gout)[1,3]
        }
        S = cbind( S , sigmat); nextS = c(nextS,nextSt)
    }
    U = R/S; #filtered out time-varying volatility
    if (!hasArg(clean)){ 
        clean = match.call(expand.dots = TRUE)$clean
    }else{ clean = NULL }
    if(!is.null(clean)){ 
        cleanU <- try(Return.clean(U, method = clean))
        if (!inherits(cleanU, "try-error")) { U = cleanU }
    }
    Rcor = cor(U)
    D = diag( nextS ,ncol=cAssets )
    momentargs$sigma = D%*%Rcor%*%D
    # set volatility of all U to last observation, such that cov(rescaled U)=sigma 
    uncS = sqrt(diag( cov(U) ))
    U = U*matrix( rep(nextS/uncS,T  ) , ncol = cAssets , byrow = T )
    momentargs$m3 = PerformanceAnalytics:::M3.MM(U)
    momentargs$m4 = PerformanceAnalytics:::M4.MM(U)
    return(momentargs)
} 

#' set portfolio moments for use by lower level optimization functions
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
#' @param \dots any other passthru parameters
#' @export
set.portfolio.moments_v1 <- function(R, constraints, momentargs=NULL,...){

    if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
    if(is.null(constraints$objectives)) {
        warning("no objectives specified in constraints")
        next()
    } else {

        lcl <- grep('garch', constraints)
        if (!identical(lcl, integer(0))) {
            for (objective in constraints[lcl]) {
                objective = unlist(objective)
                if( is.null( objective$garch ) ) next
                if (objective$garch){
                   if (is.null(momentargs$mu)|is.null(momentargs$sigma)|is.null(momentargs$m3)|is.null(momentargs$m4))
                   {
                        momentargs =  CCCgarch.MM(R,clean=objective$arguments.clean,...)
                   }
               }
           }
        }


        lcl<-grep('clean',constraints)
        if(!identical(lcl,integer(0))) {
            for (objective in constraints[lcl]){
                objective = unlist(objective)
                #if(!is.null(objective$arguments$clean)) {
                if (!is.null(objective$arguments.clean)){
                   	if (is.null(momentargs$mu)|is.null(momentargs$sigma)|is.null(momentargs$m3)|is.null(momentargs$m4))
                   	{
                       	# cleanR<-try(Return.clean(R,method=objective$arguments$clean))
                       	cleanR <- try(Return.clean(R, method = objective$arguments.clean,...))
                    	if(!inherits(cleanR,"try-error")) {
                        	momentargs$mu = matrix( as.vector(apply(cleanR,2,'mean')),ncol=1);
                        	momentargs$sigma = cov(cleanR);
                        	momentargs$m3 = PerformanceAnalytics:::M3.MM(cleanR)
                        	momentargs$m4 = PerformanceAnalytics:::M4.MM(cleanR)
                        	#' FIXME NOTE: this isn't perfect as it overwrites the moments for all objectives, not just one with clean='boudt'
                    	}
                	}
            	}    
        	}
        }
        for (objective in constraints$objectives){
            switch(objective$name,
                    sd =,
                    StdDev = { 
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R, use='pairwise.complete.obs')
                    },
                    var =,
                    mVaR =,
                    VaR = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
                    },
                    es =,
                    mES =,
                    CVaR =,
                    cVaR =,
                    ES = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
                    }
            ) # end switch on objectives    
        }    
    }    
    return(momentargs)
}

#' set portfolio moments for use by lower level optimization functions
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization, see \code{\link{portfolio.spec}}
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
#' @param \dots any other passthru parameters
#' @aliases set.portfolio.moments
#' @rdname set.portfolio.moments
#' @export
set.portfolio.moments_v2 <- function(R, portfolio, momentargs=NULL,...){
  
  if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
  if(is.null(portfolio$objectives)) {
    warning("no objectives specified in portfolio")
    next()
  } else {
    
    # How would this be specified in the new portfolio.spec? As a constraint or in the portfolio part?
    # 
    lcl <- grep('garch', portfolio)
    if (!identical(lcl, integer(0))) {
      for (objective in portfolio[lcl]) {
        objective = unlist(objective)
        if( is.null( objective$garch ) ) next
        if (objective$garch){
          if (is.null(momentargs$mu)|is.null(momentargs$sigma)|is.null(momentargs$m3)|is.null(momentargs$m4))
          {
            momentargs =  CCCgarch.MM(R,clean=objective$arguments.clean,...)
          }
        }
      }
    }
    
    
    lcl<-grep('clean',portfolio)
    if(!identical(lcl,integer(0))) {
      for (objective in portfolio[lcl]){
        objective = unlist(objective)
        #if(!is.null(objective$arguments$clean)) {
        if (!is.null(objective$arguments.clean)){
          if (is.null(momentargs$mu)|is.null(momentargs$sigma)|is.null(momentargs$m3)|is.null(momentargs$m4))
          {
            # cleanR<-try(Return.clean(R,method=objective$arguments$clean))
            cleanR <- try(Return.clean(R, method = objective$arguments.clean,...))
            if(!inherits(cleanR,"try-error")) {
              momentargs$mu = matrix( as.vector(apply(cleanR,2,'mean')),ncol=1);
              momentargs$sigma = cov(cleanR);
              momentargs$m3 = PerformanceAnalytics:::M3.MM(cleanR)
              momentargs$m4 = PerformanceAnalytics:::M4.MM(cleanR)
              #' FIXME NOTE: this isn't perfect as it overwrites the moments for all objectives, not just one with clean='boudt'
            }
          }
        }    
      }
    }
    for (objective in portfolio$objectives){
      switch(objective$name,
             mean = {
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1)
               },
             var =,
             sd =,
             StdDev = { 
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1);
               if(is.null(momentargs$sigma)) momentargs$sigma = cov(R, use='pairwise.complete.obs')
             },
             mVaR =,
             VaR = {
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
               if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
               if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
               if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
             },
             es =,
             mES =,
             CVaR =,
             cVaR =,
             ETL=,
             mETL=,
             ES = {
               # We don't want to calculate these moments if we have an ES 
               # objective and are solving as an LP problem.
               if(hasArg(ROI)) ROI=match.call(expand.dots=TRUE)$ROI else ROI=FALSE
               if(!ROI){
                 if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
                 if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
                 if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
                 if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
               }
             }
      ) # end switch on objectives    
    }    
  }    
  return(momentargs)
}

# Alias for set.portfolio.moments
#' @export
set.portfolio.moments <- set.portfolio.moments_v2

garch.mm <- function(R,mu_ts, covlist,momentargs=list(),...) {
    #momentargs<-list()
    #momentargs$mu<-mu_ts[last(index(R)),]
    momentargs$mu<-mu_ts[last(index(R)),]
    
    momentargs$sigma<-covlist[as.character(last(index(R)))]
    if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics:::M3.MM(R)
    if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics:::M4.MM(R)
    return(momentargs)
}
###############################################################################
# $Id$
###############################################################################
