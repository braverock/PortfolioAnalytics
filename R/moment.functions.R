###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2018 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
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
  stopifnot("package:fGarch" %in% search() || requireNamespace("fGarch",quietly=TRUE))
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
    gout =  fGarch::garchFit(formula ~ garch(1,1), data = R[,i],include.mean = F, cond.dist="QMLE", trace = FALSE )
    if( as.vector(gout@fit$coef["alpha1"]) < 0.01 ){
      sigmat = rep( sd( as.vector(R[,i])), length(R[,i]) ); nextSt = sd( as.vector(R[,i]))
    }else{
      sigmat = gout@sigma.t; nextSt = fGarch::predict(gout)[1,3]
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
  momentargs$m3 = PerformanceAnalytics::M3.MM(U)
  momentargs$m4 = PerformanceAnalytics::M4.MM(U)
  return(momentargs)
} 

#' set portfolio moments for use by lower level optimization functions
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
#' @param \dots any other passthru parameters
set.portfolio.moments_v1 <- function(R, constraints, momentargs=NULL,...){
  
  if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
  if(is.null(constraints$objectives)) {
    warning("no objectives specified in constraints")
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
              momentargs$m3 = PerformanceAnalytics::M3.MM(cleanR)
              momentargs$m4 = PerformanceAnalytics::M4.MM(cleanR)
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
               if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(R)
               if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(R)
             },
             es =,
             mES =,
             CVaR =,
             cVaR =,
             ES = {
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
               if(is.null(momentargs$sigma)) momentargs$sigma = cov(R)
               if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(R)
               if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(R)
             }
      ) # end switch on objectives    
    }    
  }    
  return(momentargs)
}

#' Portfolio Moments
#' 
#' Set portfolio moments for use by lower level optimization functions. Currently
#' three methods for setting the moments are available
#' 
#' \itemize{
#'   \item{sample: }{sample estimates are used for the moments}
#'   \item{boudt: }{estimate the second, third, and fourth moments using a 
#'   statistical factor model based on the work of Kris Boudt.}
#'   See \code{\link{statistical.factor.model}}
#'   \item{black_litterman: }{estimate the first and second moments using the 
#'   Black Litterman Formula. See \code{\link{black.litterman}}}.
#' }
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization, see \code{\link{portfolio.spec}}
#' @param momentargs list containing arguments to be passed down to lower level functions, default NULL
#' @param method the method used to estimate portfolio moments. Valid choices include "sample", "boudt", and "black_litterman".
#' @param \dots any other passthru parameters
#' @aliases set.portfolio.moments set.portfolio.moments_v2
#' @rdname set.portfolio.moments
#' @export set.portfolio.moments
set.portfolio.moments <- set.portfolio.moments_v2 <- function(R, 
                                     portfolio, 
                                     momentargs=NULL, 
                                     method=c("sample", "boudt", "black_litterman", "meucci"), 
                                     ...){
  
  if(!hasArg(momentargs) | is.null(momentargs)) momentargs <- list()
  if(is.null(portfolio$objectives)) {
    warning("no objectives specified in portfolio")
  } else {
    method <- match.arg(method)
    
    # If any of the objectives have clean as an argument, we fit the factor
    # model and Black Litterman model with cleaned returns.
    clean <- unlist(lapply(portfolio$objectives, function(x) x$arguments$clean))
    if(!is.null(clean)){
      if(length(unique(clean)) > 1){
        warning(paste("Multiple methods detected for cleaning returns, default to use clean =", clean[1]))
      }
      cleanR <- Return.clean(R, method=clean[1])
      cleaned <- TRUE
    } else {
      cleaned <- FALSE
    }
    
    if(cleaned){
      tmpR <- cleanR
    } else {
      tmpR <- R
    }
    
    # Fit model based on method
    switch(method,
           boudt = {
             if(hasArg(k)) k=match.call(expand.dots=TRUE)$k else k=1
             fit <- statistical.factor.model(R=tmpR, k=k)
           },
           black_litterman = {
             if(hasArg(P)) P=match.call(expand.dots=TRUE)$P else P=matrix(rep(1, ncol(R)), nrow=1)
             if(hasArg(Mu)) Mu=match.call(expand.dots=TRUE)$Mu else Mu=NULL
             if(hasArg(Sigma)) Sigma=match.call(expand.dots=TRUE)$Sigma else Sigma=NULL
             if(hasArg(Views)) Views=match.call(expand.dots=TRUE)$Views else Views=NULL
             B <- black.litterman(R=tmpR, P=P, Mu=Mu, Sigma=Sigma, Views=Views)
           },
           meucci = {
             if(hasArg(posterior_p)) posterior_p=match.call(expand.dots=TRUE)$posterior_p else posterior_p=rep(1 / nrow(R), nrow(R))
             meucci.model <- meucci.moments(R=tmpR, posterior_p=posterior_p)
           }
    ) # end switch for fitting models based on method
    
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
    
    for (objective in portfolio$objectives){
      # The returns should already have been cleaned if any objective has
      # arguments=list(clean=*). One drawback is if different cleaning
      # methods are being used for different objectives, only the first 
      # method for cleaning is used. This is mor efficient and avoids "re"-cleaning.
      # Not sure that anyone would want to use different cleaning methods anyway. 
      # Another thing is that we don't recalculate the moments. So if a moment 
      # is set with un-cleaned returns then the next objective may have 
      # clean="boudt", but the cleaned returns are not used for that moment.
      # I think this is more consisent with how the objectives are specified
      # rather than overwriting all moments, but I am open to other ideas or
      # suggestions.
      if(!is.null(objective$arguments$clean)){
        tmpR <- cleanR
      } else {
        tmpR <- R
      }
      switch(objective$name,
             mean = {
               switch(method,
                      sample =,
                      boudt = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean', na.rm=TRUE)), ncol=1)
                      },
                      black_litterman = {
                        if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
                      },
                      meucci = {
                        if(is.null(momentargs$mu)) momentargs$mu = meucci.model$mu
                      }
               ) # end nested switch on method
             }, # end switch on mean
             var =,
             sd =,
             StdDev = {
               switch(method,
                      sample = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean', na.rm=TRUE)), ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(tmpR, use='pairwise.complete.obs')
                      },
                      boudt = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean', na.rm=TRUE)), ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
                      },
                      black_litterman = {
                        if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
                        if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
                      },
                      meucci = {
                        if(is.null(momentargs$mu)) momentargs$mu = meucci.model$mu
                        if(is.null(momentargs$sigma)) momentargs$sigma = meucci.model$sigma
                      }
               ) # end nested switch on method 
             }, # end switch on var, sd, StdDev
             mVaR =,
             VaR = {
               switch(method,
                      sample = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean')), ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = cov(tmpR)
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                      },
                      boudt = {
                        if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean')), ncol=1);
                        if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
                        if(is.null(momentargs$m3)) momentargs$m3 = extractCoskewness(fit)
                        if(is.null(momentargs$m4)) momentargs$m4 = extractCokurtosis(fit)
                      },
                      black_litterman = {
                        if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
                        if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                      },
                      meucci = {
                        if(is.null(momentargs$mu)) momentargs$mu = meucci.model$mu
                        if(is.null(momentargs$sigma)) momentargs$sigma = meucci.model$sigma
                        if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                        if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                      }
               ) # end nested switch on method
             }, # end switch on mVaR, VaR
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
                 switch(method,
                        sample = {
                          if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean')), ncol=1);
                          if(is.null(momentargs$sigma)) momentargs$sigma = cov(tmpR)
                          if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                          if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                        },
                        boudt = {
                          if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(tmpR, 2, 'mean')), ncol=1);
                          if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
                          if(is.null(momentargs$m3)) momentargs$m3 = extractCoskewness(fit)
                          if(is.null(momentargs$m4)) momentargs$m4 = extractCokurtosis(fit)
                        },
                        black_litterman = {
                          if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
                          if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
                          if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                          if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                        },
                        meucci = {
                          if(is.null(momentargs$mu)) momentargs$mu = meucci.model$mu
                          if(is.null(momentargs$sigma)) momentargs$sigma = meucci.model$sigma
                          if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(tmpR)
                          if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(tmpR)
                        }
                 ) # end nested switch on method
               }
             } # end switch on es, mES, CVaR, cVaR, ETL, mETL, ES
      ) # end switch on objectives    
    }    
  }
  return(momentargs)
}

garch.mm <- function(R,mu_ts, covlist,momentargs=list(),...) {
  #momentargs<-list()
  #momentargs$mu<-mu_ts[last(index(R)),]
  momentargs$mu<-mu_ts[last(index(R)),]
  
  momentargs$sigma<-covlist[as.character(last(index(R)))]
  if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(R)
  if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(R)
  return(momentargs)
}

#' Portfolio Moments
#' 
#' Set portfolio moments for use by lower level optimization functions using
#' a statistical factor model based on the work of Kris Boudt.
#' 
#' @note If any of the objectives in the \code{portfolio} object have 
#' \code{clean} as an argument, the cleaned returns are used to fit the model. 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' asset returns
#' @param portfolio an object of type \code{portfolio} specifying the 
#' constraints and objectives for the optimization, see 
#' \code{\link{portfolio.spec}}
#' @param momentargs list containing arguments to be passed down to lower level 
#' functions, default NULL
#' @param k number of factors used for fitting statistical factor model
#' @param \dots any other passthru parameters
portfolio.moments.boudt <- function(R, portfolio, momentargs=NULL, k=1, ...){
  
  # Fit the statistical factor model
  # If any of the objectives have clean as an argument, we fit the factor
  # model with cleaned returns. Is this the desired behavior we want?
  clean <- unlist(lapply(portfolio$objectives, function(x) x$arguments$clean))
  if(!is.null(clean)){
    if(length(unique(clean)) > 1){
      warning(paste("Multiple methods detected for cleaning returns, default to use clean =", clean[1]))
    }
    # This sets R as the cleaned returns for the rest of the function
    # This is proably fine since the only other place R is used is for the 
    # mu estimate
    R <- Return.clean(R, method=clean[1])
  }
  fit <- statistical.factor.model(R=R, k=k)
  
  if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
  if(is.null(portfolio$objectives)) {
    warning("no objectives specified in portfolio")
  } else {
    for (objective in portfolio$objectives){
      switch(objective$name,
             mean = {
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1)
             },
             var =,
             sd =,
             StdDev = { 
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean', na.rm=TRUE)),ncol=1)
               if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
             },
             mVaR =,
             VaR = {
               if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1)
               if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
               if(is.null(momentargs$m3)) momentargs$m3 = extractCoskewness(fit)
               if(is.null(momentargs$m4)) momentargs$m4 = extractCokurtosis(fit)
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
                 if(is.null(momentargs$mu)) momentargs$mu = matrix( as.vector(apply(R,2,'mean')),ncol=1)
                 if(is.null(momentargs$sigma)) momentargs$sigma = extractCovariance(fit)
                 if(is.null(momentargs$m3)) momentargs$m3 = extractCoskewness(fit)
                 if(is.null(momentargs$m4)) momentargs$m4 = extractCokurtosis(fit)
               }
             }
      ) # end switch on objectives    
    }    
  }    
  return(momentargs)
}

#' Portfolio Moments
#' 
#' Set portfolio moments for use by lower level optimization functions using
#' a basic Black Litterman model.
#' 
#' @note If any of the objectives in the \code{portfolio} object have 
#' \code{clean} as an argument, the cleaned returns are used to fit the model. 
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' asset returns
#' @param portfolio an object of type \code{portfolio} specifying the 
#' constraints and objectives for the optimization, see 
#' \code{\link{portfolio.spec}}
#' @param momentargs list containing arguments to be passed down to lower level 
#' functions, default NULL
#' @param P a K x N pick matrix representing views
#' @param Mu vector of length N of the prior expected values. The sample mean
#' is used if \code{Mu=NULL}.
#' @param Sigma an N x N matrix of the prior covariance matrix. The sample 
#' covariance is used if \code{Sigma=NULL}.
#' @param \dots any other passthru parameters
portfolio.moments.bl <- function(R, portfolio, momentargs=NULL, P, Mu=NULL, Sigma=NULL, ...){
  
  
  # If any of the objectives have clean as an argument, we fit the factor
  # model with cleaned returns. Is this the desired behavior we want?
  clean <- unlist(lapply(portfolio$objectives, function(x) x$arguments$clean))
  if(!is.null(clean)){
    if(length(unique(clean)) > 1){
      warning(paste("Multiple methods detected for cleaning returns, default to use clean =", clean[1]))
    }
    # This sets R as the cleaned returns for the rest of the function
    # This is probably fine since the only other place R is used is for the 
    # mu estimate
    R <- Return.clean(R, method=clean[1])
  }
  
  # Compute the Black Litterman estimates
  B <- black.litterman(R=R, P=P, Mu=Mu, Sigma=Sigma)
  
  if(!hasArg(momentargs) | is.null(momentargs)) momentargs<-list()
  if(is.null(portfolio$objectives)) {
    warning("no objectives specified in portfolio")
  } else {
    for (objective in portfolio$objectives){
      switch(objective$name,
             mean = {
               if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
             },
             var =,
             sd =,
             StdDev = { 
               if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
               if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
             },
             mVaR =,
             VaR = {
               if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
               if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
               if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(R)
               if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(R)
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
                 if(is.null(momentargs$mu)) momentargs$mu = B$BLMu
                 if(is.null(momentargs$sigma)) momentargs$sigma = B$BLSigma
                 if(is.null(momentargs$m3)) momentargs$m3 = PerformanceAnalytics::M3.MM(R)
                 if(is.null(momentargs$m4)) momentargs$m4 = PerformanceAnalytics::M4.MM(R)
               }
             }
      ) # end switch on objectives    
    }    
  }    
  return(momentargs)
}


###############################################################################
# $Id$
###############################################################################
