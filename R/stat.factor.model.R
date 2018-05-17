###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2018 Brian G. Peterson, Peter Carl, Ross Bennett, Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: charts.DE.R 3378 2014-04-28 21:43:21Z rossbennett34 $
#
###############################################################################

# Note that many of these functions were provided by Kris Boudt and modified
# only slightly to work with this package

#' Statistical Factor Model
#' 
#' Fit a statistical factor model using Principal Component Analysis (PCA)
#' 
#' @details
#' The statistical factor model is fitted using \code{prcomp}. The factor
#' loadings, factor realizations, and residuals are computed and returned
#' given the number of factors used for the model.
#' 
#' @param R xts of asset returns
#' @param k number of factors to use
#' @param \dots additional arguments passed to \code{prcomp}
#' @return
#' #' \itemize{
#' \item{factor_loadings}{ N x k matrix of factor loadings (i.e. betas)}
#' \item{factor_realizations}{ m x k matrix of factor realizations}
#' \item{residuals}{ m x N matrix of model residuals representing idiosyncratic 
#' risk factors}
#' }
#' Where N is the number of assets, k is the number of factors, and m is the 
#' number of observations.
#' @export
statistical.factor.model <- function(R, k=1, ...){
  if(!is.xts(R)){
    R <- try(as.xts(R))
    if(inherits(R, "try-error")) stop("R must be an xts object or coercible to an xts object")
  }
  # dimensions of R
  m <- nrow(R)
  N <- ncol(R)
  
  # checks for R
  if(m < N) stop("fewer observations than assets")
  x <- coredata(R)
  
  # Make sure k is an integer
  if(k <= 0) stop("k must be a positive integer")
  k <- as.integer(k)
  
  # Fit a statistical factor model using Principal Component Analysis (PCA)
  fit <- prcomp(x, ...=...)
  
  # Extract the betas
  # (N x k)
  betas <- fit$rotation[, 1:k]
  
  # Compute the estimated factor realizations
  # (m x N) %*% (N x k) = (m x k)
  f <- x %*% betas
  
  # Compute the residuals
  # These can be computed manually or by fitting a linear model
  tmp <- x - f %*% t(betas)
  b0 <- colMeans(tmp)
  res <- tmp - matrix(rep(b0, m), ncol=N, byrow=TRUE)
  
  # Compute residuals via fitting a linear model
  # tmp.model <- lm(x ~ f)
  # tmp.beta <- coef(tmp.model)[2:(k+1),]
  # tmp.resid <- resid(tmp.model)
  # all.equal(t(tmp.beta), betas, check.attributes=FALSE)
  # all.equal(res, tmp.resid)
  
  # structure and return
  # stfm = *st*atistical *f*actor *m*odel
  structure(list(factor_loadings=betas,
                 factor_realizations=f,
                 residuals=res,
                 m=m,
                 k=k,
                 N=N),
            class="stfm")
}


#' Center
#' 
#' Center a matrix
#' 
#' This function is used primarily to center a time series of asset returns or 
#' factors. Each column should represent the returns of an asset or factor 
#' realizations. The expected value is taken as the sample mean.
#' 
#' x.centered = x - mean(x)
#' 
#' @param x matrix
#' @return matrix of centered data
#' @export
center <- function(x){
  if(!is.matrix(x)) stop("x must be a matrix")
  n <- nrow(x)
  p <- ncol(x)
  meanx <- colMeans(x)
  x.centered <- x - matrix(rep(meanx, n), ncol=p, byrow=TRUE)
  x.centered
}

##### Single Factor Model Comoments #####

#' Covariance Matrix Estimate
#' 
#' Estimate covariance matrix using a single factor statistical factor model
#' 
#' @details
#' This function estimates an (N x N) covariance matrix from a single factor 
#' statistical factor model with k=1 factors, where N is the number of assets. 
#' 
#' @param beta vector of length N or (N x 1) matrix of factor loadings 
#' (i.e. the betas) from a single factor statistical factor model
#' @param stockM2 vector of length N of the variance (2nd moment) of the 
#' model residuals (i.e. idiosyncratic variance of the stock)
#' @param factorM2 scalar value of the 2nd moment of the factor realizations 
#' from a single factor statistical factor model
#' @return (N x N) covariance matrix
covarianceSF <- function(beta, stockM2, factorM2){
  # Beta of the stock with the factor index
  beta = as.numeric(beta)
  
  N = length(beta)
  
  # Idiosyncratic variance of the stock
  stockM2 = as.numeric(stockM2)
  
  if(length(stockM2) != N) stop("dimensions do not match for beta and stockM2")
  
  # Variance of the factor
  factorM2 = as.numeric(factorM2)
  
  # Coerce beta to a matrix
  beta = matrix(beta, ncol = 1)
  
  # Compute estimate
  # S = (beta %*% t(beta)) * factorM2
  S = tcrossprod(beta) * factorM2
  D = diag(stockM2)
  return(S + D)
}

#' Coskewness Matrix Estimate
#' 
#' Estimate coskewness matrix using a single factor statistical factor model
#' 
#' @details
#' This function estimates an (N x N^2) coskewness matrix from a single factor 
#' statistical factor model with k=1 factors, where N is the number of assets. 
#' 
#' @param beta vector of length N or (N x 1) matrix of factor loadings 
#' (i.e. the betas) from a single factor statistical factor model
#' @param stockM3 vector of length N of the 3rd moment of the model residuals
#' @param factorM3 scalar of the 3rd moment of the factor realizations from a 
#' single factor statistical factor model
#' @return (N x N^2) coskewness matrix
coskewnessSF <- function(beta, stockM3, factorM3){
  # Beta of the stock with the factor index
  beta = as.numeric(beta)
  N = length(beta)
  
  # Idiosyncratic third moment of the stock
  stockM3 = as.numeric(stockM3) 
  
  if(length(stockM3) != N) stop("dimensions do not match for beta and stockM3")
  
  # Third moment of the factor
  factorM3 = as.numeric(factorM3)
  
  # Coerce beta to a matrix
  beta = matrix(beta, ncol = 1)
  
  # Compute estimate
  # S = ((beta %*% t(beta)) %x% t(beta)) * factorM3
  S = (tcrossprod(beta) %x% t(beta)) * factorM3
  D = matrix(0, nrow=N, ncol=N^2)
  for(i in 1:N){
    col = (i - 1) * N + i
    D[i, col] = stockM3[i]
  }
  return(S + D)
}

#' Cokurtosis Matrix Estimate
#' 
#' Estimate cokurtosis matrix using a single factor statistical factor model
#' 
#' @details
#' This function estimates an (N x N^3) cokurtosis matrix from a statistical 
#' factor model with k factors, where N is the number of assets. 
#' 
#' @param beta vector of length N or (N x 1) matrix of factor loadings 
#' (i.e. the betas) from a single factor statistical factor model
#' @param stockM2 vector of length N of the 2nd moment of the model residuals
#' @param stockM4 vector of length N of the 4th moment of the model residuals
#' @param factorM2 scalar of the 2nd moment of the factor realizations from a 
#' single factor statistical factor model
#' @param factorM4 scalar of the 4th moment of the factor realizations from a 
#' single factor statistical factor model
#' @return (N x N^3) cokurtosis matrix
cokurtosisSF <- function(beta, stockM2, stockM4, factorM2, factorM4){
  # Beta of the stock with the factor index
  beta = as.numeric(beta)
  N = as.integer(length(beta))
  
  # Idiosyncratic second moment of the stock
  stockM2 = as.numeric(stockM2)
  
  if(length(stockM2) != N) stop("dimensions do not match for beta and stockM2")
  
  # Idiosyncratic fourth moment of the stock 
  stockM4 = as.numeric(stockM4)
  
  if(length(stockM4) != N) stop("dimensions do not match for beta and stockM4")
  
  # Second moment of the factor
  factorM2 = as.numeric(factorM2)
  
  # Fourth moment of the factor
  factorM4 = as.numeric(factorM4)
  
  # Compute estimate
  # S = ((beta %*% t(beta)) %x% t(beta) %x% t(beta)) * factorM4
  S = (tcrossprod(beta) %x% t(beta) %x% t(beta)) * factorM4
  D = .residualcokurtosisSF(NN=N, sstockM2=stockM2, sstockM4=stockM4, mfactorM2=factorM2, bbeta=beta)
  return(S + D)
}

# Wrapper function to compute the residual cokurtosis matrix of a statistical
# factor model with k = 1.
# Note that this function was orignally written in C++ (using Rcpp) by
# Joshua Ulrich and re-written using the C API by Ross Bennett
#' @useDynLib "PortfolioAnalytics"
.residualcokurtosisSF <- function(NN, sstockM2, sstockM4, mfactorM2, bbeta){
  # NN        : integer
  # sstockM2  : vector of length NN
  # sstockM4  : vector of length NN
  # mfactorM2 : double
  # bbeta     : vector of length NN
  
  if(!is.integer(NN)) NN <- as.integer(NN)
  if(length(sstockM2) != NN) stop("sstockM2 must be a vector of length NN")
  if(length(sstockM4) != NN) stop("sstockM4 must be a vector of length NN")
  if(!is.double(mfactorM2)) mfactorM2 <- as.double(mfactorM2)
  if(length(bbeta) != NN) stop("bbeta must be a vector of length NN")
  
 .Call('residualcokurtosisSF', NN, sstockM2, sstockM4, mfactorM2, bbeta, PACKAGE="PortfolioAnalytics")
}

##### Multiple Factor Model Comoments #####

#' Covariance Matrix Estimate
#' 
#' Estimate covariance matrix using a statistical factor model
#' 
#' @details
#' This function estimates an (N x N) covariance matrix from a statistical 
#' factor model with k factors, where N is the number of assets. 
#' 
#' @param beta (N x k) matrix of factor loadings (i.e. the betas) from a 
#' statistical factor model
#' @param stockM2 vector of length N of the variance (2nd moment) of the 
#' model residuals (i.e. idiosyncratic variance of the stock)
#' @param factorM2 (k x k) matrix of the covariance (2nd moment) of the factor 
#' realizations from a statistical factor model
#' @return (N x N) covariance matrix
covarianceMF <- function(beta, stockM2, factorM2){
  # Formula for covariance matrix estimate
  # Sigma = beta %*% factorM2 %*% beta**T + Delta
  # Delta is a diagonal matrix with the 2nd moment of residuals on the diagonal
  
  # N = number of assets
  # k = number of factors
  
  # Use the dimensions of beta for checks of stockM2 and factorM2
  # beta should be an (N x k) matrix
  if(!is.matrix(beta)) stop("beta must be a matrix")
  N <- nrow(beta)
  k <- ncol(beta)
  
  # stockM2 should be a vector of length N
  stockM2 <- as.numeric(stockM2)
  if(length(stockM2) != N) stop("dimensions do not match for beta and stockM2")
  
  # factorM2 should be a (k x k) matrix
  if(!is.matrix(factorM2)) stop("factorM2 must be a matrix")
  if((nrow(factorM2) != k) | (ncol(factorM2) != k)){
    stop("dimensions do not match for beta and factorM2")
  }
  
  # Compute covariance matrix
  S <- beta %*% tcrossprod(factorM2, beta)
  D <- diag(stockM2)
  return(S + D)
}

#' Coskewness Matrix Estimate
#' 
#' Estimate coskewness matrix using a statistical factor model
#' 
#' @details
#' This function estimates an (N x N^2) coskewness matrix from a statistical 
#' factor model with k factors, where N is the number of assets. 
#' 
#' @param beta (N x k) matrix of factor loadings (i.e. the betas) from a 
#' statistical factor model
#' @param stockM3 vector of length N of the 3rd moment of the model residuals
#' @param factorM3 (k x k^2) matrix of the 3rd moment of the factor 
#' realizations from a statistical factor model
#' @return (N x N^2) coskewness matrix
coskewnessMF <- function(beta, stockM3, factorM3){
  # Formula for coskewness matrix estimate
  # Phi = beta %*% factorM3 %*% (beta**T %x% beta**T) + Omega
  # %x% is the kronecker product
  # Omega is the (N x N^2) matrix matrix of zeros except for the i,j elements 
  # where j = (i - 1) * N + i, which is corresponding to the expected third
  # moment of the idiosyncratic factors
  
  # N = number of assets
  # k = number of factors
  
  # Use the dimensions of beta for checks of stockM2 and factorM2
  # beta should be an (N x k) matrix
  if(!is.matrix(beta)) stop("beta must be a matrix")
  N <- nrow(beta)
  k <- ncol(beta)
  
  # stockM3 should be a vector of length N
  stockM3 <- as.numeric(stockM3)
  if(length(stockM3) != N) stop("dimensions do not match for beta and stockM3")
  
  # factorM3 should be an (k x k^2) matrix
  if(!is.matrix(factorM3)) stop("factorM3 must be a matrix")
  if((nrow(factorM3) != k) | (ncol(factorM3) != k^2)){
    stop("dimensions do not match for beta and factorM3")
  }
  
  # Compute coskewness matrix
  beta.t <- t(beta)
  S <- (beta %*% factorM3) %*% (beta.t %x% beta.t)
  D <- matrix(0, nrow=N, ncol=N^2)
  for(i in 1:N){
    col <- (i - 1) * N + i
    D[i, col] <- stockM3[i]
  }
  return(S + D)
}

#' Cokurtosis Matrix Estimate
#' 
#' Estimate cokurtosis matrix using a statistical factor model
#' 
#' @details
#' This function estimates an (N x N^3) cokurtosis matrix from a statistical 
#' factor model with k factors, where N is the number of assets. 
#' 
#' @param beta (N x k) matrix of factor loadings (i.e. the betas) from a 
#' statistical factor model
#' @param stockM2 vector of length N of the 2nd moment of the model residuals
#' @param stockM4 vector of length N of the 4th moment of the model residuals
#' @param factorM2 (k x k) matrix of the 2nd moment of the factor 
#' realizations from a statistical factor model
#' @param factorM4 (k x k^3) matrix of the 4th moment of the factor 
#' realizations from a statistical factor model
#' @return (N x N^3) cokurtosis matrix
cokurtosisMF <- function(beta , stockM2 , stockM4 , factorM2 , factorM4){
  
  # Formula for cokurtosis matrix estimate
  # Psi = beta %*% factorM4 %*% (beta**T %x% beta**T %x% beta**T) + Y
  # %x% is the kronecker product
  # Y is the residual matrix. 
  # see Asset allocation with higher order moments and factor models for
  # definition of Y
  
  # N = number of assets
  # k = number of factors
  
  # Use the dimensions of beta for checks of stockM2 and factorM2
  # beta should be an (N x k) matrix
  if(!is.matrix(beta)) stop("beta must be a matrix")
  N <- nrow(beta)
  k <- ncol(beta)
  
  # stockM2 should be a vector of length N
  stockM2 <- as.numeric(stockM2)
  if(length(stockM2) != N) stop("dimensions do not match for beta and stockM2")
  
  # stockM4 should be a vector of length N
  stockM4 <- as.numeric(stockM4)
  if(length(stockM4) != N) stop("dimensions do not match for beta and stockM4")
  
  # factorM2 should be a (k x k) matrix
  if(!is.matrix(factorM2)) stop("factorM2 must be a matrix")
  if((nrow(factorM2) != k) | (ncol(factorM2) != k)){
    stop("dimensions do not match for beta and factorM2")
  }
  
  # factorM4 should be a (k x k^3) matrix
  if(!is.matrix(factorM4)) stop("factorM4 must be a matrix")
  if((nrow(factorM4) != k) | (ncol(factorM4) != k^3)){
    stop("dimensions do not match for beta and factorM4")
  }
  
  # Compute cokurtosis matrix
  beta.t <- t(beta)
  S <- (beta %*% factorM4) %*% (beta.t %x% beta.t %x% beta.t)
  # betacov
  betacov <- as.numeric(beta %*% tcrossprod(factorM2, beta))
  # Compute the residual cokurtosis matrix
  D <- .residualcokurtosisMF(NN=N, sstockM2=stockM2, sstockM4=stockM4, bbetacov=betacov)
  return( S + D )
}

# Wrapper function to compute the residual cokurtosis matrix of a statistical
# factor model with k > 1.
# Note that this function was orignally written in C++ (using Rcpp) by
# Joshua Ulrich and re-written using the C API by Ross Bennett
#' @useDynLib "PortfolioAnalytics"
.residualcokurtosisMF <- function(NN, sstockM2, sstockM4, bbetacov){
  # NN        : integer, number of assets
  # sstockM2  : numeric vector of length NN
  # ssstockM4 : numeric vector of length NN
  # bbetacov  : numeric vector of length NN * NN
  
  if(!is.integer(NN)) NN <- as.integer(NN)
  if(length(sstockM2) != NN) stop("sstockM2 must be a vector of length NN")
  if(length(sstockM4) != NN) stop("sstockM4 must be a vector of length NN")
  if(length(bbetacov) != NN*NN) stop("bbetacov must be a vector of length NN*NN")
  
 .Call('residualcokurtosisMF', NN, sstockM2, sstockM4, bbetacov, PACKAGE="PortfolioAnalytics")
}

##### Extract Moments #####

#' Covariance Estimate
#' 
#' Extract the covariance matrix estimate from a statistical factor model
#' 
#' @param model statistical factor model estimated via 
#' \code{\link{statistical.factor.model}}
#' @param \dots not currently used
#' @return covariance matrix estimate
#' @seealso \code{\link{statistical.factor.model}}
#' @author Ross Bennett
#' @export
extractCovariance <- function(model, ...){
  if(!inherits(model, "stfm")) stop("model must be of class 'stfm'")
  
  # Extract elements from the model
  beta <- model$factor_loadings
  f <- model$factor_realizations
  res <- model$residuals
  m <- model$m
  k <- model$k
  N <- model$N
  
  # Residual moments
  denom <- m - k - 1
  stockM2 <- colSums(res^2) / denom
  
  # Factor moments
  factorM2 <- cov(f)
  
  # Compute covariance estimate
  if(k == 1){
    out <- covarianceSF(beta, stockM2, factorM2)
  } else if(k > 1){
    out <- covarianceMF(beta, stockM2, factorM2)
  } else {
    # invalid k
    message("invalid k, returning NULL")
    out <- NULL
  }
  return(out)
}

#' Coskewness Estimate
#' 
#' Extract the coskewness matrix estimate from a statistical factor model
#' 
#' @param model statistical factor model estimated via 
#' \code{\link{statistical.factor.model}}
#' @param \dots not currently used
#' @return coskewness matrix estimate
#' @seealso \code{\link{statistical.factor.model}}
#' @author Ross Bennett
#' @export
extractCoskewness <- function(model, ...){
  if(!inherits(model, "stfm")) stop("model must be of class 'stfm'")
  
  # Extract elements from the model
  beta <- model$factor_loadings
  f <- model$factor_realizations
  res <- model$residuals
  m <- model$m
  k <- model$k
  N <- model$N
  
  # Residual moments
  denom <- m - k - 1
  stockM3 <- colSums(res^3) / denom
  
  # Factor moments
  # f.centered <- center(f)
  # factorM3 <- M3.MM(f.centered)
  factorM3 <- PerformanceAnalytics::M3.MM(f)
  
  # Compute covariance estimate
  if(k == 1){
    # Single factor model
    out <- coskewnessSF(beta, stockM3, factorM3)
  } else if(k > 1){
    # Multi-factor model
    out <- coskewnessMF(beta, stockM3, factorM3)
  } else {
    # invalid k
    message("invalid k, returning NULL")
    out <- NULL
  }
  return(out)
}

#' Cokurtosis Estimate
#' 
#' Extract the cokurtosis matrix estimate from a statistical factor model
#' 
#' @param model statistical factor model estimated via 
#' \code{\link{statistical.factor.model}}
#' @param \dots not currently used
#' @return cokurtosis matrix estimate
#' @seealso \code{\link{statistical.factor.model}}
#' @author Ross Bennett
#' @export
extractCokurtosis <- function(model, ...){
  if(!inherits(model, "stfm")) stop("model must be of class 'stfm'")
  
  # Extract elements from the model
  beta <- model$factor_loadings
  f <- model$factor_realizations
  res <- model$residuals
  m <- model$m
  k <- model$k
  N <- model$N
  
  # Residual moments
  denom <- m - k - 1
  stockM2 <- colSums(res^2) / denom
  stockM4 <- colSums(res^4) / denom
  
  # Factor moments
  factorM2 <- cov(f)
  # f.centered <- center(f)
  # factorM4 <- M4.MM(f.centered)
  factorM4 <- PerformanceAnalytics::M4.MM(f)
  
  # Compute covariance estimate
  if(k == 1){
    # Single factor model
    out <- cokurtosisSF(beta, stockM2, stockM4, factorM2, factorM4)
  } else if(k > 1){
    # Multi-factor model
    out <- cokurtosisMF(beta, stockM2, stockM4, factorM2, factorM4)
  } else {
    # invalid k
    message("invalid k, returning NULL")
    out <- NULL
  }
  return(out)
}

