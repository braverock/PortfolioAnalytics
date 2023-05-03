#' @title Compute moments using covRob.MM methods
#'
#' @description 
#' This is a function that makes use of covRob from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRob.MM
#'
#' @return a list contains mean and covariance matrix of the data matrix
#' @author Yifu Kang
#' @export
#'
custom.covRob.MM <- function(R, ...){
  out <- list()
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=1e-4
  if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=50
  
  robustCov <- RobStatTM::covRob(X=R, tolpar=tol, maxit=maxit)
  
  out$sigma <- robustCov$cov
  out$mu <- robustCov$center
  return(out)
}

#' @title Compute moments using covRobRocke methods
#'
#' @description 
#' This is a function that makes use of covRobRocke from package RobStatTM to calculate
#' robust estimators of moments, including mean and variance.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRob.Rocke
#'
#' @return a list contains mean and covariance matrix of the data matrix
#' @author Yifu Kang
#' @export
#'
custom.covRob.Rocke <- function(R, ...){
  out <- list()
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=1e-4
  if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=50
  if(hasArg(initial)) initial=match.call(expand.dots=TRUE)$initial else initial='K'
  if(hasArg(maxsteps)) maxsteps=match.call(expand.dots=TRUE)$maxsteps else maxsteps=5
  if(hasArg(propmin)) propmin=match.call(expand.dots=TRUE)$propmin else propmin=2
  if(hasArg(qs)) qs=match.call(expand.dots=TRUE)$qs else qs=50
  
  robustCov <- RobStatTM::covRobRocke(X=R, initial=initial, maxsteps=maxsteps, propmin=propmin, 
                                      qs=qs, tol=tol, maxit=maxit)
  
  out$sigma <- robustCov$cov
  out$mu <- robustCov$center
  return(out)
}

#' @title Compute moments using covRob.MCD robust method
#' 
#' @description 
#' This is a function that makes use of covMcd function from robustbase package. 
#' For more details of the parameters and function please refer to the manual of 
#' robustbase on cran.
#' 
#' @param R  xts object of asset returns
#' @param ... parameters for covRob.Mcd
#'
#' @return a list contains mean and covariance matrix of the stock return matrix
#' @export
custom.covRob.Mcd <- function(R, ...){
  
  if(hasArg(control)) control=match.call(expand.dots=TRUE)$control else control=MycovMcd()
  if(hasArg(alpha)) alpha=match.call(expand.dots=TRUE)$alpha else alpha=control$alpha
  if(hasArg(nsamp)) nsamp=match.call(expand.dots=TRUE)$nsamp else nsamp=control$nsamp
  if(hasArg(nmini)) nmini=match.call(expand.dots=TRUE)$nmini else nmini=control$nmini
  if(hasArg(kmini)) kmini=match.call(expand.dots=TRUE)$kmini else kmini=control$kmini
  if(hasArg(scalefn)) scalefn=match.call(expand.dots=TRUE)$scalefn else scalefn=control$scalefn
  if(hasArg(maxcsteps)) maxcsteps=match.call(expand.dots=TRUE)$maxcsteps else maxcsteps=control$maxcsteps
  if(hasArg(initHsets)) initHsets=match.call(expand.dots=TRUE)$initHsets else initHsets=control$initHsets
  if(hasArg(seed)) seed=match.call(expand.dots=TRUE)$seed else seed=control$seed
  if(hasArg(tolSolve)) tolSolve=match.call(expand.dots=TRUE)$tolSolve else tolSolve=control$tolSolve
  if(hasArg(wgtFUN)) wgtFUN=match.call(expand.dots=TRUE)$wgtFUN else wgtFUN=control$wgtFUN
  if(hasArg(use.correction)) use.correction=match.call(expand.dots=TRUE)$use.correction else use.correction=control$use.correction
  
  
  robustMCD <- robustbase::covMcd(x=R, alpha=alpha, 
                                  nsamp=nsamp, nmini=nmini, 
                                  kmini=kmini, seed=seed,
                                  tolSolve=tolSolve, scalefn=scalefn, 
                                  maxcsteps=maxcsteps,
                                  initHsets=initHsets, 
                                  wgtFUN=wgtFUN, use.correction=use.correction)
  
  return(list(mu = robustMCD$center, sigma = robustMCD$cov))
}

#' @title 
#' Control settings for MCD robust moments
#' 
#' @description 
#' Auxiliary function for passing the estimation options as parameters 
#' to the estimation function MCD.robust.moment
#' 
#' @param alpha numeric parameter controlling the size of the subsets over 
#'              which the determinant is minimized. Allowed values are between 
#'              0.5 and 1 and the default is 0.5.
#' @param nsamp number of subsets used for initial estimates or "best", "exact", 
#'              or "deterministic". Default is nsamp = 500. For nsamp = "best" 
#'              exhaustive enumeration is done, as long as the number of trials 
#'              does not exceed 100'000, which is the value of nlarge. For "exact", 
#'              exhaustive enumeration will be attempted however many samples are needed.
#'              In this case a warning message may be displayed saying that 
#'              the computation can take a very long time.
#'              For "deterministic", the deterministic MCD is computed; 
#'              as proposed by Hubert et al. (2012) it starts from the h most 
#'              central observations of six (deterministic) estimators.
#' @param nmini,kmini for n >= 2*n0, n0 := nmini, the algorithm splits the data 
#'                    into maximally kmini (by default 5) subsets, of size approximately, 
#'                    but at least nmini. When nmini*kmini < n, the initial search 
#'                    uses only a subsample of size nmini*kmini. The original algorithm 
#'                    had nmini = 300 and kmini = 5 hard coded.
#' @param scalefn function to compute a robust scale estimate or character string 
#'                specifying a rule determining such a function for the deterministic MCD.
#'                The default is "hrv2012". Another option value is "v2014".
#' @param maxcsteps maximal number of concentration steps in the deterministic MCD
#' @param seed initial seed for random generator
#' @param tolSolve numeric tolerance to be used for inversion of the covariance matrix
#' @param wgtFUN a character string or function, specifying how the weights for 
#'               the reweighting step should be computed. Default is "01.originalz".
#' @param beta a quantile, experimentally used for some of the prespecified wgtFUNs. For our 
#'             MCD method, the default is 0.975.
#' @param use.correction whether to use finite sample correction factors; defaults to TRUE.
#' @return a list of passed parameters
#' @export
#'

MycovMcd <- function(alpha = 1/2,
                     nsamp = 500, nmini = 300, kmini = 5,
                     scalefn = "hrv2012", maxcsteps = 200,
                     seed = NULL, tolSolve = 1e-14,
                     wgtFUN = "01.original", beta,
                     use.correction=TRUE
){
  if(missing(beta) || !is.numeric(beta))
    beta <- 0.975
  
  return(list(alpha=alpha, nsamp=nsamp, nmini=as.integer(nmini), kmini=as.integer(kmini),
              seed = as.integer(seed),
              tolSolve=tolSolve, scalefn=scalefn, maxcsteps=as.integer(maxcsteps),
              wgtFUN=wgtFUN, beta=beta,
              use.correction=use.correction))
}

#' @title  Compute TSGS moments
#' 
#' @description 
#' This is a function making use of TSGS function from GSE package to compute
#' the Two-Step Generalized S-Estimate, a robust estimate of location 
#' and scatter for data with cell-wise and case-wise contamination.
#'
#' @param R xts object of asset returns
#' @param ... parameters for covRob.TSGS
#'
#' @return a list contains mean and covariance matrix of the stock return matrix
#' @export
#' 
#' @references Claudio Agostinelli, Andy Leung, "Robust estimation of multivariate 
#'             location and scatter in the presence of cellwise and casewise contamination",
#'             2014.

custom.covRob.TSGS <- function(R, ...){
  if(hasArg(control)) control=match.call(expand.dots=TRUE)$control else control=MyTSGS()
  if(hasArg(filter)) filter=match.call(expand.dots=TRUE)$filter else filter=control$filter
  if(hasArg(partial.impute)) partial.impute=match.call(expand.dots=TRUE)$partial.impute else partial.impute=control$partial.impute
  if(hasArg(tol)) tol=match.call(expand.dots=TRUE)$tol else tol=control$tol
  if(hasArg(maxiter)) maxiter=match.call(expand.dots=TRUE)$maxiter else maxiter=control$maxiter
  if(hasArg(loss)) loss=match.call(expand.dots=TRUE)$loss else loss=control$loss
  if(hasArg(init)) init=match.call(expand.dots=TRUE)$init else init=control$init
  
  tsgsRob <- GSE::TSGS(x=R, filter=filter,
                       partial.impute=partial.impute, tol=tol, 
                       maxiter=maxiter, method=loss,
                       init=init)
  
  return(list(mu = tsgsRob@mu, sigma = tsgsRob@S))
  
}

#' @title
#' Control settings for custom.TSGS
#'
#' @description 
#' Auxiliary function for passing the estimation options as parameters 
#' to the estimation function custom.TSGS
#'
#' @param filter the filter to be used in the first step. Available choices are 
#'               "UBF-DDC","UBF","DDC","UF". The default one is "UBF-DDC".
#' @param partial.impute whether partial imputation is used prior to estimation.
#'                       The default is FALSE.
#' @param tol tolerance for the convergence criterion. Default is 1e-4.
#' @param maxiter maximum number of iterations. Default is 150.
#' @param loss loss function to use, "bisquare" or "rocke". Default is "bisquare"
#' @param init type of initial estimator. Options include "emve", "qc", "huber","imputed","emve_c"
#'
#' @return a list of passed parameters
#' @export
#'

MyTSGS <- function(filter=c("UBF-DDC","UBF","DDC","UF"),
                   partial.impute=FALSE, tol=1e-4, maxiter=150, 
                   loss=c("bisquare","rocke"),
                   init=c("emve","qc","huber","imputed","emve_c")){
  
  filter <- match.arg(filter)
  loss <- match.arg(loss)
  init <- match.arg(init)
  
  return(list(filter=filter, partial.impute=partial.impute, 
              tol=tol, maxiter=as.integer(maxiter), 
              loss=loss,init))
}