#' @title Compute moments using MCD robust method
#' 
#' @description 
#' This is a function that makes use of covMcd function from robustbase package. 
#' For more details of the parameters and function please refer to the manual of 
#' robustbase on cran.
#' 
#' @param R  xts object of asset returns
#' @param ... parameters for covMcd
#'
#' @return a list contains mean and covariance matrix of the stock return matrix
#' @export
#'
#' @examples
custom.covMcd <- function(R, ...){
  
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
#' @examples
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