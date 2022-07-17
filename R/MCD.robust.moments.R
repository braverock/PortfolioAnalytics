#' Title
#'
#' @param R 
#' @param cor 
#' @param raw.only 
#' @param alpha 
#' @param nsamp 
#' @param nmini 
#' @param kmini 
#' @param scalefn 
#' @param maxcsteps 
#' @param initHsets 
#' @param save.hsets 
#' @param names 
#' @param seed 
#' @param tolSolve 
#' @param trace 
#' @param use.correction 
#' @param wgtFUN 
#' @param control 
#'
#' @return
#' @export
#'
#' @examples
MCD.robust.moment <- function(R, alpha=0.75, nsamp=500, nmini=300, kmini=5,
                              scalefn='hrv2012', maxcsteps=NULL, initHsets = NULL, 
                              seed=NULL, tolSolve=NULL, 
                              wgtFUN="01.original", control=robustbase::rrcov.control()){
  
  robustMCD <- robustbase::covMcd(x=R,
         alpha=alpha, nsamp=nsamp, nmini=nmini, kmini=kmini,
         scalefn=scalefn, maxcsteps=maxcsteps,
         initHsets = NULL, seed=seed, tolSolve=tolSolve,
         wgtFUN=wgtFUN, control=control)
  
  return(list(rbMCD.Mu = robustMCD$center, rbMCD.Sig = robustMCD$cov))
}
