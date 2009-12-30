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


#' wrapper for constrained optimization of portfolios
#' 
#' This function aims to provide a wrapper for constrained optimization of 
#' portfolios that allows the user to specify box constraints and business 
#' objectives.
#' 
#' This function currently supports DEoptim and random portfolios as back ends.
#' Additional back end contributions for Rmetrics, ghyp, etc. would be welcome.
#'
#' TODO add multivariate moment calcs wherever possible for greater efficiency
#'   
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @callGraph 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ...)
{
  start_t<-Sys.time()

  #store the call for later
  call <- match.call()

  if (is.null(constraints) | !is.constraint(constraints)){
      stop("you must pass in an object of class constraints to control the optimization")
  }
  
  R <- checkData(R)
  N = length(constraints$assets)
  if (ncol(R)>N) {
      R=R[,names(constraints$assets)]
  }
  T = nrow(R)
  
  out=list()
  
  normalize_weights <- function(w){
      # normalize results if necessary
      if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
          # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
          # we'll normalize the weights passed in to whichever boundary condition has been violated
          # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
          # might violate your constraints, so you'd need to renormalize them after optimizing
          # we'll create functions for that so the user is less likely to mess it up.
          
          ##' NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
          ##' In Kris' original function, this was manifested as a full investment constraint
          if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
              max_sum=constraints$max_sum
              if(sum(w)>max_sum) { w<-(max_sum/sum(w))*w } # normalize to max_sum
          }
          
          if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
              min_sum=constraints$min_sum
              if(sum(w)<min_sum) { w<-(min_sum/sum(w))*w } # normalize to min_sum
          }
          
      } # end min_sum and max_sum normalization
      return(w)
  }
  if(optimize_method=="DEoptim"){
    stopifnot("package:DEoptim" %in% search() || require("DEoptim",quietly = TRUE))
    # DEoptim does 200 generations by default, so lets set the size of each generation to search_size/200)
    if(hasArg(itermax)) itermax=match.call(expand.dots=TRUE)$itermax else itermax=200
    NP = round(search_size/itermax)
    if(NP>2000) NP=2000
    if(!hasArg(controlDE)) controlDE = list( NP=NP, itermax=itermax, trace=trace, trystart=5 ) else controlDE=match.call(expand.dots=TRUE)$controlDE
    if(hasArg(VTR)) controlDE$VTR <- match.call(expand.dots=TRUE)$VTR #target number for the objective function
    if(hasArg(F))   controlDE$F  <- match.call(expand.dots=TRUE)$F   # stepsize, default .8
    if(hasArg(CR))  controlDE$CR <- match.call(expand.dots=TRUE)$CR 	 # Crossover probability from interval [0,1]. Default to '0.5'
    #if(hasArg(trace))  controlDE$trace <- match.call(expand.dots=TRUE)$trace      # trace
    if(hasArg(trace))  controlDE$trystart <- match.call(expand.dots=TRUE)$trystart      # how many times to try to generate an initial population
    if(!hasArg(mu))    mu = matrix( as.vector(apply(R,2,'mean')),ncol=1);
    if(!hasArg(sigma)) sigma = cov(R);
    if(!hasArg(M3))    M3 = PerformanceAnalytics:::M3.MM(R,mu)
    if(!hasArg(M4))    M4 = PerformanceAnalytics:::M4.MM(R,mu)

    # get upper and lower weights parameters from constraints
    upper = constraints$max
    lower = constraints$min

    minw = try(DEoptim( constrained_objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, ...=...)) # add ,silent=TRUE here?
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
        message(paste("Optimizer was unable to find a solution for target"))
        return (paste("Optimizer was unable to find a solution for target"))
    }

    w = as.vector( minw$optim$bestmem)
    w <- normalize_weights(w)
    names(w) = colnames(R)

    out = list(weights=w , objective_measures=constrained_objective(w=w,R=R,constraints,trace=TRUE)$objective_measures,call=call) 
    if(trace){out$DEoutput=minw}
    
  } ## end case for DEoptim
  if(optimize_method=="random"){
      #' call random_portfolios() with constraints and search_size to create matrix of portfolios
      rp<-random_portfolios(rpconstraints=constraints,permutations=search_size)
      #' store matrix in out if trace=TRUE
      if(trace) out$random_portfolios<-rp
      #' write foreach loop to call constrained_objective() with each portfolio
      if ("package:foreach" %in% search()){
          rp_objective_results<-foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective(w=rp[ii,],R,constraints,trace=trace,...=...)
      } else {
          rp_objective_results<-apply(rp, 1, constrained_objective, R=R, constraints=constraints, trace=trace, ...=...)
      }
      #' if trace=TRUE , store results of foreach in out$random_results
      if(trace) out$random_portfolio_objective_results<-rp_objective_results
      #' loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
      search<-vector(length=length(rp_objective_results))
      for (i in 1:length(search)) {
          if (trace) {
              search[i]<-ifelse(try(rp_objective_results[[i]]$out),rp_objective_results[[i]]$out,1e6)
              min_objective_weights<- normalize_weights(rp_objective_results[[which.min(search)]]$weights)
          } else {
              search[i]<-as.numeric(rp_objective_results[[i]])
              min_objective_weights<- normalize_weights(rp[which.min(search),])
          }
      }
      #' re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
      out$weights<-min_objective_weights
      out$constrained_objective<-constrained_objective(w=min_objective_weights,R=R,constraints,trace=TRUE)$objective_measures
      out$call<-call
      #' construct out list to be as similar as possible to DEoptim list, within reason
  }
    end_t<-Sys.time()
    # print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
    message(c("elapsed time:",end_t-start_t))
    out$constraints<-constraints
    out$data_summary<-list(first=first(R),last=last(R))
    out$elapsed_time<-end_t-start_t
    out$end_t<-as.character(Sys.time())
    return(out)
}

###############################################################################
# $Id$
###############################################################################