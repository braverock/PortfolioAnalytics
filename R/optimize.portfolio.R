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


#' @rdname optimize.portfolio
#' @name optimize.portfolio
#' @export optimize.portfolio_v1
optimize.portfolio_v1 <- function(
		R,
		constraints,
		optimize_method=c("DEoptim","random","ROI","ROI_old","pso","GenSA"), 
		search_size=20000, 
		trace=FALSE, ..., 
		rp=NULL,
		momentFUN='set.portfolio.moments_v1'
)
{
  optimize_method=optimize_method[1]
  tmptrace=NULL
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
  
  weights=NULL
    
  dotargs <-list(...)    
  
  # set portfolio moments only once
  if(!is.function(momentFUN)){
	  momentFUN<-match.fun(momentFUN)
  }	
  # TODO FIXME should match formals later
  #dotargs <- set.portfolio.moments(R, constraints, momentargs=dotargs)
  .mformals <- dotargs
  .mformals$R <- R
  .mformals$constraints <- constraints
  mout <- try((do.call(momentFUN,.mformals)) ,silent=TRUE)	
  if(inherits(mout,"try-error")) { 
	  message(paste("portfolio moment function failed with message",mout))
  } else {
	  dotargs <- c(dotargs,mout)
  }
	  
  normalize_weights <- function(weights){
      # normalize results if necessary
      if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
          # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
          # we'll normalize the weights passed in to whichever boundary condition has been violated
          # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
          # might violate your constraints, so you'd need to renormalize them after optimizing
          # we'll create functions for that so the user is less likely to mess it up.
          
          # NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
          # In Kris' original function, this was manifested as a full investment constraint
          if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
              max_sum=constraints$max_sum
              if(sum(weights)>max_sum) { weights<-(max_sum/sum(weights))*weights } # normalize to max_sum
          }
          
          if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
              min_sum=constraints$min_sum
              if(sum(weights)<min_sum) { weights<-(min_sum/sum(weights))*weights } # normalize to min_sum
          }
          
      } # end min_sum and max_sum normalization
      return(weights)
  }
  
  if(optimize_method=="DEoptim"){
    stopifnot("package:DEoptim" %in% search()  ||  requireNamespace("DEoptim",quietly = TRUE) )
    # DEoptim does 200 generations by default, so lets set the size of each generation to search_size/200)
    if(hasArg(itermax)) itermax=match.call(expand.dots=TRUE)$itermax else itermax=N*50
    NP = round(search_size/itermax)
    if(NP<(N*10)) NP <- N*10
    if(NP>2000) NP=2000
    if(!hasArg(itermax)) {
        itermax<-round(search_size/NP)
        if(itermax<50) itermax=50 #set minimum number of generations
    }
    
    #check to see whether we need to disable foreach for parallel optimization, esp if called from inside foreach
    if(hasArg(parallel)) parallel=match.call(expand.dots=TRUE)$parallel else parallel=TRUE
    if(!isTRUE(parallel) && 'package:foreach' %in% search()){
        foreach::registerDoSEQ()
    }
    
    DEcformals  <- formals(DEoptim::DEoptim.control)
    DEcargs <- names(DEcformals)
    if( is.list(dotargs) ){
        pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- DEcargs[pm]
        DEcformals$NP <- NP
        DEcformals$itermax <- itermax
        DEcformals[pm] <- dotargs[pm > 0L]
		if(!hasArg(strategy)) {
		  # use DE/current-to-p-best/1
		  strategy=6
      DEcformals$strategy=strategy
      }
		if(!hasArg(reltol)) {
		  # 1/1000 of 1% change in objective is significant
		  reltol=.000001
      DEcformals$reltol=reltol
      }
		if(!hasArg(steptol)) {
		  # number of assets times 1.5 tries to improve
		  steptol=round(N*1.5)
      DEcformals$steptol=steptol
      }
		if(!hasArg(c)) {
		  # JADE mutation parameter, this could maybe use some adjustment
		  tmp.c=.4
      DEcformals$c=tmp.c
      }
        if(!hasArg(storepopfrom)) {
          storepopfrom=1
          DEcformals$storepopfrom=storepopfrom
        }
        if(isTRUE(parallel) && 'package:foreach' %in% search()){
            if(!hasArg(parallelType)) {
              #use all cores
              parallelType=2
              DEcformals$parallelType=parallelType
              }
            if(!hasArg(packages)) {
              #use all packages
              packages <- names(sessionInfo()$otherPkgs)
              DEcformals$packages <- packages
              }
        }
		 
        #TODO FIXME also check for a passed in controlDE list, including checking its class, and match formals
    }
    
    if(isTRUE(trace)) { 
        #we can't pass trace=TRUE into constrained objective with DEoptim, because it expects a single numeric return
        tmptrace=trace 
        assign('.objectivestorage', list(), as.environment(.storage))
        trace=FALSE
    } 
    
    # get upper and lower weights parameters from constraints
    upper = constraints$max
    lower = constraints$min

    if(hasArg(rpseed)){ 
      seed <- match.call(expand.dots=TRUE)$rpseed
      DEcformals$initialpop <- seed
      rpseed <- FALSE
    } else {
      rpseed <- TRUE
    }
	if(hasArg(rpseed) & isTRUE(rpseed)) {
	    # initial seed population is generated with random_portfolios function
	    if(hasArg(eps)) eps=match.call(expand.dots=TRUE)$eps else eps = 0.01
    	rpconstraint<-constraint(assets=length(lower), min_sum=constraints$min_sum-eps, max_sum=constraints$max_sum+eps, 
             					min=lower, max=upper, weight_seq=generatesequence())
    	rp <- random_portfolios_v1(rpconstraints=rpconstraint,permutations=NP)
    	DEcformals$initialpop=rp
    }
    controlDE <- do.call(DEoptim::DEoptim.control,DEcformals)

    # minw = try(DEoptim( constrained_objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, ...=...)) # add ,silent=TRUE here?
    minw = try(DEoptim::DEoptim( constrained_objective_v1 ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, nargs = dotargs , ...=...)) # add ,silent=TRUE here?
 
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
        message(paste("Optimizer was unable to find a solution for target"))
        return (paste("Optimizer was unable to find a solution for target"))
    }
    
    if(isTRUE(tmptrace)) trace <- tmptrace
    
    weights = as.vector( minw$optim$bestmem)
    weights <- normalize_weights(weights)
    names(weights) = colnames(R)

    out = list(weights=weights, objective_measures=constrained_objective_v1(w=weights,R=R,constraints,trace=TRUE)$objective_measures,out=minw$optim$bestval, call=call)
    if (isTRUE(trace)){
        out$DEoutput=minw
        out$DEoptim_objective_results<-try(get('.objectivestorage',envir=.storage),silent=TRUE)
        rm('.objectivestorage',envir=.storage)
    }
    
  } ## end case for DEoptim
  
  
  if(optimize_method=="random"){
      # call random_portfolios() with constraints and search_size to create matrix of portfolios
      if(missing(rp) | is.null(rp)){
          rp<-random_portfolios_v1(rpconstraints=constraints,permutations=search_size)
      }
      # store matrix in out if trace=TRUE
      if (isTRUE(trace)) out$random_portfolios<-rp
      # write foreach loop to call constrained_objective() with each portfolio
      if ("package:foreach" %in% search() & !hasArg(parallel)){
          ii <- 1
          rp_objective_results<-foreach::foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective_v1(w=rp[ii,],R,constraints,trace=trace,...=dotargs)
      } else {
          rp_objective_results<-apply(rp, 1, constrained_objective_v1, R=R, constraints=constraints, trace=trace, ...=dotargs)
      }
      # if trace=TRUE , store results of foreach in out$random_results
      if(isTRUE(trace)) out$random_portfolio_objective_results<-rp_objective_results
      # loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
      search<-vector(length=length(rp_objective_results))
      # first we construct the vector of results
      for (i in 1:length(search)) {
          if (isTRUE(trace)) {
              search[i]<-ifelse(try(rp_objective_results[[i]]$out),rp_objective_results[[i]]$out,1e6)
          } else {
              search[i]<-as.numeric(rp_objective_results[[i]])
          }
      }
      # now find the weights that correspond to the minimum score from the constrained objective
      # and normalize_weights so that we meet our min_sum/max_sum constraints
      if (isTRUE(trace)) {
          min_objective_weights<- try(normalize_weights(rp_objective_results[[which.min(search)]]$weights))
      } else {
          min_objective_weights<- try(normalize_weights(rp[which.min(search),]))
      }
      # re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
      out$weights<-min_objective_weights
      out$objective_measures<-try(constrained_objective_v1(w=min_objective_weights,R=R,constraints,trace=TRUE)$objective_measures)
      out$call<-call
      # construct out list to be as similar as possible to DEoptim list, within reason

  } ## end case for random
  
  
  if(optimize_method == "ROI_old"){
    # This will take a new constraint object that is of the same structure of a 
    # ROI constraint object, but with an additional solver arg.
    # then we can do something like this
    print("ROI_old is going to be depricated.")
    roi.result <- ROI::ROI_solve(x=constraints$constrainted_objective, constraints$solver)
    weights <- roi.result$solution
    names(weights) <- colnames(R)
    out$weights <- weights
    out$objective_measures <- roi.result$objval
    out$call <- call
  } ## end case for ROI_old
  
  
  
  if(optimize_method == "ROI"){
    # This takes in a regular constraint object and extracts the desired business objectives
    # and converts them to matrix form to be inputed into a closed form solver
    # Applying box constraints
    bnds <- list(lower = list(ind = seq.int(1L, N), val = as.numeric(constraints$min)),
                 upper = list(ind = seq.int(1L, N), val = as.numeric(constraints$max)))
    # retrive the objectives to minimize, these should either be "var" and/or "mean"
    # we can eight miniminze variance or maximize quiadratic utility (we will be minimizing the neg. quad. utility)
    moments <- list(mean=rep(0, N))
    alpha <- 0.05
    target <- NA
    for(objective in constraints$objectives){
      if(objective$enabled){
        if(!any(c(objective$name == "mean", objective$name == "var", objective$name == "CVaR")))
          stop("ROI only solves mean, var, or sample CVaR type business objectives, choose a different optimize_method.")
        # I'm not sure what changed, but moments$mean used to be a vector of the column means
        # now it is a scalar value of the mean of the entire R object
        if(objective$name == "mean"){
          moments[[objective$name]] <- try(as.vector(apply(R, 2, "mean", na.rm=TRUE)), silent=TRUE)
        } else {
          moments[[objective$name]] <- try(eval(as.symbol(objective$name))(R), silent=TRUE)
        }
        target <- ifelse(!is.null(objective$target),objective$target, target)
        alpha <- ifelse(!is.null(objective$alpha), objective$alpha, alpha)
        lambda <- ifelse(!is.null(objective$risk_aversion), objective$risk_aversion, 1)
      }
    }
    plugin <- ifelse(any(names(moments)=="var"), "quadprog", "glpk")  
    if(plugin == "quadprog") ROI_objective <- ROI::Q_objective(Q=2*lambda*moments$var, L=-moments$mean)
    if(plugin == "glpk") ROI_objective <- ROI::L_objective(L=-moments$mean)
    Amat <- rbind(rep(1, N), rep(1, N))
    dir.vec <- c(">=","<=")
    rhs.vec <- c(constraints$min_sum, constraints$max_sum)
    if(!is.na(target)) {
      Amat <- rbind(Amat, moments$mean)
      dir.vec <- c(dir.vec, "==")
      rhs.vec <- c(rhs.vec, target)
    }
    if(try(!is.null(constraints$groups), silent=TRUE)){
      if(sum(constraints$groups) != N)
        stop("Number of assets in each group needs to sum to number of total assets.")
      n.groups <- length(constraints$groups)
      if(!all(c(length(constraints$cLO),length(constraints$cLO)) == n.groups) )
        stop("Number of group constraints exceeds number of groups.")
      Amat.group <- matrix(0, nrow=n.groups, ncol=N)
      k <- 1
      l <- 0
      for(i in 1:n.groups){
        j <- constraints$groups[i] 
        Amat.group[i, k:(l+j)] <- 1
        k <- l + j + 1
        l <- k - 1
      }
      if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
      if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
      Amat <- rbind(Amat, Amat.group, -Amat.group)
      dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
      rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
    }
    if(any(names(moments)=="CVaR")) {
      Rmin <- ifelse(is.na(target), 0, target)
      ROI_objective <- ROI::L_objective(c(rep(0,N), rep(1/(alpha*T),T), 1))
      Amat <- cbind(rbind(1, 1, moments$mean, coredata(R)), rbind(0, 0, 0, cbind(diag(T), 1))) 
      dir.vec <- c(">=","<=",">=",rep(">=",T))
      rhs.vec <- c(constraints$min_sum, constraints$max_sum, Rmin ,rep(0, T))
      if(try(!is.null(constraints$groups), silent=TRUE)){
        zeros <- matrix(0, nrow=n.groups, ncol=(T+1))
        Amat <- rbind(Amat, cbind(Amat.group, zeros), cbind(-Amat.group, zeros))
        dir.vec <- c(dir.vec, rep(">=", (n.groups + n.groups)))
        rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)
      }
    }
    opt.prob <- ROI::OP(objective=ROI_objective, 
                         constraints=ROI::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                         bounds=bnds)
    roi.result <- ROI::ROI_solve(x=opt.prob, solver=plugin)
    weights <- roi.result$solution[1:N]
    names(weights) <- colnames(R)
    out$weights <- weights
    out$out <- roi.result$objval
    out$call <- call
  } ## end case for ROI

  
  ## case if method=pso---particle swarm
  if(optimize_method=="pso"){
    stopifnot("package:pso" %in% search()  ||  requireNamespace("pso",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlPSO <- list(trace=FALSE, fnscale=1, maxit=1000, maxf=Inf, abstol=-Inf, reltol=0)
    PSOcargs <- names(controlPSO)
    
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), PSOcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- PSOcargs[pm]
      controlPSO$maxit <- maxit
      controlPSO[pm] <- dotargs[pm > 0L]
      if(!hasArg(reltol)) controlPSO$reltol <- .0001 # 1/100 of 1% change in objective is insignificant enough to restart a swarm
      #NOTE reltol has a different meaning for pso than it has for DEoptim.  for DEoptim, reltol is a stopping criteria, for pso, 
      #     it is a restart criteria.
        
      if(!hasArg(s)) {
        s <- N*10
        controlPSO$s<-s
        } #swarm size
      if(!hasArg(maxit.stagnate)) {
        #stopping criteria 
        maxit.stagnate <- controlPSO$s
        controlPSO$maxit.stagnate <- maxit.stagnate
        }     
      if(hasArg(trace) && try(trace==TRUE,silent=TRUE)) controlPSO$trace <- TRUE
      if(hasArg(trace) && isTRUE(trace)) {
          controlPSO$trace <- TRUE
          controlPSO$trace.stats=TRUE
      }
  }
    
    # get upper and lower weights parameters from constraints
    upper <- constraints$max
    lower <- constraints$min
    
    minw = try(pso::psoptim( par = rep(NA, N), fn = constrained_objective_v1 ,  R=R, constraints=constraints,
                        lower = lower[1:N] , upper = upper[1:N] , control = controlPSO)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector( minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    
    out = list(weights=weights, 
               objective_measures=constrained_objective_v1(w=weights,R=R,constraints,trace=TRUE)$objective_measures,
               out=minw$value, 
               call=call)
    if (isTRUE(trace)){
      out$PSOoutput=minw
    }
    
  } ## end case for pso
  
  
  ## case if method=GenSA---Generalized Simulated Annealing
  if(optimize_method=="GenSA"){
    stopifnot("package:GenSA" %in% search()  ||  requireNamespace("GenSA",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlGenSA <- list(maxit = 5000, threshold.stop = NULL, temperature = 5230, 
                          visiting.param = 2.62, acceptance.param = -5, max.time = NULL, 
                          nb.stop.improvement = 1e+06, smooth = TRUE, max.call = 1e+07, 
                          verbose = FALSE)
    GenSAcargs <- names(controlGenSA)
    
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), GenSAcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- GenSAcargs[pm]
      controlGenSA$maxit <- maxit
      controlGenSA[pm] <- dotargs[pm > 0L]
      if(hasArg(trace) && try(trace==TRUE,silent=TRUE)) controlGenSA$verbose <- TRUE
    }
    
    upper <- constraints$max
    lower <- constraints$min
    
    if(!is.null(rp)) par = rp[,1] else par = rep(1/N, N)
    
    minw = try(GenSA::GenSA( par=par, lower = lower[1:N] , upper = upper[1:N], control = controlGenSA, 
                      fn = constrained_objective_v1 ,  R=R, constraints=constraints)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector(minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    
    out = list(weights=weights, 
               objective_measures=constrained_objective_v1(w=weights,R=R,constraints,trace=TRUE)$objective_measures,
               out=minw$value, 
               call=call)
    if (isTRUE(trace)){
      out$GenSAoutput=minw
    }
    
  } ## end case for GenSA
  
  
    end_t<-Sys.time()
    # print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
    message(c("elapsed time:",end_t-start_t))
    out$constraints<-constraints
    out$data_summary<-list(first=first(R),last=last(R))
    out$elapsed_time<-end_t-start_t
    out$end_t<-as.character(Sys.time())
    class(out)<-c(paste("optimize.portfolio",optimize_method,sep='.'),"optimize.portfolio")
    return(out)
}

.onLoad <- function(lib, pkg) {
  if(!exists('.storage'))
    .storage <<- new.env()
}

#' Constrained optimization of portfolios
#' 
#' This function aims to provide a wrapper for constrained optimization of 
#' portfolios that specify constraints and objectives.
#' 
#' @details
#' This function currently supports DEoptim, random portfolios, pso, GenSA, and ROI as back ends.
#' Additional back end contributions for Rmetrics, ghyp, etc. would be welcome.
#'
#' When using random portfolios, search_size is precisely that, how many 
#' portfolios to test.  You need to make sure to set your feasible weights 
#' in generatesequence to make sure you have search_size unique 
#' portfolios to test, typically by manipulating the 'by' parameter 
#' to select something smaller than .01 
#' (I often use .002, as .001 seems like overkill)
#' 
#' When using DE, search_size is decomposed into two other parameters 
#' which it interacts with, NP and itermax.
#' 
#' NP, the number of members in each population, is set to cap at 2000 in 
#' DEoptim, and by default is the number of parameters (assets/weights) * 10.
#' 
#' itermax, if not passed in dots, defaults to the number of parameters (assets/weights) * 50.
#' 
#' When using GenSA and want to set \code{verbose=TRUE}, instead use \code{trace}. 
#' 
#' If \code{optimize_method="ROI"} is specified, a default solver will be 
#' selected based on the optimization problem. The \code{glpk} solver is the
#' default solver for LP and MILP optimization problems. The \code{quadprog} 
#' solver is the default solver for QP optimization problems. For example,
#' \code{optimize_method = "quadprog"} can be specified and the optimization
#' problem will be solved via ROI using the quadprog solver.
#' 
#' The extension to ROI solves a limited type of convex optimization problems:
#' \itemize{
#' \item{Maxmimize portfolio return subject leverage, box, group, position limit, target mean return, and/or factor exposure constraints on weights.}
#' \item{Minimize portfolio variance subject to leverage, box, group, turnover, and/or factor exposure constraints (otherwise known as global minimum variance portfolio).}
#' \item{Minimize portfolio variance subject to leverage, box, group, and/or factor exposure constraints and a desired portfolio return.}
#' \item{Maximize quadratic utility subject to leverage, box, group, target mean return, turnover, and/or factor exposure constraints and risk aversion parameter.
#' (The risk aversion parameter is passed into \code{optimize.portfolio} as an added argument to the \code{portfolio} object).}
#' \item{Maximize portfolio mean return per unit standard deviation (i.e. the Sharpe Ratio) can be done by specifying \code{maxSR=TRUE} in \code{optimize.portfolio}. 
#' If both mean and StdDev are specified as objective names, the default action is to maximize quadratic utility, therefore \code{maxSR=TRUE} must be specified to maximize Sharpe Ratio.}
#' \item{Minimize portfolio ES/ETL/CVaR optimization subject to leverage, box, group, position limit, target mean return, and/or factor exposure constraints and target portfolio return.}
#' \item{Maximize portfolio mean return per unit ES/ETL/CVaR (i.e. the STARR Ratio) can be done by specifying \code{maxSTARR=TRUE} in \code{optimize.portfolio}. 
#' If both mean and ES/ETL/CVaR are specified as objective names, the default action is to maximize mean return per unit ES/ETL/CVaR.}
#' }
#' These problems also support a weight_concentration objective where concentration
#' of weights as measured by HHI is added as a penalty term to the quadratic objective.
#' 
#' Because these convex optimization problem are standardized, there is no need for a penalty term. 
#' The \code{multiplier} argument in \code{\link{add.objective}} passed into the complete constraint object are ingnored by the ROI solver.
#'
#' @note
#' An object of class \code{v1_constraint} can be passed in for the \code{constraints} argument.
#' The \code{v1_constraint} object was used in the previous 'v1' specification to specify the 
#' constraints and objectives for the optimization problem, see \code{\link{constraint}}. 
#' We will attempt to detect if the object passed into the constraints argument 
#' is a \code{v1_constraint} object and update to the 'v2' specification by adding the 
#' constraints and objectives to the \code{portfolio} object.
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param constraints default=NULL, a list of constraint objects. An object of class 'v1_constraint' can be passed in here.
#' @param objectives default=NULL, a list of objective objects.
#' @param optimize_method one of "DEoptim", "random", "ROI", "pso", "GenSA". A solver
#' for ROI can also be specified and will be solved using ROI. See Details.
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param rp matrix of random portfolio weights, default NULL, mostly for automated use by rebalancing optimization or repeated tests on same portfolios
#' @param momentFUN the name of a function to call to set portfolio moments, default \code{\link{set.portfolio.moments_v2}}
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' 
#' @return a list containing the following elements
#' \itemize{
#'   \item{\code{weights}:}{ The optimal set weights.}
#'   \item{\code{objective_measures}:}{ A list containing the value of each objective corresponding to the optimal weights.}
#'   \item{\code{opt_values}:}{ A list containing the value of each objective corresponding to the optimal weights.}
#'   \item{\code{out}:}{ The output of the solver.}
#'   \item{\code{call}:}{ The function call.}
#'   \item{\code{portfolio}:}{ The portfolio object.}
#'   \item{\code{R}:}{ The asset returns.}
#'   \item{\code{data summary:}}{ The first row and last row of \code{R}.}
#'   \item{\code{elapsed_time:}}{ The amount of time that elapses while the optimization is run.}
#'   \item{\code{end_t:}}{ The date and time the optimization completed.}
#' }
#' When Trace=TRUE is specified, the following elements will be returned in 
#' addition to the elements above. The output depends on the optimization 
#' method and is specific to each solver. Refer to the documentation of the
#' desired solver for more information.
#' 
#' \code{optimize_method="random"}
#' \itemize{
#'   \item{\code{random_portfolios}:}{ A matrix of the random portfolios.}
#'   \item{\code{random_portfolio_objective_results}:}{ A list of the following elements for each random portfolio.}
#'   \itemize{
#'     \item{\code{out}:}{ The output value of the solver corresponding to the random portfolio weights.}
#'     \item{\code{weights}:}{ The weights of the random portfolio.}
#'     \item{\code{objective_measures}:}{ A list of each objective measure corresponding to the random portfolio weights.}
#'   }
#' }
#' 
#' \code{optimize_method="DEoptim"}
#' \itemize{
#'   \item{\code{DEoutput:}}{ A list (of length 2) containing the following elements:}
#'   \itemize{
#'     \item{\code{optim}}
#'     \item{\code{member}}
#'   }
#'   \item{\code{DEoptim_objective_results}:}{ A list containing the following elements for each intermediate population.}
#'   \itemize{
#'     \item{\code{out}:}{ The output of the solver.}
#'     \item{\code{weights}:}{ Population weights.}
#'     \item{\code{init_weights}:}{ Initial population weights.}
#'     \item{\code{objective_measures}:}{ A list of each objective measure corresponding to the weights}
#'   }
#' }
#' 
#' \code{optimize_method="pso"}
#' \itemize{
#'   \item{\code{PSOoutput}:}{ A list containing the following elements:}
#'   \itemize{
#'     \item{par}
#'     \item{value}
#'     \item{counts}
#'     \item{convergence}
#'     \item{message}
#'     \item{stats}
#'   }
#' }
#' 
#' \code{optimize_method="GenSA"}
#' \itemize{
#'   \item{\code{GenSAoutput:}}{ A list containing the following elements:}
#'   \itemize{
#'     \item{value}
#'     \item{par}
#'     \item{trace.mat}
#'     \item{counts}
#'   }
#' }
#' 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson, Ross Bennett
#' @aliases optimize.portfolio_v2 optimize.portfolio_v1
#' @seealso \code{\link{portfolio.spec}}
#' @rdname optimize.portfolio
#' @name optimize.portfolio
#' @export optimize.portfolio
#' @export optimize.portfolio_v2
optimize.portfolio <- optimize.portfolio_v2 <- function(
  R,
  portfolio=NULL,
  constraints=NULL,
  objectives=NULL,
  optimize_method=c("DEoptim","random","ROI","pso","GenSA"),
  search_size=20000,
  trace=FALSE, ...,
  rp=NULL,
  momentFUN='set.portfolio.moments',
  message=FALSE
)
{
  # This is the case where the user has passed in a list of portfolio objects
  # for the portfolio argument.
  # Loop through the portfolio list and recursively call optimize.portfolio
  # Note that I return at the end of this block. I know it is not good practice
  # to return before the end of a function, but I am not sure of another way
  # to handle a list of portfolio objects with the recursive call to 
  # optimize.portfolio. 
  if(inherits(portfolio, "portfolio.list")){
    n.portf <- length(portfolio)
    opt.list <- vector("list", n.portf)
    for(i in 1:length(opt.list)){
      if(message) cat("Starting optimization of portfolio ", i, "\n")
      opt.list[[i]] <- optimize.portfolio(R=R, 
                                          portfolio=portfolio[[i]],
                                          constraints=constraints, 
                                          objectives=objectives, 
                                          optimize_method=optimize_method, 
                                          search_size=search_size, 
                                          trace=trace, 
                                          ...=..., 
                                          rp=rp, 
                                          momentFUN=momentFUN, 
                                          message=message)
    }
    out <- combine.optimizations(opt.list)
    ##### return here for portfolio.list because this is a recursive call
    ##### for optimize.portfolio
    return(out)
  }
  
  # Detect regime switching portfolio
  if(inherits(portfolio, "regime.portfolios")){
    regime.switching <- TRUE
    regime <- portfolio$regime
    if(index(last(R)) %in% index(regime)){
      regime.idx <- as.numeric(regime[index(last(R))])[1]
      portfolio <- portfolio$portfolio.list[[regime.idx]]
      #cat("regime: ", regime.idx, "\n")
    } else {
      warning("Dates in regime and R do not match, defaulting to first portfolio")
      regime.idx <- 1
      portfolio <- portfolio$portfolio.list[[regime.idx]]
    }
  } else {
    regime.switching <- FALSE
  }
  
  # This is the case where the user has passed in a mult.portfolio.spec
  # object for multiple layer portfolio optimization.
  if(inherits(portfolio, "mult.portfolio.spec")){
    # This function calls optimize.portfolio.rebalancing on each sub portfolio
    # according to the given optimization parameters and returns an xts object
    # representing the proxy returns of each sub portfolio.
    R <- proxy.mult.portfolio(R=R, mult.portfolio=portfolio)
    
    # The optimization is controlled by the constraints and objectives in the
    # top level portfolio so now set the 'portfolio' to the top level portfolio
    portfolio <- portfolio$top.portfolio
  }
  
  optimize_method <- optimize_method[1]
  tmptrace <- NULL
  start_t <- Sys.time()
  
  #store the call for later
  call <- match.call()
  
  if (!is.null(portfolio) & !is.portfolio(portfolio)){
    stop("you must pass in an object of class 'portfolio' to control the optimization")
  }
  
  # Check for constraints and objectives passed in separately outside of the portfolio object
  if(!is.null(constraints)){
    if(inherits(constraints, "v1_constraint")){
      if(is.null(portfolio)){
        # If the user has not passed in a portfolio, we will create one for them
        tmp_portf <- portfolio.spec(assets=constraints$assets)
      }
      message("constraint object passed in is a 'v1_constraint' object, updating to v2 specification")
      portfolio <- update_constraint_v1tov2(portfolio=tmp_portf, v1_constraint=constraints)
      # print.default(portfolio)
    }
    if(!inherits(constraints, "v1_constraint")){
      # Insert the constraints into the portfolio object
      portfolio <- insert_constraints(portfolio=portfolio, constraints=constraints)
    }
  }
  if(!is.null(objectives)){
    # Insert the objectives into the portfolio object
    portfolio <- insert_objectives(portfolio=portfolio, objectives=objectives)
  }
  
  R <- checkData(R)
  N <- length(portfolio$assets)
  if (ncol(R) > N) {
    R <- R[,names(portfolio$assets)]
  }
  T <- nrow(R)
  
  # Initialize an empty list used as the return object
  out <- list()
  
  weights <- NULL 
  
  # Get the constraints from the portfolio object
  constraints <- get_constraints(portfolio)
  
  # set portfolio moments only once
  # For set.portfolio.moments, we are passing the returns,
  # portfolio object, and dotargs. dotargs is a list of arguments
  # that are passed in as dots in optimize.portfolio. This was
  # causing errors if clean="boudt" was specified in an objective
  # and an argument such as itermax was passed in as dots to 
  # optimize.portfolio. See r2931
  if(!is.function(momentFUN)){
    momentFUN <- match.fun(momentFUN)
  }
  
  # **
  # When an ES/ETL/CVaR problem is being solved by a linear solver, the higher
  # moments do not need to be calculated. The moments are very compute 
  # intensive and slow down the optimization problem.
  
  # match the args for momentFUN
  .formals <- formals(momentFUN)
  .formals <- modify.args(formals=.formals, arglist=list(...), dots=TRUE)
  # ** pass ROI=TRUE to set.portfolio.moments so the moments are not calculated
  if(optimize_method %in% c("ROI", "quadprog", "glpk", "symphony", "ipop")){
    obj_names <- unlist(lapply(portfolio$objectives, function(x) x$name))
    if(any(obj_names %in% c("CVaR", "ES", "ETL"))){
      .formals <- modify.args(formals=.formals, arglist=list(ROI=TRUE), dots=TRUE)
    }
  }
  if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=R, dots=FALSE)
  if("portfolio" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, portfolio=portfolio, dots=FALSE)
  .formals$... <- NULL
  
  # call momentFUN
  mout <- try(do.call(momentFUN, .formals), silent=TRUE)
  
  if(inherits(mout, "try-error")) { 
    message(paste("portfolio moment function failed with message", mout))
  } else {
    #.args_env <- as.environment(mout)
    #.args_env <- new.env()
    # Assign each element of mout to the .args_env environment
    #for(name in names(mout)){
    #  .args_env[[name]] <- mout[[name]]
    #}
    dotargs <- mout
  }
  
  # Function to normalize weights to min_sum and max_sum
  # This function could be replaced by rp_transform
  normalize_weights <- function(weights){
    # normalize results if necessary
    if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
      # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
      # we'll normalize the weights passed in to whichever boundary condition has been violated
      # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
      # might violate your constraints, so you'd need to renormalize them after optimizing
      # we'll create functions for that so the user is less likely to mess it up.
      
      # NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
      # In Kris' original function, this was manifested as a full investment constraint
      if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
        max_sum=constraints$max_sum
        if(sum(weights)>max_sum) { weights<-(max_sum/sum(weights))*weights } # normalize to max_sum
      }
      
      if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
        min_sum=constraints$min_sum
        if(sum(weights)<min_sum) { weights<-(min_sum/sum(weights))*weights } # normalize to min_sum
      }
      
    } # end min_sum and max_sum normalization
    return(weights)
  }
  
  # DEoptim optimization method
  if(optimize_method == "DEoptim"){
    stopifnot("package:DEoptim" %in% search()  ||  requireNamespace("DEoptim",quietly = TRUE))
    # DEoptim does 200 generations by default, so lets set the size of each generation to search_size/200)
    if(hasArg(itermax)) itermax=match.call(expand.dots=TRUE)$itermax else itermax=N*50
    NP <- round(search_size/itermax)
    if(NP < (N * 10)) NP <- N * 10
    if(NP > 2000) NP <- 2000
    if(!hasArg(itermax)) {
      itermax <- round(search_size / NP)
      if(itermax < 50) itermax <- 50 #set minimum number of generations
    }
    
    #check to see whether we need to disable foreach for parallel optimization, esp if called from inside foreach
    if(hasArg(parallel)) parallel <- match.call(expand.dots=TRUE)$parallel else parallel <- TRUE
    if(!isTRUE(parallel) && 'package:foreach' %in% search()){
      foreach::registerDoSEQ()
    }
    
    DEcformals <- formals(DEoptim::DEoptim.control)
    DEcargs <- names(DEcformals)
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- DEcargs[pm]
      DEcformals$NP <- NP
      DEcformals$itermax <- itermax
      DEcformals[pm] <- dotargs[pm > 0L]
      if(!hasArg(strategy)) {
        # use DE/current-to-p-best/1
        strategy=6
        DEcformals$strategy=strategy
        }
      if(!hasArg(reltol)) {
        # 1/1000 of 1% change in objective is significant
        reltol=0.000001
        DEcformals$reltol=reltol
        } 
      if(!hasArg(steptol)) {
        # number of assets times 1.5 tries to improve
        steptol=round(N*1.5)
        DEcformals$steptol=steptol
        } 
      if(!hasArg(c)) {
        # JADE mutation parameter, this could maybe use some adjustment
        tmp.c=0.4
        DEcformals$c=tmp.c
        }
      if(!hasArg(storepopfrom)) {
        storepopfrom=1
        DEcformals$storepopfrom=storepopfrom
      }
      if(isTRUE(parallel) && 'package:foreach' %in% search()){
        if(!hasArg(parallelType)) {
          #use all cores
          parallelType=2
          DEcformals$parallelType=parallelType
          }
        if(!hasArg(packages)) {
          #use all packages
          packages <- names(sessionInfo()$otherPkgs)
          DEcformals$packages <- packages
          }
      }
      #TODO FIXME also check for a passed in controlDE list, including checking its class, and match formals
    }
    if(hasArg(traceDE)) traceDE=match.call(expand.dots=TRUE)$traceDE else traceDE=TRUE
    DEcformals$trace <- traceDE
    if(isTRUE(trace)) { 
      #we can't pass trace=TRUE into constrained objective with DEoptim, because it expects a single numeric return
      tmptrace <- trace 
      assign('.objectivestorage', list(), envir=as.environment(.storage))
      trace=FALSE
    } 
    
    # get upper and lower weights parameters from constraints
    upper <- constraints$max
    lower <- constraints$min
    
    # issue message if min_sum and max_sum are restrictive
    if((constraints$max_sum - constraints$min_sum) < 0.02){
      message("Leverage constraint min_sum and max_sum are restrictive, 
              consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
    }
    
    #if(hasArg(rpseed)){ 
    #  seed <- match.call(expand.dots=TRUE)$rpseed
    #  DEcformals$initialpop <- seed
    #  rpseed <- FALSE
    #} else {
    #  rpseed <- TRUE
    #}
    #if(hasArg(rpseed) & isTRUE(rpseed)) {
    #  # initial seed population is generated with random_portfolios function
    #  # if(hasArg(eps)) eps=match.call(expand.dots=TRUE)$eps else eps = 0.01
    #  if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
    #  if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
    #  if(hasArg(fev)) fev=match.call(expand.dots=TRUE)$fev else fev=0:5
    #  rp <- random_portfolios(portfolio=portfolio, permutations=NP, rp_method=rp_method, eliminate=eliminate, fev=fev)
    #  DEcformals$initialpop <- rp
    #}
    
    # Use rp as the initial population or generate from random portfolios
    if(!is.null(rp)){
      rp_len <- min(nrow(rp), NP)
      seed <- rp[1:rp_len,]
      DEcformals$initialpop <- seed
    } else{
      # Initial seed population is generated with random_portfolios function if rp is not passed in
      if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
      # if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
      if(hasArg(fev)) fev=match.call(expand.dots=TRUE)$fev else fev=0:5
      rp <- random_portfolios(portfolio=portfolio, permutations=(NP+1), rp_method=rp_method, eliminate=FALSE, fev=fev)
      DEcformals$initialpop <- rp
    }
    
    controlDE <- do.call(DEoptim::DEoptim.control, DEcformals)
    # We are passing fn_map to the optional fnMap function to do the 
    # transformation so we need to force normalize=FALSE in call to constrained_objective
    minw = try(DEoptim::DEoptim( constrained_objective,  lower=lower[1:N], upper=upper[1:N], control=controlDE, R=R, portfolio=portfolio, env=dotargs, normalize=FALSE, fnMap=function(x) fn_map(x, portfolio=portfolio)$weights), silent=TRUE)
    
    if(inherits(minw, "try-error")) { 
      message(minw)
      minw=NULL
    }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    if(isTRUE(tmptrace)) trace <- tmptrace
    
    weights <- as.vector(minw$optim$bestmem)
    print(weights)
    # is it necessary to normalize the weights here?
    # weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE, env=dotargs)$objective_measures
    out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=minw$optim$bestval, call=call)
    if (isTRUE(trace)){
      out$DEoutput <- minw
      out$DEoptim_objective_results <- try(get('.objectivestorage',envir=.storage),silent=TRUE)
      rm('.objectivestorage',envir=.storage)
      #out$DEoptim_objective_results <- try(get('.objectivestorage',pos='.GlobalEnv'),silent=TRUE)
      #rm('.objectivestorage',pos='.GlobalEnv')
    }
    
  } ## end case for DEoptim
  
  # case for random portfolios optimization method
  if(optimize_method=="random"){
    # issue message if min_sum and max_sum are too restrictive
    if((constraints$max_sum - constraints$min_sum) < 0.02){
      message("Leverage constraint min_sum and max_sum are restrictive, 
              consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01")
    }
    
    # call random_portfolios() with portfolio and search_size to create matrix of portfolios
    if(missing(rp) | is.null(rp)){
      if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
      if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
      if(hasArg(fev)) fev=match.call(expand.dots=TRUE)$fev else fev=0:5
      rp <- random_portfolios(portfolio=portfolio, permutations=search_size, rp_method=rp_method, eliminate=eliminate, fev=fev)
    }
    # store matrix in out if trace=TRUE
    if (isTRUE(trace)) out$random_portfolios <- rp
    # rp is already being generated with a call to fn_map so set normalize=FALSE in the call to constrained_objective
    # write foreach loop to call constrained_objective() with each portfolio
    if ("package:foreach" %in% search() & !hasArg(parallel)){
      ii <- 1
      rp_objective_results <- foreach::foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective(w=rp[ii,], R=R, portfolio=portfolio, trace=trace, env=dotargs, normalize=FALSE)
    } else {
      rp_objective_results <- apply(rp, 1, constrained_objective, R=R, portfolio=portfolio, trace=trace, normalize=FALSE, env=dotargs)
    }
    # if trace=TRUE , store results of foreach in out$random_results
    if(isTRUE(trace)) out$random_portfolio_objective_results <- rp_objective_results
    # loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
    search <- vector(length=length(rp_objective_results))
    # first we construct the vector of results
    for (i in 1:length(search)) {
      if (isTRUE(trace)) {
        search[i] <- ifelse(try(rp_objective_results[[i]]$out), rp_objective_results[[i]]$out,1e6)
      } else {
        search[i] <- as.numeric(rp_objective_results[[i]])
      }
    }
    # now find the weights that correspond to the minimum score from the constrained objective
    # and normalize_weights so that we meet our min_sum/max_sum constraints
    # Is it necessary to normalize the weights at all with random portfolios?
    if (isTRUE(trace)) {
      min_objective_weights <- try(normalize_weights(rp_objective_results[[which.min(search)]]$weights))
    } else {
      min_objective_weights <- try(normalize_weights(rp[which.min(search),]))
    }
    # re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
    out$weights <- min_objective_weights
    obj_vals <- try(constrained_objective(w=min_objective_weights, R=R, portfolio=portfolio, trace=TRUE, normalize=FALSE, env=dotargs)$objective_measures)
    out$objective_measures <- obj_vals
    out$opt_values <- obj_vals
    out$call <- call
    # construct out list to be as similar as possible to DEoptim list, within reason
    
  } ## end case for random
  
  roi_solvers <- c("ROI", "quadprog", "glpk", "symphony", "ipop")
  if(optimize_method %in% roi_solvers){
    # check for a control argument for list of solver control arguments
    if(hasArg(control)) control=match.call(expand.dots=TRUE)$control else control=NULL
    
    # This takes in a regular portfolio object and extracts the constraints and
    # objectives and converts them for input. to a closed form solver using
    # ROI as an interface.
    moments <- list(mean=rep(0, N))
    alpha <- 0.05
    if(!is.null(constraints$return_target)){
      target <- constraints$return_target
    } else {
      target <- NA
    }
    lambda <- 1
    
    # list of valid objective names for ROI solvers
    valid_objnames <- c("HHI", "mean", "var", "sd", "StdDev", "CVaR", "ES", "ETL")
    #objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
    for(objective in portfolio$objectives){
      if(objective$enabled){
        if(!(objective$name %in% valid_objnames)){
          stop("ROI only solves mean, var/StdDev, HHI, or sample ETL/ES/CVaR type business objectives, choose a different optimize_method.")
        }
        
        # Grab the arguments list per objective
        # Currently we are only getting arguments for "p" and "clean", not sure if we need others for the ROI QP/LP solvers
        # if(length(objective$arguments) >= 1) arguments <- objective$arguments else arguments <- list()
        arguments <- objective$arguments
        if(!is.null(arguments$clean)) clean <- arguments$clean else clean <- "none"
        # Note: arguments$p grabs arguments$portfolio_method if no p is specified
        # so we need to be explicit with arguments[["p"]]
        if(!is.null(arguments[["p"]])) alpha <- arguments$p else alpha <- alpha
        if(alpha > 0.5) alpha <- (1 - alpha)
        
        # Some of the sub-functions for optimizations use the returns object as
        # part of the constraints matrix (e.g. etl_opt and etl_milp_opt) so we
        # will store the cleaned returns in the moments object. This may not
        # be the most efficient way to pass around a cleaned returns object, 
        # but it will keep it separate from the R object passed in by the user
        # and avoid "re-cleaning" already cleaned returns if specified in 
        # multiple objectives.
        if(clean != "none") moments$cleanR <- Return.clean(R=R, method=clean)
        
        # Use $mu and $sigma estimates from momentFUN if available, fall back to
        # calculating sample mean and variance
        if(objective$name == "mean"){
          if(!is.null(mout$mu)){
            moments[["mean"]] <- as.vector(mout$mu)
          } else {
            moments[["mean"]] <- try(as.vector(apply(Return.clean(R=R, method=clean), 2, "mean", na.rm=TRUE)), silent=TRUE)
          }
        } else if(objective$name %in% c("StdDev", "sd", "var")){
          if(!is.null(mout$sigma)){
            moments[["var"]] <- mout$sigma
          } else {
            moments[["var"]] <- try(var(x=Return.clean(R=R, method=clean), na.rm=TRUE), silent=TRUE)
          }
        } else if(objective$name %in% c("CVaR", "ES", "ETL")){
          # do nothing (no point in computing ES here)
          moments[[objective$name]] <- ""
        } else {
          # I'm not sure this is serving any purpose since we control the types
          # of problems solved with ROI. The min ES problem only uses 
          # moments$mu if a target return is specified.
          moments[[objective$name]] <- try(eval(as.symbol(objective$name))(Return.clean(R=R, method=clean)), silent=TRUE)
        }
        target <- ifelse(!is.null(objective$target), objective$target, target)
        # alpha <- ifelse(!is.null(objective$alpha), objective$alpha, alpha)
        # only accept confidence level for ES/ETL/CVaR to come from the 
        # arguments list to be consistent with how this is done in other solvers.
        lambda <- ifelse(!is.null(objective$risk_aversion), objective$risk_aversion, lambda)
        if(!is.null(objective$conc_aversion)) lambda_hhi <- objective$conc_aversion else lambda_hhi <- NULL
        if(!is.null(objective$conc_groups)) conc_groups <- objective$conc_groups else conc_groups <- NULL
      }
    }
    
    if("var" %in% names(moments)){
      # Set a default solver if optimize_method == "ROI", otherwise assume the
      # optimize_method specified by the user is the solver for ROI
      if(optimize_method == "ROI"){
        solver <- "quadprog"
      } else {
        solver <- optimize_method
      }
      # Minimize variance if the only objective specified is variance
      # Maximize Quadratic Utility if var and mean are specified as objectives
      if(!is.null(constraints$turnover_target) | !is.null(constraints$ptc) | !is.null(constraints$leverage)){
        if(!is.null(constraints$turnover_target) & !is.null(constraints$ptc)){
          warning("Turnover and proportional transaction cost constraints detected, only running optimization for turnover constraint.")
          constraints$ptc <- NULL
        }
        # turnover constraint
        if(!is.null(constraints$turnover_target) & is.null(constraints$ptc)){
          qp_result <- gmv_opt_toc(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, init_weights=portfolio$assets, solver=solver, control=control)
          weights <- qp_result$weights
          # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
          obj_vals <- qp_result$obj_vals
          out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=qp_result$out, call=call)
        }
        # proportional transaction costs constraint
        if(!is.null(constraints$ptc) & is.null(constraints$turnover_target)){
          qp_result <- gmv_opt_ptc(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, init_weights=portfolio$assets, solver=solver, control=control)
          weights <- qp_result$weights
          # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
          obj_vals <- qp_result$obj_vals
          out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=qp_result$out, call=call)
        }
        # leverage constraint
        if(!is.null(constraints$leverage)){
          qp_result <- gmv_opt_leverage(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, solver=solver, control=control)
          weights <- qp_result$weights
          # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
          obj_vals <- qp_result$obj_vals
          out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=qp_result$out, call=call)
        }
      } else {
        # if(hasArg(ef)) ef=match.call(expand.dots=TRUE)$ef else ef=FALSE
        if(hasArg(maxSR)) maxSR=match.call(expand.dots=TRUE)$maxSR else maxSR=FALSE
        if(maxSR){
          target <- max_sr_opt(R=R, constraints=constraints, moments=moments, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver, control=control)
          # need to set moments$mean=0 here because quadratic utility and target return is sensitive to returning no solution
          tmp_moments_mean <- moments$mean
          moments$mean <- rep(0, length(moments$mean))
        }
        roi_result <- gmv_opt(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, lambda_hhi=lambda_hhi, conc_groups=conc_groups, solver=solver, control=control)
        weights <- roi_result$weights
        # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        obj_vals <- roi_result$obj_vals
        if(maxSR){
          # need to recalculate mean here if we are maximizing sharpe ratio
          port.mean <- as.numeric(sum(weights * tmp_moments_mean))
          names(port.mean) <- "mean"
          obj_vals$mean <- port.mean
        }
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
    if(length(names(moments)) == 1 & "mean" %in% names(moments)) {
      # Set a default solver if optimize_method == "ROI", otherwise assume the
      # optimize_method specified by the user is the solver for ROI
      if(optimize_method == "ROI"){
        solver <- "glpk"
      } else {
        solver <- optimize_method
      }
      
      # Maximize return if the only objective specified is mean
      if(!is.null(constraints$max_pos) | !is.null(constraints$leverage)) {
        # This is an MILP problem if max_pos is specified as a constraint
        roi_result <- maxret_milp_opt(R=R, constraints=constraints, moments=moments, target=target, solver=solver, control=control)
        weights <- roi_result$weights
        # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        obj_vals <- roi_result$obj_vals
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      } else {
        # Maximize return LP problem
        roi_result <- maxret_opt(R=R, constraints=constraints, moments=moments, target=target, solver=solver, control=control)
        weights <- roi_result$weights
        # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        obj_vals <- roi_result$obj_vals
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
    if( any(c("CVaR", "ES", "ETL") %in% names(moments)) ) {
      # Set a default solver if optimize_method == "ROI", otherwise assume the
      # optimize_method specified by the user is the solver for ROI
      if(optimize_method == "ROI"){
        solver <- "glpk"
      } else {
        solver <- optimize_method
      }
      
      if(hasArg(ef)) ef=match.call(expand.dots=TRUE)$ef else ef=FALSE
      if(hasArg(maxSTARR)) maxSTARR=match.call(expand.dots=TRUE)$maxSTARR else maxSTARR=TRUE
      if(ef) meanetl <- TRUE else meanetl <- FALSE
      tmpnames <- c("CVaR", "ES", "ETL")
      idx <- which(tmpnames %in% names(moments))
      # Minimize sample ETL/ES/CVaR if CVaR, ETL, or ES is specified as an objective
      if(length(moments) == 2 & all(moments$mean != 0) & ef==FALSE & maxSTARR){
        # This is called by meanetl.efficient.frontier and we do not want that for efficient frontiers, need to have ef==FALSE
        target <- mean_etl_opt(R=R, constraints=constraints, moments=moments, alpha=alpha, solver=solver, control=control)
        meanetl <- TRUE
      }
      if(!is.null(constraints$max_pos)) {
        # This is an MILP problem if max_pos is specified as a constraint
        roi_result <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=target, alpha=alpha, solver=solver, control=control)
        weights <- roi_result$weights
        # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        # obj_vals <- roi_result$obj_vals
        # calculate obj_vals based on solver output
        obj_vals <- list()
        if(meanetl) obj_vals$mean <- sum(weights * moments$mean)
        obj_vals[[tmpnames[idx]]] <- roi_result$out
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      } else {
        # Minimize sample ETL/ES/CVaR LP Problem
        roi_result <- etl_opt(R=R, constraints=constraints, moments=moments, target=target, alpha=alpha, solver=solver, control=control)
        weights <- roi_result$weights
        # obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        # obj_vals <- roi_result$obj_vals
        obj_vals <- list()
        if(meanetl) obj_vals$mean <- sum(weights * moments$mean)
        obj_vals[[tmpnames[idx]]] <- roi_result$out
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
    # Set here at the end so we get optimize.portfolio.ROI and not optimize.portfolio.{solver} classes
    optimize_method <- "ROI"
  } ## end case for ROI
  
  ## case if method=pso---particle swarm
  if(optimize_method=="pso"){
    stopifnot("package:pso" %in% search()  ||  requireNamespace("pso",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlPSO <- list(trace=FALSE, fnscale=1, maxit=1000, maxf=Inf, abstol=-Inf, reltol=0)
    PSOcargs <- names(controlPSO)
    
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), PSOcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- PSOcargs[pm]
      controlPSO$maxit <- maxit
      controlPSO[pm] <- dotargs[pm > 0L]
      if(!hasArg(reltol)) controlPSO$reltol <- .000001 # 1/1000 of 1% change in objective is significant
      if(hasArg(trace) && try(trace==TRUE,silent=TRUE)) controlPSO$trace <- TRUE
      if(hasArg(trace) && isTRUE(trace)) {
        controlPSO$trace <- TRUE
        controlPSO$trace.stats=TRUE
      }
    }
    
    # get upper and lower weights parameters from constraints
    upper <- constraints$max
    lower <- constraints$min
    
    minw <- try(pso::psoptim( par = rep(NA, N), fn = constrained_objective,  R=R, portfolio=portfolio, env=dotargs,
                         lower = lower[1:N] , upper = upper[1:N] , control = controlPSO)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector( minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    obj_vals <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE, env=dotargs)$objective_measures
    out <- list(weights=weights, 
                objective_measures=obj_vals,
                opt_values=obj_vals,
                out=minw$value, 
                call=call)
    if (isTRUE(trace)){
      out$PSOoutput=minw
    }
    
  } ## end case for pso
  
  ## case if method=GenSA---Generalized Simulated Annealing
  if(optimize_method=="GenSA"){
    stopifnot("package:GenSA" %in% search()  ||  requireNamespace("GenSA",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlGenSA <- list(maxit = 5000, threshold.stop = NULL, temperature = 5230, 
                         visiting.param = 2.62, acceptance.param = -5, max.time = NULL, 
                         nb.stop.improvement = 1e+06, smooth = TRUE, max.call = 1e+07, 
                         verbose = FALSE)
    GenSAcargs <- names(controlGenSA)
    
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), GenSAcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- GenSAcargs[pm]
      controlGenSA$maxit <- maxit
      controlGenSA[pm] <- dotargs[pm > 0L]
      if(hasArg(trace) && try(trace==TRUE,silent=TRUE)) controlGenSA$verbose <- TRUE
    }
    
    upper <- constraints$max
    lower <- constraints$min

    if(!is.null(rp)) par = rp[,1] else par = rep(1/N, N)
      
    minw = try(GenSA::GenSA( par=par, lower = lower[1:N] , upper = upper[1:N], control = controlGenSA, 
                      fn = constrained_objective ,  R=R, portfolio=portfolio, env=dotargs)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector(minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    obj_vals <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE, env=dotargs)$objective_measures
    out = list(weights=weights, 
               objective_measures=obj_vals,
               opt_values=obj_vals,
               out=minw$value, 
               call=call)
    if (isTRUE(trace)){
      out$GenSAoutput=minw
    }
    
  } ## end case for GenSA
  
  # Prepare for final object to return
  end_t <- Sys.time()
  # print(c("elapsed time:",round(end_t-start_t,2),":diff:",round(diff,2), ":stats: ", round(out$stats,4), ":targets:",out$targets))
  if(message) message(c("elapsed time:", end_t-start_t))
  out$portfolio <- portfolio
  if(trace) out$R <- R
  out$data_summary <- list(first=first(R), last=last(R))
  out$elapsed_time <- end_t - start_t
  out$end_t <- as.character(Sys.time())
  # return a $regime element to indicate what regime portfolio used for
  # optimize.portfolio. The regime information is used in extractStats and
  # extractObjectiveMeasures
  if(regime.switching){
    out$regime <- regime.idx
  }
  class(out) <- c(paste("optimize.portfolio", optimize_method, sep='.'), "optimize.portfolio")
  return(out)
}

#' @rdname optimize.portfolio.rebalancing
#' @name optimize.portfolio.rebalancing
#' @export
optimize.portfolio.rebalancing_v1 <- function(R,constraints,optimize_method=c("DEoptim","random","ROI"), search_size=20000, trace=FALSE, ..., rp=NULL, rebalance_on=NULL, training_period=NULL, rolling_window=NULL)
{
    stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly=TRUE))
    start_t<-Sys.time()
      
    #store the call for later
    call <- match.call()
    if(optimize_method=="random"){
        # call random_portfolios() with constraints and search_size to create matrix of portfolios
        if(is.null(rp))
            rp<-random_portfolios(rpconstraints=constraints,permutations=search_size)
    } else {
        rp=NULL
    }
    
    # check for trailing_periods argument and set rolling_window equal to 
    # trailing_periods for backwards compatibility
    if(hasArg(trailing_periods)) {
      trailing_periods=match.call(expand.dots=TRUE)$trailing_periods
      rolling_window <- trailing_periods
    }
    
    if(is.null(training_period)) {if(nrow(R)<36) training_period=nrow(R) else training_period=36}
    if (is.null(rolling_window)){
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        ep <- ep.i[1]
        out_list<-foreach::foreach(ep=iterators::iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
                    optimize.portfolio(R[1:ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    } else {
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        out_list<-foreach::foreach(ep=iterators::iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
                    optimize.portfolio(R[(ifelse(ep-rolling_window>=1,ep-rolling_window,1)):ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    }
    names(out_list)<-index(R[ep.i])
    
    end_t<-Sys.time()
    message(c("overall elapsed time:",end_t-start_t))
    class(out_list)<-c("optimize.portfolio.rebalancing")
    return(out_list)
}

#' Portfolio Optimization with Rebalancing Periods
#' 
#' Portfolio optimization with support for rebalancing periods for 
#' out-of-sample testing (i.e. backtesting)
#' 
#' @details
#' Run portfolio optimization with periodic rebalancing at specified time periods. 
#' Running the portfolio optimization with periodic rebalancing can help 
#' refine the constraints and objectives by evaluating the out of sample
#' performance of the portfolio based on historical data.
#' 
#' If both \code{training_period} and \code{rolling_window} are \code{NULL}, 
#' then \code{training_period} is set to a default value of 36. 
#' 
#' If \code{training_period} is \code{NULL} and a \code{rolling_window} is 
#' specified, then \code{training_period} is set to the value of 
#' \code{rolling_window}.
#' 
#' The user should be aware of the following behavior when both 
#' \code{training_period} and \code{rolling_window} are specified and have 
#' different values
#' \itemize{
#'   \item{\code{training_period < rolling_window}: }{For example, if you have 
#'   \code{rolling_window=60}, \code{training_period=50}, and the periodicity 
#'   of the data is the same as the rebalance frequency (i.e. monthly data with 
#'   \code{rebalance_on="months")} then the returns data used in the optimization 
#'   at each iteration are as follows:
#'   \itemize{
#'   \item{1: R[1:50,]}
#'   \item{2: R[1:51,]}
#'   \item{...}
#'   \item{11: R[1:60,]}
#'   \item{12: R[1:61,]}
#'   \item{13: R[2:62,]}
#'   \item{...}
#'   }
#'   This results in a growing window for several optimizations initially while
#'   the endpoint iterator (i.e. \code{[50, 51, ...]}) is less than the 
#'   rolling window width.}
#'   \item{\code{training_period > rolling_window}: }{The data used in the initial 
#'   optimization is \code{R[(training_period - rolling_window):training_period,]}. 
#'   This results in some of the data being "thrown away", i.e. periods 1 to 
#'   \code{(training_period - rolling_window - 1)} are not used in the optimization.}
#' }
#' 
#' This function is a essentially a wrapper around \code{optimize.portfolio} 
#' and thus the discussion in the Details section of the 
#' \code{\link{optimize.portfolio}} help file is valid here as well.
#' 
#' This function is massively parallel and requires the 'foreach' package. It
#' is suggested to register a parallel backend.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints 
#' and objectives for the optimization
#' @param constraints default NULL, a list of constraint objects
#' @param objectives default NULL, a list of objective objects
#' @param optimize_method one of "DEoptim", "random", "pso", "GenSA", or "ROI"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional 
#' information on the path or portfolios searched
#' @param \dots any other passthru parameters to \code{\link{optimize.portfolio}}
#' @param rp a set of random portfolios passed into the function to prevent recalculation
#' @param rebalance_on character string of period to rebalance on. See 
#' \code{\link[xts]{endpoints}} for valid names.
#' @param training_period an integer of the number of periods to use as 
#' a training data in the front of the returns data
#' @param rolling_window an integer of the width (i.e. number of periods)
#' of the rolling window, the default of NULL will run the optimization 
#' using the data from inception.
#' @return a list containing the following elements
#' \itemize{
#'   \item{\code{portfolio}:}{ The portfolio object.}
#'   \item{\code{R}:}{ The asset returns.}
#'   \item{\code{call}:}{ The function call.}
#'   \item{\code{elapsed_time:}}{ The amount of time that elapses while the 
#'   optimization is run.}
#'   \item{\code{opt_rebalancing:}}{ A list of \code{optimize.portfolio} 
#'   objects computed at each rebalancing period.}
#' }
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @name optimize.portfolio.rebalancing
#' @aliases optimize.portfolio.rebalancing optimize.portfolio.rebalancing_v1
#' @seealso \code{\link{portfolio.spec}} \code{\link{optimize.portfolio}}
#' @examples
#' \dontrun{
#' data(edhec)
#' R <- edhec[,1:4]
#' funds <- colnames(R)
#' 
#' portf <- portfolio.spec(funds)
#' portf <- add.constraint(portf, type="full_investment")
#' portf <- add.constraint(portf, type="long_only")
#' portf <- add.objective(portf, type="risk", name="StdDev")
#' 
#' # Quarterly rebalancing with 5 year training period
#' bt.opt1 <- optimize.portfolio.rebalancing(R, portf,
#' optimize_method="ROI",
#' rebalance_on="quarters",
#' training_period=60)
#' 
#' # Monthly rebalancing with 5 year training period and 4 year rolling window
#' bt.opt2 <- optimize.portfolio.rebalancing(R, portf,
#' optimize_method="ROI",
#' rebalance_on="months",
#' training_period=60,
#' rolling_window=48)
#' }
#' @export
optimize.portfolio.rebalancing <- function(R, portfolio=NULL, constraints=NULL, objectives=NULL, optimize_method=c("DEoptim","random","ROI"), search_size=20000, trace=FALSE, ..., rp=NULL, rebalance_on=NULL, training_period=NULL, rolling_window=NULL)
{
  stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly=TRUE))
  stopifnot("package:iterators" %in% search() || requireNamespace("iterators",quietly=TRUE))
  
  # This is the case where the user has passed in a list of portfolio objects
  # for the portfolio argument.
  # Loop through the portfolio list and recursively call 
  # optimize.portfolio.rebalancing. 
  #Note that I return at the end of this block. I know it is not good practice
  # to return before the end of a function, but I am not sure of another way
  # to handle a list of portfolio objects with the recursive call to 
  # optimize.portfolio. 
  if(inherits(portfolio, "portfolio.list")){
    n.portf <- length(portfolio)
    opt.list <- vector("list", n.portf)
    for(i in 1:length(opt.list)){
      if(hasArg(message)) message=match.call(expand.dots=TRUE)$message else message=FALSE
      if(message) cat("Starting optimization of portfolio ", i, "\n")
      opt.list[[i]] <- optimize.portfolio.rebalancing(R=R, 
                                                      portfolio=portfolio[[i]], 
                                                      constraints=constraints, 
                                                      objectives=objectives, 
                                                      optimize_method=optimize_method, 
                                                      search_size=search_size, 
                                                      trace=trace, 
                                                      ...=..., 
                                                      rp=rp, 
                                                      rebalance_on=rebalance_on, 
                                                      training_period=training_period, 
                                                      rolling_window=rolling_window)
    }
    out <- combine.optimizations(opt.list)
    class(out) <- "opt.rebal.list"
    ##### return here for portfolio.list because this is a recursive call
    ##### for optimize.portfolio.rebalancing
    return(out)
  }
  
  # This is the case where the user has passed in a mult.portfolio.spec
  # object for multiple layer portfolio optimization.
  if(inherits(portfolio, "mult.portfolio.spec")){
    # This function calls optimize.portfolio.rebalancing on each sub portfolio
    # according to the given optimization parameters and returns an xts object
    # representing the proxy returns of each sub portfolio.
    R <- proxy.mult.portfolio(R=R, mult.portfolio=portfolio)
    # The optimization is controlled by the constraints and objectives in the
    # top level portfolio
    portfolio <- portfolio$top.portfolio
  }
  
  # Store the call to return later
  call <- match.call()
  
  start_t<-Sys.time()
  
  if (!is.null(portfolio) & !is.portfolio(portfolio)){
    stop("you must pass in an object of class 'portfolio' to control the optimization")
  }
  
  if(hasArg(message)) message=match.call(expand.dots=TRUE)$message else message=FALSE
  
  # check for trailing_periods argument and set rolling_window equal to 
  # trailing_periods for backwards compatibility
  if(hasArg(trailing_periods)) {
    trailing_periods=match.call(expand.dots=TRUE)$trailing_periods
    rolling_window <- trailing_periods
  }
  
  
  # Check for constraints and objectives passed in separately outside of the portfolio object
  if(!is.null(constraints)){
    if(inherits(constraints, "v1_constraint")){
      if(is.null(portfolio)){
        # If the user has not passed in a portfolio, we will create one for them
        tmp_portf <- portfolio.spec(assets=constraints$assets)
      }
      message("constraint object passed in is a 'v1_constraint' object, updating to v2 specification")
      portfolio <- update_constraint_v1tov2(portfolio=tmp_portf, v1_constraint=constraints)
      # print.default(portfolio)
    }
    if(!inherits(constraints, "v1_constraint")){
      # Insert the constraints into the portfolio object
      portfolio <- insert_constraints(portfolio=portfolio, constraints=constraints)
    }
  }
  if(!is.null(objectives)){
    # Insert the objectives into the portfolio object
    portfolio <- insert_objectives(portfolio=portfolio, objectives=objectives)
  }
  
  #store the call for later
  call <- match.call()
  if(optimize_method=="random"){
    # get any rp related arguments passed in through dots
    if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
    if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
    if(hasArg(fev)) fev=match.call(expand.dots=TRUE)$fev else fev=0:5
    
    # call random_portfolios() with constraints and search_size to create matrix of portfolios
    if(is.null(rp))
      if(inherits(portfolio, "regime.portfolios")){
        rp <- rp.regime.portfolios(regime=portfolio, permutations=search_size, rp_method=rp_method, eliminate=eliminate, fev=fev)
      } else {
        rp <- random_portfolios(portfolio=portfolio, permutations=search_size, rp_method=rp_method, eliminate=eliminate, fev=fev)
      }
  } else {
    rp = NULL
  }
  # set training_period equal to rolling_window if training_period is null
  # and rolling_window is not null
  if(is.null(training_period) & !is.null(rolling_window))
    training_period <- rolling_window
  
  if(is.null(training_period)) {if(nrow(R)<36) training_period=nrow(R) else training_period=36}
  if (is.null(rolling_window)){
    # define the index endpoints of our periods
    ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
    # now apply optimize.portfolio to the periods, in parallel if available
    ep <- ep.i[1]
    out_list<-foreach::foreach(ep=iterators::iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
      optimize.portfolio(R[1:ep,], portfolio=portfolio, optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
    }
  } else {
    # define the index endpoints of our periods
    ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
    # now apply optimize.portfolio to the periods, in parallel if available
    out_list<-foreach::foreach(ep=iterators::iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
      optimize.portfolio(R[(ifelse(ep-rolling_window>=1,ep-rolling_window,1)):ep,], portfolio=portfolio, optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
    }
  }
  # out_list is a list where each element is an optimize.portfolio object
  # at each rebalance date
  names(out_list)<-index(R[ep.i])
  
  end_t <- Sys.time()
  elapsed_time <- end_t - start_t
  if(message) message(c("overall elapsed time:", end_t-start_t))
  
  # out object to return
  out <- list()
  out$portfolio <- portfolio
  out$R <- R
  out$call <- call
  out$elapsed_time <- elapsed_time
  out$opt_rebalancing <- out_list
  
  class(out) <- c("optimize.portfolio.rebalancing")
  return(out)
}

#' Execute multiple optimize.portfolio calls, presumably in parallel
#' 
#' This function will not speed up optimization!
#' 
#' This function exists to run multiple copies of optimize.portfolio, presumabley in parallel using foreach.
#' 
#' This is typically done to test your parameter settings, specifically 
#' total population size, but also possibly to help tune your 
#' convergence settings, number of generations, stopping criteria,
#' etc.
#' 
#' If you want to use all the cores on your multi-core computer, use 
#' the parallel version of the apppropriate optimization engine, not 
#' this function.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param optimize_method one of "DEoptim", "random", "pso", "GenSA".
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param rp matrix of random portfolio weights, default NULL, mostly for automated use by rebalancing optimization or repeated tests on same portfolios
#' @param momentFUN the name of a function to call to set portfolio moments, default \code{\link{set.portfolio.moments_v2}}
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param nodes how many processes to run in the foreach loop, default 4
#' 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio.parallel <- function(R,
                                        portfolio,
                                        optimize_method=c("DEoptim","random","ROI","pso","GenSA"),
                                        search_size=20000,
                                        trace=FALSE, ...,
                                        rp=NULL,
                                        momentFUN='set.portfolio.moments',
                                        message=FALSE,
                                        nodes=4)
{
    stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly=TRUE))
    optimize_method=optimize_method[1]  
    
    start_t <- Sys.time()
    
    #store the call for later
    call <- match.call()
    
    opt_out_list <- foreach::foreach(1:nodes, .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
      optimize.portfolio(R=R, portfolio=portfolio, 
                         optimize_method=optimize_method, 
                         search_size=search_size, trace=trace, 
                         rp=rp, momentFUN=momentFUN, parallel=FALSE, 
                         ...=...)
    }

    end_t <- Sys.time()
    elapsed_t <- end_t - start_t
    if(message) message(c("overall elapsed time:", elapsed_t))
    
    out <- list()
    out$optimizations <- opt_out_list
    out$call <- call
    out$elapsed_time <- elapsed_t
    
    class(out) <- c("optimize.portfolio.parallel")
    return(out)
}


#TODO write function to compute an efficient frontier of optimal portfolios

###############################################################################
# $Id$
###############################################################################
