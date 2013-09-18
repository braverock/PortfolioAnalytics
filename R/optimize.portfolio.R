###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2012 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


#' @rdname optimize.portfolio
#' @name optimize.portfolio
#' @export
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
          
          ##' NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
          ##' In Kris' original function, this was manifested as a full investment constraint
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
    stopifnot("package:DEoptim" %in% search()  ||  require("DEoptim",quietly = TRUE) )
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
        registerDoSEQ()
    }
    
    DEcformals  <- formals(DEoptim.control)
    DEcargs <- names(DEcformals)
    if( is.list(dotargs) ){
        pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
        names(dotargs[pm > 0L]) <- DEcargs[pm]
        DEcformals$NP <- NP
        DEcformals$itermax <- itermax
        DEcformals[pm] <- dotargs[pm > 0L]
		if(!hasArg(strategy)) DEcformals$strategy=6 # use DE/current-to-p-best/1
		if(!hasArg(reltol)) DEcformals$reltol=.000001 # 1/1000 of 1% change in objective is significant
		if(!hasArg(steptol)) DEcformals$steptol=round(N*1.5) # number of assets times 1.5 tries to improve
		if(!hasArg(c)) DEcformals$c=.4 # JADE mutation parameter, this could maybe use some adjustment
        if(!hasArg(storepopfrom)) DEcformals$storepopfrom=1
        if(isTRUE(parallel) && 'package:foreach' %in% search()){
            if(!hasArg(parallelType) ) DEcformals$parallelType='auto' #use all cores
            if(!hasArg(packages) ) DEcformals$packages <- names(sessionInfo()$otherPkgs) #use all packages
        }
		 
        #TODO FIXME also check for a passed in controlDE list, including checking its class, and match formals
    }
    
    if(isTRUE(trace)) { 
        #we can't pass trace=TRUE into constrained objective with DEoptim, because it expects a single numeric return
        tmptrace=trace 
        assign('.objectivestorage', list(), pos='.GlobalEnv')
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
    controlDE <- do.call(DEoptim.control,DEcformals)

    # minw = try(DEoptim( constrained_objective ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, ...=...)) # add ,silent=TRUE here?
    minw = try(DEoptim( constrained_objective_v1 ,  lower = lower[1:N] , upper = upper[1:N] , control = controlDE, R=R, constraints=constraints, nargs = dotargs , ...=...)) # add ,silent=TRUE here?
 
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
        out$DEoptim_objective_results<-try(get('.objectivestorage',pos='.GlobalEnv'),silent=TRUE)
        rm('.objectivestorage',pos='.GlobalEnv')
    }
    
  } ## end case for DEoptim
  
  
  if(optimize_method=="random"){
      #' call random_portfolios() with constraints and search_size to create matrix of portfolios
      if(missing(rp) | is.null(rp)){
          rp<-random_portfolios_v1(rpconstraints=constraints,permutations=search_size)
      }
      #' store matrix in out if trace=TRUE
      if (isTRUE(trace)) out$random_portfolios<-rp
      #' write foreach loop to call constrained_objective() with each portfolio
      if ("package:foreach" %in% search() & !hasArg(parallel)){
          rp_objective_results<-foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective_v1(w=rp[ii,],R,constraints,trace=trace,...=dotargs)
      } else {
          rp_objective_results<-apply(rp, 1, constrained_objective_v1, R=R, constraints=constraints, trace=trace, ...=dotargs)
      }
      #' if trace=TRUE , store results of foreach in out$random_results
      if(isTRUE(trace)) out$random_portfolio_objective_results<-rp_objective_results
      #' loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
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
      #' re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
      out$weights<-min_objective_weights
      out$objective_measures<-try(constrained_objective_v1(w=min_objective_weights,R=R,constraints,trace=TRUE)$objective_measures)
      out$call<-call
      #' construct out list to be as similar as possible to DEoptim list, within reason

  } ## end case for random
  
  
  if(optimize_method == "ROI_old"){
    # This will take a new constraint object that is of the same structure of a 
    # ROI constraint object, but with an additional solver arg.
    # then we can do something like this
    print("ROI_old is going to be depricated.")
    roi.result <- ROI:::ROI_solve(x=constraints$constrainted_objective, constraints$solver)
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
    if(plugin == "quadprog") ROI_objective <- ROI:::Q_objective(Q=2*lambda*moments$var, L=-moments$mean)
    if(plugin == "glpk") ROI_objective <- ROI:::L_objective(L=-moments$mean)
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
      ROI_objective <- ROI:::L_objective(c(rep(0,N), rep(1/(alpha*T),T), 1))
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
    opt.prob <- ROI:::OP(objective=ROI_objective, 
                         constraints=ROI:::L_constraint(L=Amat, dir=dir.vec, rhs=rhs.vec),
                         bounds=bnds)
    roi.result <- ROI:::ROI_solve(x=opt.prob, solver=plugin)
    weights <- roi.result$solution[1:N]
    names(weights) <- colnames(R)
    out$weights <- weights
    out$out <- roi.result$objval
    out$call <- call
  } ## end case for ROI

  
  ## case if method=pso---particle swarm
  if(optimize_method=="pso"){
    stopifnot("package:pso" %in% search()  ||  require("pso",quietly = TRUE) )
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
        
      if(!hasArg(s)) controlPSO$s<-N*10 #swarm size
      if(!hasArg(maxit.stagnate)) controlPSO$maxit.stagnate <- controlPSO$s #stopping criteria      
      if(hasArg(trace) && try(trace==TRUE,silent=TRUE)) controlPSO$trace <- TRUE
      if(hasArg(trace) && isTRUE(trace)) {
          controlPSO$trace <- TRUE
          controlPSO$trace.stats=TRUE
      }
  }
    
    # get upper and lower weights parameters from constraints
    upper <- constraints$max
    lower <- constraints$min
    
    minw = try(psoptim( par = rep(NA, N), fn = constrained_objective_v1 ,  R=R, constraints=constraints,
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
    stopifnot("package:GenSA" %in% search()  ||  require("GenSA",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlGenSA <- list(maxit = 5000, threshold.stop = NULL, temp = 5230, 
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
    
    minw = try(GenSA( par = rep(1/N, N), lower = lower[1:N] , upper = upper[1:N], control = controlGenSA, 
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

##### version 2 of optimize.portfolio #####
#' @rdname optimize.portfolio
#' @export
optimize.portfolio_v2 <- function(
  R,
  portfolio=NULL,
  constraints=NULL,
  objectives=NULL,
  optimize_method=c("DEoptim","random","ROI","ROI_old","pso","GenSA"),
  search_size=20000,
  trace=FALSE, ...,
  rp=NULL,
  momentFUN='set.portfolio.moments',
  message=FALSE
)
{
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
  
  out <- list()
  
  weights <- NULL
  
  dotargs <- list(...)    
  
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
  # TODO FIXME should match formals later
  #dotargs <- set.portfolio.moments(R, constraints, momentargs=dotargs)
  .mformals <- dotargs
  #.mformals$R <- R
  #.mformals$portfolio <- portfolio
  .formals <- formals(momentFUN)
  onames <- names(.formals)
  if (length(.mformals)) {
    dargs <- .mformals
    pm <- pmatch(names(dargs), onames, nomatch = 0L)
    names(dargs[pm > 0L]) <- onames[pm]
    .formals[pm] <- dargs[pm > 0L]
  }
  .formals$R <- R
  .formals$portfolio <- portfolio
  .formals$... <- NULL
  
  # If no dotargs are passed in, .formals was a pairlist and do.call was failing
  if(!inherits(.formals, "list")) .formals <- as.list(.formals)
  
  mout <- try((do.call(momentFUN, .formals)) ,silent=TRUE)
  if(inherits(mout,"try-error")) { 
    message(paste("portfolio moment function failed with message",mout))
  } else {
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
      
      ##' NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
      ##' In Kris' original function, this was manifested as a full investment constraint
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
    stopifnot("package:DEoptim" %in% search()  ||  require("DEoptim",quietly = TRUE))
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
      registerDoSEQ()
    }
    
    DEcformals <- formals(DEoptim.control)
    DEcargs <- names(DEcformals)
    if( is.list(dotargs) ){
      pm <- pmatch(names(dotargs), DEcargs, nomatch = 0L)
      names(dotargs[pm > 0L]) <- DEcargs[pm]
      DEcformals$NP <- NP
      DEcformals$itermax <- itermax
      DEcformals[pm] <- dotargs[pm > 0L]
      if(!hasArg(strategy)) DEcformals$strategy=6 # use DE/current-to-p-best/1
      if(!hasArg(reltol)) DEcformals$reltol=.000001 # 1/1000 of 1% change in objective is significant
      if(!hasArg(steptol)) DEcformals$steptol=round(N*1.5) # number of assets times 1.5 tries to improve
      if(!hasArg(c)) DEcformals$c=.4 # JADE mutation parameter, this could maybe use some adjustment
      if(!hasArg(storepopfrom)) DEcformals$storepopfrom=1
      if(isTRUE(parallel) && 'package:foreach' %in% search()){
        if(!hasArg(parallelType) ) DEcformals$parallelType='auto' #use all cores
        if(!hasArg(packages) ) DEcformals$packages <- names(sessionInfo()$otherPkgs) #use all packages
      }
      #TODO FIXME also check for a passed in controlDE list, including checking its class, and match formals
    }
    if(hasArg(traceDE)) traceDE=match.call(expand.dots=TRUE)$traceDE else traceDE=TRUE
    DEcformals$trace <- traceDE
    
    if(isTRUE(trace)) { 
      #we can't pass trace=TRUE into constrained objective with DEoptim, because it expects a single numeric return
      tmptrace <- trace 
      assign('.objectivestorage', list(), pos='.GlobalEnv')
      trace=FALSE
    } 
    
    # get upper and lower weights parameters from constraints
    upper <- constraints$max
    lower <- constraints$min
    
    if(hasArg(rpseed)){ 
      seed <- match.call(expand.dots=TRUE)$rpseed
      DEcformals$initialpop <- seed
      rpseed <- FALSE
    } else {
      rpseed <- TRUE
    }
    if(hasArg(rpseed) & isTRUE(rpseed)) {
      # initial seed population is generated with random_portfolios function
      # if(hasArg(eps)) eps=match.call(expand.dots=TRUE)$eps else eps = 0.01
      if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
      if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
      rp <- random_portfolios(portfolio=portfolio, permutations=NP, rp_method=rp_method, eliminate=eliminate, ...)
      DEcformals$initialpop <- rp
    }
    controlDE <- do.call(DEoptim.control, DEcformals)
    
    # We are passing fn_map to the optional fnMap function to do the 
    # transformation so we need to force normalize=FALSE in call to constrained_objective
    minw = try(DEoptim( constrained_objective,  lower=lower[1:N], upper=upper[1:N], control=controlDE, R=R, portfolio=portfolio, nargs = dotargs , ...=..., normalize=FALSE, fnMap=function(x) fn_map(x, portfolio=portfolio)$weights)) # add ,silent=TRUE here?
    
    if(inherits(minw, "try-error")) { minw=NULL }
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
    obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
    out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=minw$optim$bestval, call=call)
    if (isTRUE(trace)){
      out$DEoutput <- minw
      out$DEoptim_objective_results <- try(get('.objectivestorage',pos='.GlobalEnv'),silent=TRUE)
      rm('.objectivestorage',pos='.GlobalEnv')
    }
    
  } ## end case for DEoptim
  
  # case for random portfolios optimization method
  if(optimize_method=="random"){
    #' call random_portfolios() with portfolio and search_size to create matrix of portfolios
    if(missing(rp) | is.null(rp)){
      if(hasArg(rp_method)) rp_method=match.call(expand.dots=TRUE)$rp_method else rp_method="sample"
      if(hasArg(eliminate)) eliminate=match.call(expand.dots=TRUE)$eliminate else eliminate=TRUE
      rp <- random_portfolios(portfolio=portfolio, permutations=search_size, rp_method=rp_method, eliminate=eliminate, ...)
    }
    #' store matrix in out if trace=TRUE
    if (isTRUE(trace)) out$random_portfolios <- rp
    # rp is already being generated with a call to fn_map so set normalize=FALSE in the call to constrained_objective
    #' write foreach loop to call constrained_objective() with each portfolio
    if ("package:foreach" %in% search() & !hasArg(parallel)){
      rp_objective_results <- foreach(ii=1:nrow(rp), .errorhandling='pass') %dopar% constrained_objective(w=rp[ii,], R, portfolio, trace=trace,...=dotargs, normalize=FALSE)
    } else {
      rp_objective_results <- apply(rp, 1, constrained_objective, R=R, portfolio=portfolio, trace=trace, ...=dotargs, normalize=FALSE)
    }
    #' if trace=TRUE , store results of foreach in out$random_results
    if(isTRUE(trace)) out$random_portfolio_objective_results <- rp_objective_results
    #' loop through results keeping track of the minimum value of rp_objective_results[[objective]]$out
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
    #' re-call constrained_objective on the best portfolio, as above in DEoptim, with trace=TRUE to get results for out list
    out$weights <- min_objective_weights
    obj_vals <- try(constrained_objective(w=min_objective_weights, R=R, portfolio=portfolio, trace=TRUE, normalize=FALSE)$objective_measures)
    out$objective_measures <- obj_vals
    out$opt_values <- obj_vals
    out$call <- call
    #' construct out list to be as similar as possible to DEoptim list, within reason
    
  } ## end case for random
  
  if(optimize_method == "ROI"){
    # This takes in a regular portfolio object and extracts the desired business objectives
    # and converts them to matrix form to be inputed into a closed form solver
    # retrieve the objectives to minimize, these should either be "var" and/or "mean"
    # we can either miniminze variance or maximize quiadratic utility (we will be minimizing the neg. quad. utility)
    moments <- list(mean=rep(0, N))
    alpha <- 0.05
    if(!is.null(constraints$return_target)){
      target <- constraints$return_target
    } else {
      target <- NA
    }
    # comment out so concentration aversion can only be specified as an objective
    # because it is added to the quadratic objective term for QP problems (minvar and qu)
    # if(!is.null(constraints$conc_aversion)){
    #  lambda_hhi <- constraints$conc_aversion
    #} else {
    #  lambda_hhi <- 0
    #}
    lambda <- 1
    for(objective in portfolio$objectives){
      if(objective$enabled){
        if(!any(c(objective$name == "HHI", objective$name == "mean", objective$name == "var", objective$name == "CVaR", objective$name == "ES", objective$name == "ETL")))
          stop("ROI only solves mean, var, or sample ETL/ES/CVaR type business objectives, choose a different optimize_method.")
        # I'm not sure what changed, but moments$mean used to be a vector of the column means
        # now it is a scalar value of the mean of the entire R object
        if(objective$name == "mean"){
          moments[[objective$name]] <- try(as.vector(apply(R, 2, "mean", na.rm=TRUE)), silent=TRUE)
        } else {
          moments[[objective$name]] <- try(eval(as.symbol(objective$name))(R), silent=TRUE)
        }
        target <- ifelse(!is.null(objective$target), objective$target, target)
        alpha <- ifelse(!is.null(objective$alpha), objective$alpha, alpha)
        lambda <- ifelse(!is.null(objective$risk_aversion), objective$risk_aversion, lambda)
        if(!is.null(objective$conc_aversion)) lambda_hhi <- objective$conc_aversion else lambda_hhi <- NULL
        if(!is.null(objective$conc_groups)) conc_groups <- objective$conc_groups else conc_groups <- NULL
      }
    }
    if("var" %in% names(moments)){
      # Minimize variance if the only objective specified is variance
      # Maximize Quadratic Utility if var and mean are specified as objectives
      if(!is.null(constraints$turnover_target) | !is.null(constraints$ptc)){
        if(!is.null(constraints$turnover_target) & !is.null(constraints$ptc)){
          warning("Turnover and proportional transaction cost constraints detected, only running optimization for turnover constraint.")
          constraints$ptc <- NULL
        }
        if(!is.null(constraints$turnover_target) & is.null(constraints$ptc)){
          qp_result <- gmv_opt_toc(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, init_weights=portfolio$assets)
          weights <- qp_result$weights
          obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
          out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=qp_result$out, call=call)
        }
        if(!is.null(constraints$ptc) & is.null(constraints$turnover_target)){
          qp_result <- gmv_opt_ptc(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, init_weights=portfolio$assets)
          weights <- qp_result$weights
          obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
          out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=qp_result$out, call=call)
        }
      } else {
        roi_result <- gmv_opt(R=R, constraints=constraints, moments=moments, lambda=lambda, target=target, lambda_hhi=lambda_hhi, conc_groups=conc_groups)
        weights <- roi_result$weights
        obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
    if(length(names(moments)) == 1 & "mean" %in% names(moments)) {
      # Maximize return if the only objective specified is mean
      if(!is.null(constraints$max_pos)) {
        # This is an MILP problem if max_pos is specified as a constraint
        roi_result <- maxret_milp_opt(R=R, constraints=constraints, moments=moments, target=target)
        weights <- roi_result$weights
        obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      } else {
        # Maximize return LP problem
        roi_result <- maxret_opt(R=R, constraints=constraints, moments=moments, target=target)
        weights <- roi_result$weights
        obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
    if( any(c("CVaR", "ES", "ETL") %in% names(moments)) ) {
      # Minimize sample ETL/ES/CVaR if CVaR, ETL, or ES is specified as an objective
      if(!is.null(constraints$max_pos)) {
        # This is an MILP problem if max_pos is specified as a constraint
        roi_result <- etl_milp_opt(R=R, constraints=constraints, moments=moments, target=target, alpha=alpha)
        weights <- roi_result$weights
        obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      } else {
        # Minimize sample ETL/ES/CVaR LP Problem
        roi_result <- etl_opt(R=R, constraints=constraints, moments=moments, target=target, alpha=alpha)
        weights <- roi_result$weights
        obj_vals <- constrained_objective(w=weights, R=R, portfolio, trace=TRUE, normalize=FALSE)$objective_measures
        out <- list(weights=weights, objective_measures=obj_vals, opt_values=obj_vals, out=roi_result$out, call=call)
      }
    }
  } ## end case for ROI
  
  ## case if method=pso---particle swarm
  if(optimize_method=="pso"){
    stopifnot("package:pso" %in% search()  ||  require("pso",quietly = TRUE) )
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
    
    minw <- try(psoptim( par = rep(NA, N), fn = constrained_objective,  R=R, portfolio=portfolio,
                         lower = lower[1:N] , upper = upper[1:N] , control = controlPSO)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector( minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    obj_vals <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE)$objective_measures
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
    stopifnot("package:GenSA" %in% search()  ||  require("GenSA",quietly = TRUE) )
    if(hasArg(maxit)) maxit=match.call(expand.dots=TRUE)$maxit else maxit=N*50
    controlGenSA <- list(maxit = 5000, threshold.stop = NULL, temp = 5230, 
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
    
    minw = try(GenSA( par = rep(1/N, N), lower = lower[1:N] , upper = upper[1:N], control = controlGenSA, 
                      fn = constrained_objective ,  R=R, portfolio=portfolio)) # add ,silent=TRUE here?
    
    if(inherits(minw,"try-error")) { minw=NULL }
    if(is.null(minw)){
      message(paste("Optimizer was unable to find a solution for target"))
      return (paste("Optimizer was unable to find a solution for target"))
    }
    
    weights <- as.vector(minw$par)
    weights <- normalize_weights(weights)
    names(weights) <- colnames(R)
    obj_vals <- constrained_objective(w=weights, R=R, portfolio=portfolio, trace=TRUE)$objective_measures
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
  class(out) <- c(paste("optimize.portfolio", optimize_method, sep='.'), "optimize.portfolio")
  return(out)
}

#' constrained optimization of portfolios
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
#' The extension to ROI solves a limited type of convex optimization problems:
#' \itemize{
#' \item{Maxmimize portfolio return subject leverage, box, group, position limit, target mean return, and/or factor exposure constraints on weights.}
#' \item{Minimize portfolio variance subject to leverage, box, group, turnover, and/or factor exposure constraints (otherwise known as global minimum variance portfolio).}
#' \item{Minimize portfolio variance subject to leverage, box, group, and/or factor exposure constraints and a desired portfolio return.}
#' \item{Maximize quadratic utility subject to leverage, box, group, target mean return, turnover, and/or factor exposure constraints and risk aversion parameter.
#' (The risk aversion parameter is passed into \code{optimize.portfolio} as an added argument to the \code{portfolio} object).}
#' \item{Mean CVaR optimization subject to leverage, box, group, position limit, target mean return, and/or factor exposure constraints and target portfolio return.}
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
#' @param constraints default=NULL, a list of constraint objects. An object of class v1_constraint' can be passed in here.
#' @param objectives default=NULL, a list of objective objects.
#' @param optimize_method one of "DEoptim", "random", "ROI","ROI_old", "pso", "GenSA".  For using \code{ROI_old}, need to use a constraint_ROI object in constraints. For using \code{ROI}, pass standard \code{constratint} object in \code{constraints} argument.  Presently, ROI has plugins for \code{quadprog} and \code{Rglpk}.
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
#' @name optimize.portfolio
#' @export
optimize.portfolio <- optimize.portfolio_v2


#' @rdname optimize.portfolio.rebalancing
#' @name optimize.portfolio.rebalancing
#' @export
optimize.portfolio.rebalancing_v1 <- function(R,constraints,optimize_method=c("DEoptim","random","ROI"), search_size=20000, trace=FALSE, ..., rp=NULL, rebalance_on=NULL, training_period=NULL, trailing_periods=NULL)
{
    stopifnot("package:foreach" %in% search() || require("foreach",quietly=TRUE))
    start_t<-Sys.time()
      
    #store the call for later
    call <- match.call()
    if(optimize_method=="random"){
        #' call random_portfolios() with constraints and search_size to create matrix of portfolios
        if(is.null(rp))
            rp<-random_portfolios(rpconstraints=constraints,permutations=search_size)
    } else {
        rp=NULL
    }    
    
    if(is.null(training_period)) {if(nrow(R)<36) training_period=nrow(R) else training_period=36}
    if (is.null(trailing_periods)){
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        out_list<-foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
                    optimize.portfolio(R[1:ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    } else {
        # define the index endpoints of our periods
        ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
        # now apply optimize.portfolio to the periods, in parallel if available
        out_list<-foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
                    optimize.portfolio(R[(ifelse(ep-trailing_periods>=1,ep-trailing_periods,1)):ep,],constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
                  }
    }
    names(out_list)<-index(R[ep.i])
    
    end_t<-Sys.time()
    message(c("overall elapsed time:",end_t-start_t))
    class(out_list)<-c("optimize.portfolio.rebalancing")
    return(out_list)
}

#' portfolio optimization with support for rebalancing or rolling periods
#' 
#' This function may eventually be wrapped into optimize.portfolio
#' 
#' For now, we'll set the rebalancing periods here, though I think they should eventually be part of the constraints object
#' 
#' This function is massively parallel, and will require 'foreach' and we suggest that you register a parallel backend.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param constraints default=NULL, a list of constraint objects
#' @param objectives default=NULL, a list of objective objects
#' @param optimize_method one of "DEoptim", "random", "pso", "GenSA", or "ROI"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param rp a set of random portfolios passed into the function, to prevent recalculation
#' @param rebalance_on a periodicity as returned by xts function periodicity and usable by endpoints
#' @param training_period period to use as training in the front of the data
#' @param trailing_periods if set, an integer with the number of periods to roll over, default NULL will run from inception 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @name optimize.portfolio.rebalancing
#' @aliases optimize.portfolio.rebalancing optimize.portfolio.rebalancing_v1
#' @export
optimize.portfolio.rebalancing <- function(R, portfolio=NULL, constraints=NULL, objectives=NULL, optimize_method=c("DEoptim","random","ROI"), search_size=20000, trace=FALSE, ..., rp=NULL, rebalance_on=NULL, training_period=NULL, trailing_periods=NULL)
{
  stopifnot("package:foreach" %in% search() || require("foreach",quietly=TRUE))
  stopifnot("package:iterators" %in% search() || require("iterators",quietly=TRUE))
  start_t<-Sys.time()
  
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
  
  #store the call for later
  call <- match.call()
  if(optimize_method=="random"){
    #' call random_portfolios() with constraints and search_size to create matrix of portfolios
    if(is.null(rp))
      rp<-random_portfolios(portfolio=portfolio, permutations=search_size)
  } else {
    rp=NULL
  }    
  
  if(is.null(training_period)) {if(nrow(R)<36) training_period=nrow(R) else training_period=36}
  if (is.null(trailing_periods)){
    # define the index endpoints of our periods
    ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
    # now apply optimize.portfolio to the periods, in parallel if available
    out_list<-foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
      optimize.portfolio(R[1:ep,], portfolio=portfolio, optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
    }
  } else {
    # define the index endpoints of our periods
    ep.i<-endpoints(R,on=rebalance_on)[which(endpoints(R, on = rebalance_on)>=training_period)]
    # now apply optimize.portfolio to the periods, in parallel if available
    out_list<-foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
      optimize.portfolio(R[(ifelse(ep-trailing_periods>=1,ep-trailing_periods,1)):ep,], portfolio=portfolio, optimize_method=optimize_method, search_size=search_size, trace=trace, rp=rp, parallel=FALSE, ...=...)
    }
  }
  names(out_list)<-index(R[ep.i])
  
  end_t<-Sys.time()
  message(c("overall elapsed time:",end_t-start_t))
  class(out_list)<-c("optimize.portfolio.rebalancing")
  return(out_list)
}

#'execute multiple optimize.portfolio calls, presumably in parallel
#' 
#' TODO write function to check sensitivity of optimal results by using optimize.portfolio.parallel results
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
#' @param constraints an object of type "constraints" specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param optimize_method one of "DEoptim" or "random"
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param trace TRUE/FALSE if TRUE will attempt to return additional information on the path or portfolios searched
#' @param \dots any other passthru parameters
#' @param nodes how many processes to run in the foreach loop, default 4
#' 
#' @return a list containing the optimal weights, some summary statistics, the function call, and optionally trace information 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson
#' @export
optimize.portfolio.parallel <- function(R,constraints,optimize_method=c("DEoptim","random"), search_size=20000, trace=FALSE, ..., nodes=4)
{
    stopifnot("package:foreach" %in% search() || require("foreach",quietly=TRUE))
    optimize_method=optimize_method[1]  
    
    start_t<-Sys.time()
    
    #store the call for later
    call <- match.call()
    
    opt_out_list<-foreach(1:nodes, packages='PortfolioAnalytics') %dopar% optimize.portfolio(R=R,constraints=constraints,optimize_method=optimize_method, search_size=search_size, trace=trace, ...)    

    end_t<-Sys.time()
    message(c("overall elapsed time:",end_t-start_t))
    class(opt_out_list)<-c("optimize.portfolio.parallel")
    return(opt_out_list)
    
}


#TODO write function to compute an efficient frontier of optimal portfolios

###############################################################################
# $Id$
###############################################################################
