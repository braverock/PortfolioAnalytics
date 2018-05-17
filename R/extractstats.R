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

#' utility function to replace awkward named from unlist
#' @param rnames character vector of names to check for cleanup
name.replace <- function(rnames){
  rnames<-gsub("objective_measures.",'',rnames)
  matchvec<-c('mean.mean','median.median','ES.ES','ETL.ETL','CVaR.ES','ES.MES','ETL.MES','CVaR.MES','VaR.MVaR','maxDrawdown.maxDrawdown','sd.sd','StdDev.StdDev')
  for(str in matchvec){
    pos<-pmatch(str,rnames)
    if(!is.na(pos)){
      switch(str,
             mean.mean = {rnames[pos]<-'mean'},
             median.median = {rnames[pos]<-'median'},
             CVaR.MES =, CVaR.ES = {rnames[pos]<-'CVaR'}, 
             ES.MES =, ES.ES =  {rnames[pos]<-'ES'},
             ETL.MES =, ETL.ETL =  {rnames[pos]<-'ETL'},
             VaR.MVaR = {rnames[pos]<-'VaR'},
             maxDrawdown.maxDrawdown = {rnames[pos]<-'maxDrawdown'},
             sd.sd=, StdDev.StdDev = {rnames[pos]<-'StdDev'},
             #pamean={rnames[pos]<-'mean'}
      )
    }
  } 
  return(rnames)
}

##### extractStats #####

#' extract some stats and weights from a portfolio run via \code{optimize.portfolio}
#' 
#' This function will dispatch to the appropriate class handler based on the
#' input class of the optimize.portfolio output object.
#' 
#' For \code{optimize.portfolio} objects:
#' 
#' In general, \code{extractStats} will extract the values objective measures 
#' and weights at each iteration of a set of weights. This is the case for the
#' DEoptim, random portfolios, and pso solvers that return trace information. 
#' Note that \code{trace=TRUE} must be specified in \code{optimize.portfolio} 
#' to return the trace information.
#' 
#' For \code{optimize.portfolio.pso} objects, this function will extract the 
#' weights (swarm positions) from the PSO output and the out values
#' (swarm fitness values) for each iteration of the optimization.
#' This function can be slow because we need to run \code{constrained_objective}
#' to calculate the objective measures on the transformed weights.
#' 
#' For \code{optimize.portfolio.rebalancing} objects:
#' 
#' The \code{extractStats} function will return a list of the objective measures 
#' and weights at each rebalance date for \code{optimize.portfolio.rebalancing}
#' objects. The objective measures and weights of each iteration or permutation 
#' will be returned if the optimization was done with DEoptim, random portfolios, 
#' or pso. This could potentially result in a very large list object where each 
#' list element has thousands of rows of at each rebalance period.
#' 
#' The output from the GenSA solver does not store weights evaluated at each iteration
#' The GenSA output for trace.mat contains nb.steps, temperature, function.value, and current.minimum
#'
#' @param object list returned by optimize.portfolio
#' @param prefix prefix to add to output row names
#' @param ... any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}
#' @aliases extractStats extractStats.optimize.portfolio.DEoptim
#' extractStats.optimize.portfolio.parallel extractStats.optimize.portfolio.random
#' extractStats.optimize.portfolio.ROI extractStats.optimize.portfolio.pso
#' extractStats.optimize.portfolio.GenSA
#' @export
extractStats <- function (object, prefix=NULL, ...){
    UseMethod('extractStats')
}

#' @method extractStats optimize.portfolio.DEoptim
#' @S3method extractStats optimize.portfolio.DEoptim
#' @export 
extractStats.optimize.portfolio.DEoptim <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.DEoptim")) stop("object must be of class optimize.portfolio.DEoptim")
  
  # Check if object$DEoptim_objective_results is null, the user called optimize.portfolio with trace=FALSE
  if(is.null(object$DEoptim_objective_results)) stop("DEoptim_objective_results is null, trace=TRUE must be specified in optimize.portfolio")
  
  # first pull out the optimal portfolio
  trow<-c(unlist(object$objective_measures),out=object$out,object$weights)
  #colnames(trow)<-c(colnames(unlist(object$objective_measures)),'out',names(object$weights))
  result<-trow
  l = length(object$DEoptim_objective_results)
  nobj<-length(unlist(object$DEoptim_objective_results[[1]]$objective_measures))
  result=matrix(nrow=l,ncol=(nobj+length(object$weights))+1)
  ncols<-ncol(result)
  
  for (i in 1:l) {
    if(!is.atomic(object$DEoptim_objective_results[[i]])) {
      result[i,1:nobj]<-unlist(object$DEoptim_objective_results[[i]]$objective_measures)
      result[i,(nobj+1)]<-object$DEoptim_objective_results[[i]]$out
      result[i,(nobj+2):ncols]<-object$DEoptim_objective_results[[i]]$weights
    }
  }
  
  rnames<-c(names(unlist(object$DEoptim_objective_results[[1]]$objective_measures)),'out',paste('w',names(object$weights),sep='.'))
  rnames<-name.replace(rnames)
  colnames(result)<-rnames
  rownames(result) = paste(prefix,"DE.portf", index(object$DEoptim_objective_results), sep=".")
  #rownames(result) = paste("DE.portf.", index(result), sep="")
  return(result)
}

#' @method extractStats optimize.portfolio.ROI
#' @S3method extractStats optimize.portfolio.ROI
#' @export 
extractStats.optimize.portfolio.ROI <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.ROI")) stop("object must be of class optimize.portfolio.ROI")
  trow <- c(object$out, object$weights)
  objmeas <- extractObjectiveMeasures(object)
  objnames <- names(objmeas)
  obj <- unlist(objmeas)
  result <- c(obj, trow)
  rnames<-c(objnames, 'out', paste('w', names(object$weights), sep='.'))
  #print(result)
  #print(rnames)
  names(result)<-rnames
  return(result)
}

#' @method extractStats optimize.portfolio.pso
#' @S3method extractStats optimize.portfolio.pso
#' @export 
extractStats.optimize.portfolio.pso <- function(object, prefix=NULL, ...){
  if(!inherits(object, "optimize.portfolio.pso")) stop("object must be of class optimize.portfolio.pso")
  
  # Check if object$PSOoutput is null, the user called optimize.portfolio with trace=FALSE
  if(is.null(object$PSOoutput)) stop("PSOoutput is null, trace=TRUE must be specified in optimize.portfolio")
  
  R <- object$R
  portfolio <- object$portfolio
  
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
  
  # get the constraints for min_sum and max_sum normalization
  constraints <- get_constraints(object$portfolio)
  
  # optimal portfolio
  # trow <- c(unlist(object$objective_measures), out=object$out, object$weights)
  
  # get the weights of each iteration from PSOoutput
  psoweights <- do.call(rbind, lapply(object$PSOoutput$stats$x, t))
  
  # need to normalize so that psoweights are valid portfolios
  psoweights <- t(apply(psoweights, 1, normalize_weights))
  
  # bind the optimal weights to psoweights
  psoweights <- rbind(object$weights, psoweights)
  
  # get swarm fitness values (i.e. out value of the function evaluated with the swarm positions)
  tmpout <- unlist(object$PSOoutput$stats$f)
  
  # combine the optimal out value to the vector of out values
  tmpout <- c(object$out, tmpout)
  
  # run constrained_objective on the weights to get the objective measures in a matrix
  stopifnot("package:foreach" %in% search() || suppressMessages(requireNamespace("foreach",quietly = TRUE)))
  i <- 1
  obj <- foreach::foreach(i=1:nrow(psoweights), .inorder=TRUE, .combine=rbind, .errorhandling='remove') %dopar% {
    unlist(constrained_objective(w=psoweights[i,], R=R, portfolio=portfolio, trace=TRUE)$objective_measures)
  }
  objnames <- name.replace(colnames(obj))
  result <- cbind(obj, tmpout, psoweights)
  colnames(result) <- c(objnames, "out", paste('w',names(object$weights),sep='.'))
  rownames(result) <- paste(prefix, "pso.portf", index(tmpout), sep=".")
  return(result)
}

#' @method extractStats optimize.portfolio.GenSA
#' @S3method extractStats optimize.portfolio.GenSA
#' @export 
extractStats.optimize.portfolio.GenSA <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.GenSA")) stop("object must be of class optimize.portfolio.GenSA")
  
  # Check if object$GenSAoutput is null, the user called optimize.portfolio with trace=FALSE
  if(is.null(object$GenSAoutput)) stop("GenSAoutput is null, trace=TRUE must be specified in optimize.portfolio")
  
  trow<-c(out=object$out, object$weights)
  obj <- unlist(object$objective_measures)
  result <- c(obj, trow)
  
  rnames <- name.replace(names(result))
  names(result) <- rnames
  return(result)
}

#' @method extractStats optimize.portfolio.invol
#' @S3method extractStats optimize.portfolio.invol
#' @export 
extractStats.optimize.portfolio.invol <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.invol")) stop("object must be of class optimize.portfolio.invol")
  trow<-c(out=object$out, object$weights)
  
  obj <- unlist(object$objective_measures)
  result <- c(obj, trow)
  
  rnames <- name.replace(names(result))
  names(result) <- rnames
  return(result)
}

#' @method extractStats optimize.portfolio.eqwt
#' @S3method extractStats optimize.portfolio.eqwt
#' @export 
extractStats.optimize.portfolio.eqwt <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.eqwt")) stop("object must be of class optimize.portfolio.eqwt")
  trow<-c(out=object$out, object$weights)
  
  obj <- unlist(object$objective_measures)
  result <- c(obj, trow)
  
  rnames <- name.replace(names(result))
  names(result) <- rnames
  return(result)
}

#' @method extractStats optimize.portfolio.rebalancing
#' @S3method extractStats optimize.portfolio.rebalancing
#' @export 
extractStats.optimize.portfolio.rebalancing <- function(object, prefix=NULL, ...) {
  if(!inherits(object, "optimize.portfolio.rebalancing")) stop("object must be of class optimize.portfolio.rebalancing")
  
  if(inherits(object$portfolio, "regime.portfolios")){
    return(extractStatsRegime(object, prefix=prefix))
  } else {
    return(lapply(object$opt_rebal, extractStats, ...))
  }
}

# Helper function for extractStats.optimize.portfolio.rebalancing
# with regime switching.
# If I have N different regimes and N different portfolios, then 
# extractStats should return a list of length N where each element
# contains the extractStats output for a given regime
extractStatsRegime <- function(object, prefix=NULL){
  tmp.regimes <- unlist(lapply(object$opt_rebalancing, function(x) x$regime))
  unique.regimes <- unique(tmp.regimes)
  
  # Initialize a list to hold the optimize.portfolio objects for each regime
  out.list <- vector("list", length(unique.regimes))
  names(out.list) <- paste("regime", 1:length(unique.regimes), sep=".")
  
  # Outer loop over each regime
  for(i in 1:length(unique.regimes)){
    # Get the index for each regime
    tmp.idx <- which(tmp.regimes == unique.regimes[i])
    
    # Initialize a temporary list to store the extractStats output for each 
    # unique regime
    tmp <- vector("list", length(tmp.idx))
    
    # Nested loop over each optimize.portfolio object of the corresponding regime
    for(j in 1:length(tmp)){
      tmp[[j]] <- extractStats(object$opt_rebalancing[[tmp.idx[j]]], prefix=prefix)
    }
    out.list[[i]] <- tmp
  }
  out.list
}

#' @method extractStats optimize.portfolio.parallel
#' @S3method extractStats optimize.portfolio.parallel
#' @export
extractStats.optimize.portfolio.parallel <- function(object,prefix=NULL,...) {
    resultlist<-object
    l = length(resultlist)
    result=NULL
    for (i in 1:l) {
        if(is.null(result)) result<-extractStats(resultlist[[i]])
        else result <- rbind(result,extractStats(resultlist[[i]]))
    }
    
    rownames(result) = paste("par", index(result), rownames(result), sep=".")
    return(result)
}

#' @method extractStats optimize.portfolio.random
#' @S3method extractStats optimize.portfolio.random
#' @export
extractStats.optimize.portfolio.random <- function(object, prefix=NULL, ...){
# This just flattens the $random_portfolio_objective_results part of the object
  if(!inherits(object, "optimize.portfolio.random")) stop("object must be of class optimize.portfolio.random")
  
  # Check if object$random_portfolio_objective_results is null, the user called optimize.portfolio with trace=FALSE
  if(is.null(object$random_portfolio_objective_results)) stop("random_portfolio_objective_results is null, trace=TRUE must be specified in optimize.portfolio")
  
  OptimResults<-object  

  l = length(OptimResults$random_portfolio_objective_results)
  nobj<-length(unlist(OptimResults$random_portfolio_objective_results[[1]]$objective_measures))
  result=matrix(nrow=l,ncol=(nobj+length(OptimResults$weights))+1)
  ncols<-ncol(result)
  
  for (i in 1:l) {
      if(!is.atomic(OptimResults$random_portfolio_objective_results[[i]])) {
          result[i,1:nobj]<-unlist(OptimResults$random_portfolio_objective_results[[i]]$objective_measures)
          result[i,(nobj+1)]<-OptimResults$random_portfolio_objective_results[[i]]$out
          result[i,(nobj+2):ncols]<-OptimResults$random_portfolio_objective_results[[i]]$weights
      }
  }
  
  rnames<-c(names(unlist(OptimResults$random_portfolio_objective_results[[1]]$objective_measures)),'out',paste('w',names(OptimResults$weights),sep='.'))
  rnames<-name.replace(rnames)
  colnames(result)<-rnames
  rownames(result) = paste(prefix,"rnd.portf", index(OptimResults$random_portfolio_objective_results), sep=".")

  return(result)
}

#' @method extractStats opt.list
#' @S3method extractStats opt.list
#' @export
extractStats.opt.list <- function(object, ...){
  # get the stats of each optimization in a list
  # each element in the list is an optimize.portfolio object
  stats_list <- vector("list", length(object))
  for(i in 1:length(stats_list)){
    stats_list[[i]] <- extractStats(object[[i]])
  }
  return(stats_list)
}

#' @method extractStats opt.rebal.list
#' @S3method extractStats opt.rebal.list
#' @export
extractStats.opt.rebal.list <- function(object, ...){
  # get the stats of each optimization in a list
  # each element in the list is an optimize.portfolio.rebalancing object
  stats_list <- vector("list", length(object))
  for(i in 1:length(stats_list)){
    stats_list[[i]] <- extractStats(object[[i]])
  }
  return(stats_list)
}

##### extractWeights #####

#' Extract weights from a portfolio run via \code{optimize.portfolio} or \code{optimize.portfolio.rebalancing}
#' 
#' This function will dispatch to the appropriate class handler based on the
#' input class of the optimize.portfolio or optimize.portfolio.rebalancing output object
#'  
#' @param object list returned by optimize.portfolio
#' @param \dots any other passthru parameters
#' @seealso \code{\link{optimize.portfolio}}, \code{\link{optimize.portfolio.rebalancing}}
#' @export
extractWeights <- function (object, ...){
  UseMethod('extractWeights')
}

#' @method extractWeights optimize.portfolio
#' @S3method extractWeights optimize.portfolio
#' @export
extractWeights.optimize.portfolio <- function(object, ...){
  if(!inherits(object, "optimize.portfolio")){
    stop("object must be of class 'optimize.portfolio'")
  }
  return(object$weights)
}

#' @method extractWeights optimize.portfolio.rebalancing
#' @S3method extractWeights optimize.portfolio.rebalancing
#' @export
extractWeights.optimize.portfolio.rebalancing <- function(object, ...){
  if(!inherits(object, "optimize.portfolio.rebalancing")){
    stop("Object passed in must be of class 'optimize.portfolio.rebalancing'")
  }
  rebal_object <- object$opt_rebal
  numColumns = length(rebal_object[[1]]$weights)
  numRows = length(rebal_object)

  result <- matrix(nrow=numRows, ncol=numColumns)

  for(i in 1:numRows)
    result[i,] = unlist(rebal_object[[i]]$weights)

  colnames(result) = names(unlist(rebal_object[[1]]$weights))
  rownames(result) = names(rebal_object)
  result = as.xts(result, dateFormat="Date")
  return(result)
}

#' @method extractWeights summary.optimize.portfolio.rebalancing
#' @S3method extractWeights summary.optimize.portfolio.rebalancing
#' @export
extractWeights.summary.optimize.portfolio.rebalancing <- function(object, ...){
  object$weights
}

#' @method extractWeights opt.list
#' @S3method extractWeights opt.list
#' @export
extractWeights.opt.list <- function(object, ...){
  # get the optimal weights of each optimization in a list
  weights_list <- list()
  for(i in 1:length(object)){
    weights_list[[i]] <- object[[i]]$weights
  }
  
  # get/set the names in the object
  opt_names <- names(object)
  if(is.null(opt_names)) opt_names <- paste("opt", 1:length(object))
  
  # get the names of each element in weights_list
  weights_names <- unlist(lapply(weights_list, names))
  
  # unique names in weights_names
  names_unique <- unique(weights_names)
  
  # create a matrix of zeros to fill in with weights later
  weights_mat <- matrix(0, nrow=length(weights_list), ncol=length(names_unique), 
                        dimnames=list(opt_names, names_unique))
  for(i in 1:length(weights_list)){
    pm <- pmatch(x=names(weights_list[[i]]), table=names_unique)
    weights_mat[i, pm] <- weights_list[[i]]
  }
  return(weights_mat)
}

#' @method extractWeights opt.rebal.list
#' @S3method extractWeights opt.rebal.list
#' @export
extractWeights.opt.rebal.list <- function(object, ...){
  # get the optimal weights of each optimization in a list
  # each element in the list is an optimize.portfolio.rebalancing object
  weights_list <- vector("list", length(object))
  for(i in 1:length(weights_list)){
    weights_list[[i]] <- extractWeights(object[[i]])
  }
  return(weights_list)
}

##### extractObjectiveMeasures #####

#' Extract the objective measures
#' 
#' This function will extract the objective measures from the optimal portfolio
#' run via \code{optimize.portfolio}
#'  
#' @param object list returned by optimize.portfolio
#' @return list of objective measures
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @export
extractObjectiveMeasures <- function(object){
  UseMethod("extractObjectiveMeasures")
}

#' @method extractObjectiveMeasures optimize.portfolio
#' @S3method extractObjectiveMeasures optimize.portfolio
extractObjectiveMeasures.optimize.portfolio <- function(object){
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class 'optimize.portfolio'")
  # objective measures returned as $objective_measures for all other solvers
  out <- object$objective_measures
  return(out)
}

#' @method extractObjectiveMeasures optimize.portfolio.rebalancing
#' @S3method extractObjectiveMeasures optimize.portfolio.rebalancing
extractObjectiveMeasures.optimize.portfolio.rebalancing <- function(object){
  if(!inherits(object, "optimize.portfolio.rebalancing")) stop("object must be of class 'optimize.portfolio.rebalancing'")
  
  if(inherits(object$portfolio, "regime.portfolios")){
    result <- extractObjRegime(object)
  } else {
    rebal_object <- object$opt_rebal
    num.columns <- length(unlist(extractObjectiveMeasures(rebal_object[[1]])))
    num.rows <- length(rebal_object)
    result <- matrix(nrow=num.rows, ncol=num.columns)
    for(i in 1:num.rows){
      result[i,] <- unlist(extractObjectiveMeasures(rebal_object[[i]]))
    }
    colnames(result) <- name.replace(names(unlist(extractObjectiveMeasures(rebal_object[[1]]))))
    rownames(result) <- names(rebal_object)
    result <- as.xts(result)
  }
  return(result)
}

# Helper function for extractObjectiveMeasures.optimize.portfolio.rebalancing
# with regime switching.
# If I have N different regimes and N different portfolios, then 
# extractObjectiveMeasures should return a list of length N where each element
# contains the objective measures for a given regime
extractObjRegime <- function(object){
  tmp.regimes <- unlist(lapply(object$opt_rebalancing, function(x) x$regime))
  unique.regimes <- sort(unique(tmp.regimes))
  #print(tmp.regimes)
  #print(unique.regimes)
  
  # Initialize a list to hold the objective measures for each regime
  out.list <- vector("list", length(unique.regimes))
  names(out.list) <- paste("regime", unique.regimes, sep=".")
  
  # Outer loop over each regime
  for(i in 1:length(unique.regimes)){
    # Get the index for each regime
    tmp.idx <- which(tmp.regimes == unique.regimes[i])
    
    # Initialize a temporary list to store the objective measures for each 
    # unique regime
    tmp <- vector("list", length(tmp.idx))
    
    # Nested loop over each optimize.portfolio object of the corresponding regime
    for(j in 1:length(tmp)){
      tmp[[j]] <- unlist(object$opt_rebalancing[[tmp.idx[j]]]$objective_measures)
    }
    # rbind the objective measures and convert to an xts object
    #obj <- xts(do.call(rbind, tmp), as.Date(names(tmp.idx)))
    obj <- do.call(rbind, tmp)
    colnames(obj) <- name.replace(colnames(obj))
    obj <- xts(obj, as.Date(names(tmp.idx)))
    # insert the objective measures into the list
    out.list[[unique.regimes[i]]] <- obj
  }
  out.list
}

#' @method extractObjectiveMeasures summary.optimize.portfolio.rebalancing
#' @S3method extractObjectiveMeasures summary.optimize.portfolio.rebalancing
extractObjectiveMeasures.summary.optimize.portfolio.rebalancing <- function(object){
  object$objective_measures
}

#' @method extractObjectiveMeasures opt.list
#' @S3method extractObjectiveMeasures opt.list
extractObjectiveMeasures.opt.list <- function(object){
  # The idea is that these portfolios opt.list may have different objectives.
  # Need a function to evaluate *all* objective measures for each portfolio.
  # Challenges:
  # - allow for different R objects across portfolios
  #    - Done
  # - detect and remove duplicate objectives
  #    - Done based on name and objective type
  # - handle duplicate objective names, but different arguments (i.e. different p for ES)
  #    - TODO
  # - risk budget objectives need to be entered last
  #    - Done
  if(!inherits(object, "opt.list")) stop("object must be of class 'opt.list'")
  # Get the names of the list
  opt.names <- names(object)
  if(is.null(opt.names)) opt.names <- paste("portfolio", 1:length(object))
  
  # Use the objectives from the first element and use as the basis for comparison
  base <- sapply(object[[1]]$portfolio$objectives, function(x) paste(class(x)[1], x$name, sep="."))
  
  # Get the objective name and type from each portfolio
  obj_list <- lapply(object, function(x) sapply(x$portfolio$objectives, function(u) paste(class(u)[1], u$name, sep=".")))
  
  # If all the objective names are identical, simply extract the objective measures
  # and build the objective_measures matrix
  if(all(sapply(obj_list, function(u) identical(x=base, y=u)))){
    obj_list <- list()
    # Get the objective_measures from each element
    for(i in 1:length(object)){
      tmp <- unlist(object[[i]]$objective_measures)
      names(tmp) <- name.replace(names(tmp))
      obj_list[[opt.names[i]]] <- tmp
    }
    obj_names <- unique(unlist(lapply(obj_list, names)))
    obj_mat <- matrix(NA, nrow=length(obj_list), ncol=length(obj_names),
                      dimnames=list(opt.names, obj_names))
    for(i in 1:length(obj_list)){
      pm <- pmatch(x=names(obj_list[[i]]), table=obj_names)
      obj_mat[i, pm] <- obj_list[[i]]
    }
    out <- obj_mat
  } else {
    # The objectives across portfolios are not identical, we will build an
    # objectives list with *all* the objectives and recalculate the objective_measures
    
    # Initialize a tmp.obj list to store all of the objectives from each 
    tmp.obj <- list()
    tmp.budget <- list()
    
    # Step 1: Loop through object and get the objectives from each portfolio
    for(i in 1:length(object)){
      tmp.portf <- object[[i]]$portfolio
      for(j in 1:length(tmp.portf$objectives)){
        if(inherits(tmp.portf$objectives[[j]], "risk_budget_objective")){
          # tmp.budget <- c(tmp.budget, tmp.portf$objectives[[j]])
          num.budget <- length(tmp.budget) + 1
          tmp.budget[[num.budget]] <- tmp.portf$objectives[[j]]
        } else {
          # tmp.obj <- c(tmp.obj, tmp.portf$objectives[[j]])
          num.obj <- length(tmp.obj) + 1
          tmp.obj[[num.obj]] <- tmp.portf$objectives[[j]]
        }
      } # end inner loop of objectives
    } # end outer loop of object
    
    # This will make sure that "risk_budget_objectives" are entered last, but doesn't
    # address duplicate names with different arguments in the arguments list
    # e.g. different arguments for p, clean, etc.
    tmp.obj <- c(tmp.obj, tmp.budget)
    
    # Remove any duplicates
    # The last objective will be the one that is kept
    out.obj <- list()
    obj.names <- sapply(tmp.obj, function(x) paste(x$name, class(x)[1], sep="."))
    if(any(duplicated(obj.names))){
      idx <- which(!duplicated(obj.names, fromLast=TRUE))
      for(i in 1:length(idx)){
        out.obj[[i]] <- tmp.obj[[idx[i]]]
      }
    }
    
    # Loop through object and insert the new objectives list into each portfolio
    # and run constrained_objective on each portfolio to extract the 
    # objective_measures for each portfolio
    out <- list()
    for(i in 1:length(object)){
      object[[i]]$portfolio$objectives <- tmp.obj
      tmp.weights <- object[[i]]$weights
      tmp.R <- object[[i]]$R
      tmp.portf <- object[[i]]$portfolio
      tmp <- unlist(constrained_objective(w=tmp.weights, R=tmp.R, portfolio=tmp.portf, trace=TRUE)$objective_measures)
      names(tmp) <- name.replace(names(tmp))
      out[[opt.names[i]]] <- tmp
    }
    out <- do.call(rbind, out)
  }
  return(out)
}

#' @method extractObjectiveMeasures opt.rebal.list
#' @S3method extractObjectiveMeasures opt.rebal.list
#' @export
extractObjectiveMeasures.opt.rebal.list <- function(object, ...){
  # get the optimal weights of each optimization in a list
  # each element in the list is an optimize.portfolio.rebalancing object
  obj_list <- vector("list", length(object))
  for(i in 1:length(obj_list)){
    obj_list[[i]] <- extractObjectiveMeasures(object[[i]])
  }
  return(obj_list)
}

##### extractGroups #####

#' Extract the group and/or category weights
#' 
#' This function extracts the weights by group and/or category from an object
#' of class \code{optimize.portfolio}. Group constraints or category_labels must
#' be specified for this to return group constraints.
#' 
#' @param object object of class \code{optimize.portfolio}
#' @param ... passthrough parameters. Not currently used
#' @return a list with two elements
#' \itemize{
#'   \item{weights: }{Optimal set of weights from the \code{optimize.portfolio} object}
#'   \item{category_weights: }{Weights by category if category_labels are supplied in the \code{portfolio} object}
#'   \item{group_weights: }{Weights by group if group is a constraint type}
#' }
#' @author Ross Bennett
#' @export
extractGroups <- function(object, ...){
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class 'optimize.portfolio'")
  
  # Check category_labels in portfolio object
  category_labels <- object$portfolio$category_labels
  
  # Get the constraints to check for group constraints
  constraints <- get_constraints(object$portfolio)
  
  groups <- constraints$groups
  
  cat_weights <- NULL
  group_weights <- NULL
  
  if(!is.null(category_labels)){
    cat_names <- names(category_labels)
    ncats <- length(category_labels)
    cat_weights <- rep(0, ncats)
    for(i in 1:ncats){
      cat_weights[i] <- sum(object$weights[category_labels[[i]]])
    }
    names(cat_weights) <- cat_names
  }
  
  if(!is.null(groups)){
    n.groups <- length(groups)
    group_weights <- rep(0, n.groups)
    for(i in 1:n.groups){
      group_weights[i] <- sum(object$weights[groups[[i]]])
    }
    names(group_weights) <- constraints$group_labels
  }
  return(list(weights=object$weights, 
              category_weights=cat_weights, 
              group_weights=group_weights)
  )
}

