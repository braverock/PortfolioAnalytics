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

#' summary method for optimize.portfolio.rebalancing
#' @param object object of type optimize.portfolio.rebalancing
#' @param \dots any other passthru parameters
#' @export
summary.optimize.portfolio.rebalancing <- function(object, ...) {
    if(!inherits(object,"optimize.portfolio.rebalancing")) 
        stop ("passed object is not of class optimize.portfolio.rebalancing")
    
    
    # loop through and show the results and weights
    cat('Weights:\n')
    for(i in 1:length(object)){
        cat(names(object[i]))
        cat('\n')
        if(!inherits(object[i],'try-error')){
            print(round(object[[i]]$weights,4))
        } else {
            print(object[i])
        }
    }
    cat('Objective Measures\n')
    for(i in 1:length(object)){
        if(!inherits(object[i],'try-error')){
            cat(names(object[i]))
            cat('\n')
            print(object[[i]]$constrained_objective)
        }
    }    
}

#' Summary method for objects of class 'portfolio'
#' 
#' @param portfolio object of class portfolio
#' @author Ross Bennett
#' @export
summary.portfolio <- function(portfolio){
  if(!is.portfolio(portfolio)) stop("object passed in is not of class 'portfolio'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Portfolio Specification", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  # Assets
  cat("\nAssets\n")
  nassets <- length(portfolio$assets)
  cat("Number of assets:", nassets, "\n")
  
  # Constraints
  cat("\nConstraints\n")
  nconstraints <- length(portfolio$constraints)
  # logical vector of enabled constraints
  enabled.constraints <- sapply(pspec$constraints, function(x) x$enabled)
  # character vector of constraint types
  names.constraints <- sapply(pspec$constraints, function(x) x$type)
  cat("Number of constraints:", nconstraints, "\n")
  cat("Number of enabled constraints:", sum(enabled.constraints), "\n")
  if(sum(enabled.constraints) > 0){
    cat("Enabled constraint types\n")
    for(type in names.constraints[enabled.constraints]) {
      cat("\t\t-", type, "\n")
    }
  }
  cat("Number of disabled constraints:", nconstraints - sum(enabled.constraints), "\n")
  if((nconstraints - sum(enabled.constraints)) > 0){
    cat("Disabled constraint types\n")
    for(type in setdiff(names.constraints, names.constraints[enabled.constraints])) {
      cat("\t\t-", type, "\n")
    }
  }
  
  # Objectives
  cat("\nObjectives\n")
  nobjectives <- length(portfolio$objectives)
  # logical vector of enabled objectives
  enabled.objectives <- sapply(pspec$objectives, function(x) x$enabled)
  # character vector of objective names
  names.objectives <- sapply(pspec$objectives, function(x) x$name)
  cat("Number of objectives:", nobjectives, "\n")
  cat("Number of enabled objectives:", sum(enabled.objectives), "\n")
  if(sum(enabled.objectives) > 0){
    cat("Enabled objective names\n")
    for(name in names.objectives[enabled.objectives]) {
      cat("\t\t-", name, "\n")
    }
  }
  cat("Number of disabled objectives:", nobjectives - sum(enabled.objectives), "\n")
  if((nobjectives - sum(enabled.objectives)) > 0){
    cat("Disabled objective types\n")
    for(name in setdiff(names.objectives, names.objectives[enabled.objectives])) {
      cat("\t\t-", name, "\n")
    }
  }
  cat("\n")
}

#' print method for objects of class 'constraint'
#' 
#' @param portfolio object of class constraint
#' @author Ross Bennett
#' @export
print.constraint <- function(obj){
  print.default(obj)
}

#' Printing Output of optimize.portfolio
#' 
#' print method for optimize.portfolio.ROI
#' 
#' @param object an object of class "optimize.portfolio.ROI" resulting from a call to optimize.portfolio
#' @param digits the number of significant digits to use when printing.
#' @param ... any other passthru parameters
#' @export
print.optimize.portfolio.ROI <- function(object, digits = max(3, getOption("digits") - 3), ...){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights, digits=digits)
  cat("\n")
  
  # get objective measure
  cat("Objective Measure:\n")
  print(as.numeric(object$out), digits=digits)
  cat("\n")
}

#' Printing Output of optimize.portfolio
#' 
#' print method for optimize.portfolio.random
#' 
#' @param object an object of class "optimize.portfolio.random" resulting from a call to optimize.portfolio
#' @param digits the number of significant digits to use when printing.
#' @param ... any other passthru parameters
#' @export
print.optimize.portfolio.random <- function(object, digits=max(3, getOption("digits")-3), ...){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights, digits=digits)
  cat("\n")
  
  # get objective measure
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(tmp_obj)){
    print(tmp_obj[i], digits=digits)
    cat("\n")
  }
  cat("\n")
}

#' Printing Output of optimize.portfolio
#' 
#' print method for optimize.portfolio.DEoptim
#' 
#' @param object an object of class "optimize.portfolio.DEoptim" resulting from a call to optimize.portfolio
#' @param digits the number of significant digits to use when printing.
#' @param ... any other passthru parameters
#' @export
print.optimize.portfolio.DEoptim <- function(object, digits=max(3, getOption("digits")-3), ...){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights, digits=digits)
  cat("\n")
  
  # get objective measure
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(tmp_obj)){
    print(tmp_obj[i], digits=digits)
    cat("\n")
  }
  cat("\n")
}

#' Printing Output of optimize.portfolio
#' 
#' print method for optimize.portfolio.GenSA
#' 
#' @param object an object of class "optimize.portfolio.GenSA" resulting from a call to optimize.portfolio
#' @param digits the number of significant digits to use when printing
#' @param ... any other passthru parameters
#' @export
print.optimize.portfolio.GenSA <- function(object, digits=max(3, getOption("digits")-3), ...){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights, digits=digits)
  cat("\n")
  
  # get objective measure
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(tmp_obj)){
    print(tmp_obj[i], digits=digits)
    cat("\n")
  }
  cat("\n")
}

#' Printing Output of optimize.portfolio
#' 
#' print method for optimize.portfolio.pso
#' 
#' @param object an object of class "optimize.portfolio.pso" resulting from a call to optimize.portfolio
#' @param digits the number of significant digits to use when printing.
#' @param ... any other passthru parameters
#' @export
print.optimize.portfolio.pso <- function(object, digits=max(3, getOption("digits")-3), ...){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights, digits=digits)
  cat("\n")
  
  # get objective measure
  # get objective measure
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(tmp_obj)){
    print(tmp_obj[i], digits=digits)
    cat("\n")
  }
  cat("\n")
}

#' Summarizing Output of optimize.portfolio
#' 
#' summary method for class "optimize.portfolio"
#' 
#' @param object an object of class "optimize.portfolio.pso" resulting from a call to optimize.portfolio
#' @param ... any other passthru parameters. Currently not used.
#' @export
summary.optimize.portfolio <- function(object, ...){
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization Summary", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  # show the call to optimize.portfolio
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(object$weights)
  cat("\n")
  
  # objective measures
  # The objective measure is object$out for ROI
  cat("Objective Measures:\n")
  if(!is.null(object$objective_measures)){
    # get objective measure
    objective_measures <- object$objective_measures
    tmp_obj <- as.numeric(unlist(objective_measures))
    names(tmp_obj) <- names(objective_measures)
    for(i in 1:length(tmp_obj)){
      print(tmp_obj[i])
      cat("\n")
    }
  } else {
    print(as.numeric(object$out))
  }
  cat("\n")
  
  # get seed portfolio
  cat("Portfolio Assets and Seed Weights:\n")
  print.default(object$portfolio$assets)
  cat("\n")
  
  # summary of the portfolio object
  summary(object$portfolio)
  
  # Constraints
  cat(rep("*", 40), "\n", sep="")
  cat("Constraints\n")
  cat(rep("*", 40), "\n", sep="")
  
  # get the constraints
  constraints <- get_constraints(object$portfolio)
  
  # leverage constraints
  cat("Leverage Constraint:\n")
  if(!is.null(constraints$min_sum) & !is.null(constraints$max_sum)){
    cat("min_sum = ", constraints$min_sum, "\n", sep="")
    cat("max_sum = ", constraints$max_sum, "\n", sep="")
    cat("\n")
  }
  
  # box constraints
  cat("Box Constraints:\n")
  if(!is.null(constraints$min) & !is.null(constraints$max)){
    cat("min:\n")
    print(constraints$min)
    cat("max:\n")
    print(constraints$max)
    cat("\n")
  }
  
  # group constraints
  cat("Group Constraints:\n")
  if(!is.null(constraints$groups) & !is.null(constraints$cLO) & !is.null(constraints$cUP)){
    cat("Groups:\n")
    groups <- constraints$groups
    group_labels <- constraints$group_labels
    names(groups) <- group_labels
    print(groups)
    cat("\n")
    cat("Lower bound on group weights, group_min:\n")
    cLO <- constraints$cLO
    names(cLO) <- group_labels
    print(cLO)
    cat("\n")
    cat("Upper bound on group weights, group_max:\n")
    cUP <- constraints$cUP
    names(cUP) <- group_labels
    print(cUP)
    cat("\n")
    cat("Group position limits, group_pos:\n")
    group_pos <- constraints$group_pos
    if(!is.null(group_pos)) names(group_pos) <- group_labels
    print(group_pos)
    cat("\n")
    
    cat("Group Weights:\n")
    n.groups <- length(groups)
    group_weights <- rep(0, n.groups)
    k <- 1
    l <- 0
    for(i in 1:n.groups){
      j <- groups[i]
      group_weights[i] <- sum(object$weights[k:(l+j)])
      k <- k + j
      l <- k - 1
    }
    names(group_weights) <- group_labels
    print(group_weights)
    cat("\n")
  }
  tolerance <- .Machine$double.eps^0.5
  
  # position limit constraints
  cat("Position Limit Constraints:\n")
  cat("Maximum number of non-zero weights, max_pos:\n")
  print(constraints$max_pos)
  cat("Realized number of non-zero weights (i.e. positions):\n")
  print(sum(abs(object$weights) > tolerance))
  cat("\n")
  
  cat("Maximum number of long positions, max_pos_long:\n")
  print(constraints$max_pos_long)
  cat("Realized number of long positions:\n")
  print(sum(object$weights > tolerance))
  cat("\n")
  
  cat("Maximum number of short positions, max_pos_short:\n")
  print(constraints$max_pos_short)
  cat("Realized number of short positions:\n")
  print(sum(object$weights < -tolerance))
  cat("\n\n")
  
  # diversification
  cat("Diversification Target Constraint:\n")
  print(constraints$div_target)
  cat("\n")
  cat("Realized diversification:\n")
  print(diversification(object$weights))
  cat("\n")
  
  # turnover
  cat("Turnover Target Constraint:\n")
  print(constraints$turnover_target)
  cat("\n")
  cat("Realized turnover:\n")
  print(turnover(object$weights, wts.init=object$portfolio$assets))
  cat("\n")
  
  # Objectives
  cat(rep("*", 40), "\n", sep="")
  cat("Objectives\n")
  cat(rep("*", 40), "\n\n", sep="")
  
  for(obj in object$portfolio$objectives){
    cat("Objective:", class(obj)[1], "\n")
    print(obj)
    cat("\n", rep("*", 40), "\n", sep="")
  }
  cat("\n")
  
  # show the elapsed time for the optimization
  cat("Elapsed Time:\n")
  print(object$elapsed_time)
  cat("\n")
}
