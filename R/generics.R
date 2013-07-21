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
  print.default(object$out, digits=digits)
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
  cat("Objective Measures:\n")
  for(obj in object$objective_measures){
    print.default(obj, digits=digits)
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
  cat("Objective Measures:\n")
  for(obj in object$objective_measures){
    print.default(obj, digits=digits)
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
  cat("Objective Measures:\n")
  for(obj in object$objective_measures){
    print.default(obj, digits=digits)
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
  cat("Objective Measures:\n")
  for(obj in object$objective_measures){
    print.default(obj, digits=digits)
    cat("\n")
  }
  cat("\n")
}

