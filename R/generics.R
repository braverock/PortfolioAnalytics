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

#' Printing Portfolio Specification Objects
#' 
#' Print method for class "portfolio"
#' 
#' @param portfolio object of class portfolio
#' @author Ross Bennett
#' @export
print.portfolio <- function(portfolio){
  if(!is.portfolio(portfolio)) stop("object passed in is not of class 'portfolio'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Portfolio Specification", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(portfolio$call), sep = "\n", collapse = "\n"), 
      "\n", sep = "")
  
  # Assets
  cat("\nAssets\n")
  nassets <- length(portfolio$assets)
  cat("Number of assets:", nassets, "\n\n")
  cat("Asset Names\n")
  print(head(names(portfolio$assets), 10))
  if(nassets > 10){
    cat("More than 10 assets, only printing the first 10\n")
  }
  
  # Constraints
  cat("\nConstraints\n")
  nconstraints <- length(portfolio$constraints)
  if(nconstraints > 0){
    # logical vector of enabled constraints
    enabled.constraints <- which(sapply(portfolio$constraints, function(x) x$enabled))
    n.enabled.constraints <- ifelse(length(enabled.constraints) > 0, length(enabled.constraints), 0)
  } else {
    enabled.constraints <- NULL
    n.enabled.constraints <- 0
  }
  # character vector of constraint types
  names.constraints <- sapply(portfolio$constraints, function(x) x$type)
  cat("Number of constraints:", nconstraints, "\n")
  cat("Number of enabled constraints:", n.enabled.constraints, "\n")
  if(length(enabled.constraints) > 0){
    cat("Enabled constraint types\n")
    constraints <- portfolio$constraints
    nconstraints <- length(constraints)
    for(i in 1:nconstraints){
      if(constraints[[i]]$enabled){
        type <- constraints[[i]]$type
        if(type == "box"){
          # long only
          if(all(constraints[[i]]$min == 0) & all(constraints[[i]]$max == 1)){
            cat("\t\t-", "box (long only)", "\n")
          } else if(all(constraints[[i]]$min == -Inf) & all(constraints[[i]]$max == Inf)){
            # unconstrained
            cat("\t\t-", "box (unconstrained)", "\n")
          } else if(any(constraints[[i]]$min < 0)){
            # with shorting
            cat("\t\t-", "box (with shorting)", "\n")
          } else {
            cat("\t\t-", type, "\n")
          }
        } else {
          cat("\t\t-", type, "\n")
        }
      }
    }
  }
  cat("Number of disabled constraints:", nconstraints - n.enabled.constraints, "\n")
  if((nconstraints - n.enabled.constraints) > 0){
    cat("Disabled constraint types\n")
    constraints <- portfolio$constraints
    nconstraints <- length(constraints)
    for(i in 1:nconstraints){
      if(!constraints[[i]]$enabled){
        type <- constraints[[i]]$type
        if(type == "box"){
          # long only
          if(all(constraints[[i]]$min == 0) & all(constraints[[i]]$max == 1)){
            cat("\t\t-", "box (long only)", "\n")
          } else if(all(constraints[[i]]$min == -Inf) & all(constraints[[i]]$max == Inf)){
            # unconstrained
            cat("\t\t-", "box (unconstrained)", "\n")
          } else if(any(constraints[[i]]$min < 0)){
            # with shorting
            cat("\t\t-", "box (with shorting)", "\n")
          } else {
            cat("\t\t-", type, "\n")
          }
        } else {
          cat("\t\t-", type, "\n")
        }
      }
    }
  }
  
  # Objectives
  cat("\nObjectives\n")
  nobjectives <- length(portfolio$objectives)
  if(nobjectives > 0){
    # logical vector of enabled objectives
    enabled.objectives <- which(sapply(portfolio$objectives, function(x) x$enabled))
    n.enabled.objectives <- ifelse(length(enabled.objectives) > 0, length(enabled.objectives), 0)
  } else {
    enabled.objectives <- NULL
    n.enabled.objectives <- 0
  }
  # character vector of objective names
  names.objectives <- sapply(portfolio$objectives, function(x) x$name)
  cat("Number of objectives:", nobjectives, "\n")
  cat("Number of enabled objectives:", n.enabled.objectives, "\n")
  if(n.enabled.objectives > 0){
    cat("Enabled objective names\n")
    for(name in names.objectives[enabled.objectives]) {
      cat("\t\t-", name, "\n")
    }
  }
  cat("Number of disabled objectives:", nobjectives - n.enabled.objectives, "\n")
  if((nobjectives - n.enabled.objectives) > 0){
    cat("Disabled objective types\n")
    for(name in setdiff(names.objectives, names.objectives[enabled.objectives])) {
      cat("\t\t-", name, "\n")
    }
  }
  cat("\n")
}

#' Summarizing Portfolio Specification Objects
#' 
#' summary method for class "portfolio"
#' 
#' @param portfolio object of class portfolio
#' @author Ross Bennett
#' @export
summary.portfolio <- function(portfolio){
  if(!is.portfolio(portfolio)) stop("object passed in is not of class 'portfolio'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Portfolio Specification Summary", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("Assets and Seed Weights:\n")
  print(portfolio$assets)
  cat("\n")
  
  if(!is.null(portfolio$category_labels)) {
    cat("Category Labels:\n")
    print(portfolio$category_labels)
  }
  
  if(!is.null(portfolio$weight_seq)) {
    cat("weight_seq:\n")
    print(summary(portfolio$weight_seq))
  }
  
  cat("Constraints:\n\n")
  for(constraint in portfolio$constraints){
    if(constraint$enabled) {
      cat(rep("*", 40), "\n", sep="")
      cat(constraint$type, "constraint\n")
      cat(rep("*", 40), "\n", sep="")
      print(constraint)
      cat("\n\n")
    }
  }
  
  cat("Objectives:\n\n")
  for(objective in portfolio$objectives){
    if(objective$enabled) {
      cat(rep("*", 40), "\n", sep="")
      cat(class(objective)[1], "\n")
      cat(rep("*", 40), "\n", sep="")
      print(objective)
      cat("\n\n")
    }
  }
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
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measure:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=4)
    cat("\n")
  }
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
  
  # get objective measures
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=4)
    cat("\n")
    if(length(objective_measures[[i]]) > 1){
      # This will be the case for any objective measures with risk budgets
      for(j in 2:length(objective_measures[[i]])){
        tmpl <- objective_measures[[i]][j]
        cat(names(tmpl), ":\n")
        tmpv <- unlist(tmpl)
        names(tmpv) <- names(object$weights)
        print(tmpv)
        cat("\n")
      }
    }
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
  
  # get objective measures
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=4)
    cat("\n")
    if(length(objective_measures[[i]]) > 1){
      # This will be the case for any objective measures with risk budgets
      for(j in 2:length(objective_measures[[i]])){
        tmpl <- objective_measures[[i]][j]
        cat(names(tmpl), ":\n")
        tmpv <- unlist(tmpl)
        names(tmpv) <- names(object$weights)
        print(tmpv)
        cat("\n")
      }
    }
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
  
  # get objective measures
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=4)
    cat("\n")
    if(length(objective_measures[[i]]) > 1){
      # This will be the case for any objective measures with risk budgets
      for(j in 2:length(objective_measures[[i]])){
        tmpl <- objective_measures[[i]][j]
        cat(names(tmpl), ":\n")
        tmpv <- unlist(tmpl)
        names(tmpv) <- names(object$weights)
        print(tmpv)
        cat("\n")
      }
    }
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
  
  # get objective measures
  objective_measures <- object$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measures:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=4)
    cat("\n")
    if(length(objective_measures[[i]]) > 1){
      # This will be the case for any objective measures with risk budgets
      for(j in 2:length(objective_measures[[i]])){
        tmpl <- objective_measures[[i]][j]
        cat(names(tmpl), ":\n")
        tmpv <- unlist(tmpl)
        names(tmpv) <- names(object$weights)
        print(tmpv)
        cat("\n")
      }
    }
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
#' @author Ross Bennett
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
    # get objective measures
    objective_measures <- object$objective_measures
    tmp_obj <- as.numeric(unlist(objective_measures))
    names(tmp_obj) <- names(objective_measures)
    for(i in 1:length(objective_measures)){
      print(tmp_obj[i], digits=4)
      cat("\n")
      if(length(objective_measures[[i]]) > 1){
        # This will be the case for any objective measures with risk budgets
        for(j in 2:length(objective_measures[[i]])){
          tmpl <- objective_measures[[i]][j]
          cat(names(tmpl), ":\n")
          tmpv <- unlist(tmpl)
          names(tmpv) <- names(object$weights)
          print(tmpv)
          cat("\n")
        }
      }
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
  
  # print the portfolio object
  print(object$portfolio)
  
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
  group_weights <- NULL
  if(!is.null(constraints$groups) & !is.null(constraints$cLO) & !is.null(constraints$cUP)){
    cat("Group Constraints:\n")
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
    for(i in 1:n.groups){
      group_weights[i] <- sum(object$weights[groups[[i]]])
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
  cat("Realized turnover from seed weights:\n")
  print(turnover(object$weights, wts.init=object$portfolio$assets))
  cat("\n")
  
  # Factor exposure constraint
  tmpexp <- NULL
  if(!is.null(constraints$B) & !is.null(constraints$lower) & !is.null(constraints$upper)){
    cat("Factor Exposure Constraints:\n")
    t.B <- t(constraints$B)
    cat("Factor Exposure B Matrix:\n")
    print(constraints$B)
    cat("\n")
    cat("Lower bound on factor exposures, lower:\n")
    lower <- constraints$lower
    names(lower) <- colnames(constraints$B)
    print(lower)
    cat("\n")
    cat("Upper bound on group weights, group_max:\n")
    upper <- constraints$upper
    names(upper) <- colnames(constraints$B)
    print(upper)
    cat("\n")
    cat("Realized Factor Exposures:\n")
    tmpexp <- vector(mode="numeric", length=nrow(t.B))
    for(i in 1:nrow(t.B)){
      tmpexp[i] <- t(object$weights) %*% t.B[i, ]
    }
    names(tmpexp) <- rownames(t.B)
    print(tmpexp)
    cat("\n\n")
  }
  
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
  invisible(list(weights=object$weights,
                 opt_values=object$objective_measures,
                 group_weights=group_weights,
                 factor_exposures=tmpexp,
                 diversification=diversification(object$weights),
                 turnover=turnover(object$weights, wts.init=object$portfolio$assets),
                 positions=sum(abs(object$weights) > tolerance),
                 long_positions=sum(object$weights > tolerance),
                 short_positions=sum(object$weights < -tolerance)))
}

#' Print an efficient frontier object
#' 
#' Print method for efficient frontier objects. Display the call to create or
#' extract the efficient frontier object and the portfolio from which the 
#' efficient frontier was created or extracted.
#' 
#' @param x objective of class \code{efficient.frontier}
#' @param ... passthrough parameters
#' @author Ross Bennett
#' @export
print.efficient.frontier <- function(x, ...){
  if(!inherits(x, "efficient.frontier")) stop("object passed in is not of class 'efficient.frontier'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Efficient Frontier", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  cat("Efficient Frontier Points:", nrow(x$frontier), "\n\n")
  
  print(x$portfolio)
}

#' Summarize an efficient frontier object
#' 
#' Summary method for efficient frontier objects. Display the call to create or
#' extract the efficient frontier object as well as the weights and risk and
#' return metrics along the efficient frontier.
#' 
#' @param x objective of class \code{efficient.frontier}
#' @param ... passthrough parameters
#' @author Ross Bennett
#' @export
summary.efficient.frontier <- function(object, ..., digits=3){
  if(!inherits(object, "efficient.frontier")) stop("object passed in is not of class 'efficient.frontier'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Efficient Frontier", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  cat("Efficient Frontier Points:", nrow(object$frontier), "\n\n")
  
  # Weights
  cnames <- colnames(object$frontier)
  wts_idx <- grep(pattern="^w\\.", cnames)
  wts <- round(object$frontier[, wts_idx], digits=digits)
  colnames(wts) <- gsub("w.", "", colnames(wts))
  rownames(wts) <- 1:nrow(object$frontier)
  cat("Weights along the efficient frontier:\n")
  print(wts)
  cat("\n")
  
  # Risk and return
  cat("Risk and return metrics along the efficient frontier:\n")
  riskret <- object$frontier[, -wts_idx]
  rownames(riskret) <- 1:nrow(object$frontier)
  print(round(riskret, digits=digits))
  cat("\n")
  invisible(list(weights=wts, metrics=riskret))
}

