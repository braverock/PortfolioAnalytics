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

#' Printing output of optimize.portfolio.rebalancing
#' 
#' print method for \code{optimize.portfolio.rebalancing} objects
#' 
#' @param x an object used to select a method
#' @param \dots any other passthru parameters
#' @param digits the number of significant digits to use when printing.
#' @seealso \code{\link{optimize.portfolio.rebalancing}}
#' @author Ross Bennett
#' @rdname print.optimize.portfolio.rebalancing
#' @method print optimize.portfolio.rebalancing
#' @S3method print optimize.portfolio.rebalancing
print.optimize.portfolio.rebalancing <- function(x, ..., digits=4){
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization with Rebalancing\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  tmp_summary <- summary(x)
  rebal_dates <- tmp_summary$rebalance_dates
  num_dates <- length(rebal_dates)
  cat("Number of rebalancing dates: ", num_dates, "\n")
  
  cat("First rebalance date:\n")
  print(rebal_dates[1])
  
  cat("Last rebalance date:\n")
  print(rebal_dates[num_dates])
  
  cat("\n")
  cat("Annualized Portfolio Rebalancing Return:\n")
  print(as.numeric(tmp_summary$annualized_returns))
  cat("\n")
  
  cat("Annualized Portfolio Standard Deviation:\n")
  print(as.numeric(tmp_summary$annualized_StdDev))
  cat("\n")
}

#' summary method for optimize.portfolio.rebalancing
#' @param object object of type optimize.portfolio.rebalancing
#' @param \dots any other passthru parameters
#' @method summary optimize.portfolio.rebalancing
#' @export
summary.optimize.portfolio.rebalancing <- function(object, ...) {
    if(!inherits(object,"optimize.portfolio.rebalancing")) 
        stop ("passed object is not of class optimize.portfolio.rebalancing")
    call <- object$call
    elapsed_time <- object$elapsed_time
    
    # Extract the weights and objective measures
    weights <- extractWeights(object)
    rebalance_dates <- index(weights)
    objective_measures <- extractObjectiveMeasures(object)
    
    # Calculate the portfolio rebalancing returns and some useful 
    # performance metrics
    portfolio_returns <- Return.rebalancing(object$R, weights)
    annualized_returns <- Return.annualized(portfolio_returns)
    annualized_StdDev <- StdDev.annualized(portfolio_returns)
    downside_risk <- table.DownsideRisk(portfolio_returns)
    
    # Structure and return
    return(structure(list(weights=weights,
                          objective_measures=objective_measures,
                          portfolio_returns=portfolio_returns,
                          annualized_returns=annualized_returns,
                          annualized_StdDev=annualized_StdDev,
                          downside_risk=downside_risk,
                          rebalance_dates=rebalance_dates,
                          call=call,
                          elapsed_time=elapsed_time),
                     class="summary.optimize.portfolio.rebalancing")
    )
}

#' Printing summary output of optimize.portfolio.rebalancing
#' 
#' print method for objects of class \code{summary.optimize.portfolio.rebalancing}
#' 
#' @param x an object of class \code{summary.optimize.portfolio.rebalancing}.
#' @param \dots any other passthru parameters
#' @param digits number of digits used for printing
#' @seealso \code{\link{summary.optimize.portfolio.rebalancing}}
#' @author Ross Bennett
#' @method print summary.optimize.portfolio.rebalancing
#' @S3method print summary.optimize.portfolio.rebalancing
print.summary.optimize.portfolio.rebalancing <- function(x, ..., digits=4){
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization with Rebalancing\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  rebal_dates <- x$rebalance_dates
  num_dates <- length(rebal_dates)
  cat("First rebalance date:\n")
  print(rebal_dates[1])
  cat("\n")
  cat("Last rebalance date:\n")
  print(rebal_dates[num_dates])
  cat("\n")
  
  cat("Annualized Portfolio Rebalancing Return:\n")
  print(as.numeric(x$annualized_returns))
  cat("\n")
  
  cat("Annualized Portfolio Standard Deviation:\n")
  print(as.numeric(x$annualized_StdDev))
  cat("\n")
  
  cat("Downside Risk Measures:\n")
  print(x$downside_risk, ...=...)
  
  # Should we include the optimal weights and objective measure values on the
  # first or last rebalance date?
  # cat("Optimal weights on first rebalance date:\n")
  # print(round(first(x$weights), digits=digits), digits=digits)
  # cat("\n")
  
  # cat("Objective measures on first rebalance date:\n")
  # print(round(first(x$objective_measures), digits=digits), digits=digits)
  # cat("\n")
}

#' Printing Portfolio Specification Objects
#' 
#' Print method for objects of class \code{portfolio} created with \code{\link{portfolio.spec}}
#' 
#' @param x an object of class \code{portfolio}
#' @param \dots any other passthru parameters
#' @seealso \code{\link{portfolio.spec}}
#' @author Ross Bennett
#' @method print portfolio
#' @S3method print portfolio
print.portfolio <- function(x, ...){
  if(!is.portfolio(x)) stop("object passed in is not of class 'portfolio'")
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Portfolio Specification", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # Assets
  #cat("\nAssets\n")
  nassets <- length(x$assets)
  cat("Number of assets:", nassets, "\n")
  cat("Asset Names\n")
  print(head(names(x$assets), 10))
  if(nassets > 10){
    cat("More than 10 assets, only printing the first 10\n")
  }
  
  # Category labels
  if(!is.null(x$category_labels)){
    cat("\nCategory Labels\n")
    cat_labels <- x$category_labels
    for(i in 1:min(10, length(cat_labels))){
      cat(names(cat_labels)[i],": ")
      tmp <- names(x$assets[cat_labels[[i]]])
      cat(tmp, "\n")
    }
    if(length(cat_labels) > 10){
      cat("More than 10 categories, only printing the first 10\n")
    }
    cat("\n")
  }
  
  # Constraints
  nconstraints <- length(x$constraints)
  if(nconstraints > 0){
    cat("\nConstraints\n")
    # logical vector of enabled constraints
    enabled.constraints <- which(sapply(x$constraints, function(x) x$enabled))
    n.enabled.constraints <- ifelse(length(enabled.constraints) > 0, length(enabled.constraints), 0)
  } else {
    enabled.constraints <- NULL
    n.enabled.constraints <- 0
  }
  # character vector of constraint types
  names.constraints <- sapply(x$constraints, function(x) x$type)
  #cat("Number of constraints:", nconstraints, "\n")
  #cat("Number of enabled constraints:", n.enabled.constraints, "\n")
  if(length(enabled.constraints) > 0){
    cat("Enabled constraint types\n")
    constraints <- x$constraints
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
  
  if((nconstraints - n.enabled.constraints) > 0){
    #cat("Number of disabled constraints:", nconstraints - n.enabled.constraints, "\n")
    cat("Disabled constraint types\n")
    constraints <- x$constraints
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
  nobjectives <- length(x$objectives)
  if(nobjectives > 0){
    cat("\nObjectives:\n")
    # logical vector of enabled objectives
    enabled.objectives <- which(sapply(x$objectives, function(x) x$enabled))
    n.enabled.objectives <- ifelse(length(enabled.objectives) > 0, length(enabled.objectives), 0)
  } else {
    enabled.objectives <- NULL
    n.enabled.objectives <- 0
  }
  # character vector of objective names
  names.objectives <- sapply(x$objectives, function(x) x$name)
  #cat("Number of objectives:", nobjectives, "\n")
  #cat("Number of enabled objectives:", n.enabled.objectives, "\n")
  if(n.enabled.objectives > 0){
    cat("Enabled objective names\n")
    for(name in names.objectives[enabled.objectives]) {
      cat("\t\t-", name, "\n")
    }
  }
  
  if((nobjectives - n.enabled.objectives) > 0){
    #cat("Number of disabled objectives:", nobjectives - n.enabled.objectives, "\n")
    cat("Disabled objective names\n")
    for(name in setdiff(names.objectives, names.objectives[enabled.objectives])) {
      cat("\t\t-", name, "\n")
    }
  }
  cat("\n")
}

#' Summarize Portfolio Specification Objects
#' 
#' summary method for class \code{portfolio} created with \code{\link{portfolio.spec}}
#' 
#' @param object an object of class \code{portfolio}
#' @param \dots any other passthru parameters
#' @seealso \code{\link{portfolio.spec}}
#' @author Ross Bennett
#' @method summary portfolio
#' @export
summary.portfolio <- function(object, ...){
  if(!is.portfolio(object)) stop("object passed in is not of class 'portfolio'")
  
  out <- list()
  
  out$category_labels <- object$category_labels
  out$weight_seq <- object$weight_seq
  out$assets <- object$assets
  
  # constraints
  out$enabled_constraints <- list()
  out$disabled_constraints <- list()
  constraints <- object$constraints
  if(length(constraints) >= 1){
    for(i in 1:length(constraints)){
      if(constraints[[i]]$enabled){
        tmp <- length(out$enabled_constraints)
        out$enabled_constraints[[tmp+1]] <- constraints[[i]]
      } else {
        tmp <- length(out$disabled_constraints)
        out$disabled_constraints[[tmp+1]] <- constraints[[i]]
      }
    }
  }
  
  # objectives
  out$enabled_objectives <- list()
  out$disabled_objectives <- list()
  objectives <- object$objectives
  if(length(objectives) >= 1){
    for(i in 1:length(objectives)){
      if(objectives[[i]]$enabled){
        tmp <- length(out$enabled_objectives)
        out$enabled_objectives[[tmp+1]] <- objectives[[i]]
      } else {
        tmp <- length(out$disabled_objectives)
        out$disabled_objectives[[tmp+1]] <- objectives[[i]]
      }
    }
  }
  class(out) <- "summary.portfolio"
  return(out)
}

#' print method for constraint objects
#' 
#' @param x object of class \code{constraint}
#' @param \dots any other passthru parameters
#' @author Ross Bennett
#' @method print constraint
#' @S3method print constraint
print.constraint <- function(x, ...){
  print.default(x, ...)
}

#' Printing output of optimize.portfolio
#' 
#' print method for \code{optimize.portfolio} objects
#' 
#' @param x an object used to select a method
#' @param \dots any other passthru parameters
#' @param digits the number of significant digits to use when printing.
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @rdname print.optimize.portfolio
#' @method print optimize.portfolio.ROI
#' @S3method print optimize.portfolio.ROI
print.optimize.portfolio.ROI <- function(x, ..., digits=4){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=digits), digits=digits)
  cat("\n")
  
  # get objective measure
  objective_measures <- x$objective_measures
  tmp_obj <- as.numeric(unlist(objective_measures))
  names(tmp_obj) <- names(objective_measures)
  cat("Objective Measure:\n")
  for(i in 1:length(objective_measures)){
    print(tmp_obj[i], digits=digits)
    cat("\n")
    if(length(objective_measures[[i]]) > 1){
      # This will be the case for any objective measures with HHI for QP problems
      for(j in 2:length(objective_measures[[i]])){
        tmpl <- objective_measures[[i]][j]
        cat(names(tmpl), "\n")
        tmpv <- unlist(tmpl)
        names(tmpv) <- gsub(paste(names(tmpl), ".", sep=""), "", names(tmpv))
        print.default(round(tmpv, digits=digits), digits=digits)
        cat("\n")
      }
    }
    cat("\n")
  }
  cat("\n")
}


#' @rdname print.optimize.portfolio
#' @method print optimize.portfolio.random
#' @S3method print optimize.portfolio.random
print.optimize.portfolio.random <- function(x, ..., digits=4){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=digits), digits=digits)
  cat("\n")
  
  # get objective measures
  objective_measures <- x$objective_measures
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
        names(tmpv) <- names(x$weights)
        print(tmpv, digits=digits)
        cat("\n")
      }
    }
    cat("\n")
  }
  cat("\n")
}


#' @rdname print.optimize.portfolio
#' @method print optimize.portfolio.DEoptim
#' @S3method print optimize.portfolio.DEoptim
print.optimize.portfolio.DEoptim <- function(x, ..., digits=4){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=digits), digits=digits)
  cat("\n")
  
  # get objective measures
  objective_measures <- x$objective_measures
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
        names(tmpv) <- names(x$weights)
        print(tmpv, digits=digits)
        cat("\n")
      }
    }
    cat("\n")
  }
  cat("\n")
}


#' @rdname print.optimize.portfolio
#' @method print optimize.portfolio.GenSA
#' @S3method print optimize.portfolio.GenSA
print.optimize.portfolio.GenSA <- function(x, ..., digits=4){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=digits), digits=digits)
  cat("\n")
  
  # get objective measures
  objective_measures <- x$objective_measures
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
        names(tmpv) <- names(x$weights)
        print(tmpv, digits=digits)
        cat("\n")
      }
    }
    cat("\n")
  }
  cat("\n")
}


#' @rdname print.optimize.portfolio
#' @method print optimize.portfolio.pso
#' @S3method print optimize.portfolio.pso
print.optimize.portfolio.pso <- function(x, ..., digits=4){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=digits), digits=digits)
  cat("\n")
  
  # get objective measures
  objective_measures <- x$objective_measures
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
        names(tmpv) <- names(x$weights)
        print(tmpv, digits=digits)
        cat("\n")
      }
    }
    cat("\n")
  }
  cat("\n")
}

#' Printing summary output of optimize.portfolio
#' 
#' print method for objects of class \code{summary.optimize.portfolio}
#' 
#' @param x an object of class \code{summary.optimize.portfolio}.
#' @param ... any other passthru parameters. Currently not used.
#' @seealso \code{\link{summary.optimize.portfolio}}
#' @author Ross Bennett
#' @method print summary.optimize.portfolio
#' @S3method print summary.optimize.portfolio
print.summary.optimize.portfolio <- function(x, ...){
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization Summary", "\n")
  cat(rep("*", 50) ,"\n", sep="")
  
  # show the call to optimize.portfolio
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  
  # get optimal weights
  cat("Optimal Weights:\n")
  print.default(round(x$weights, digits=4))
  cat("\n")
  
  # objective measures
  # The objective measure is object$out for ROI
  cat("Objective Measures:\n")
  if(!is.null(x$objective_values)){
    # get objective measures
    objective_measures <- x$objective_values
    tmp_obj <- as.numeric(unlist(objective_measures))
    names(tmp_obj) <- names(objective_measures)
    for(i in 1:length(objective_measures)){
      print.default(tmp_obj[i], digits=4)
      cat("\n")
      if(length(objective_measures[[i]]) > 1){
        # This will be the case for any objective measures with risk budgets
        for(j in 2:length(objective_measures[[i]])){
          tmpl <- objective_measures[[i]][j]
          cat(names(tmpl), ":\n")
          tmpv <- unlist(tmpl)
          names(tmpv) <- names(x$weights)
          print.default(tmpv)
          cat("\n")
        }
      }
      cat("\n")
    }
  } else {
    print.default(as.numeric(x$out))
  }
  
  # get initial portfolio
  cat("Portfolio Assets and Initial Weights:\n")
  print.default(x$initial_weights)
  cat("\n")
  
  # print the portfolio object
  print(x$portfolio)
  
  # Constraints
  cat(rep("*", 40), "\n", sep="")
  cat("Constraints\n")
  cat(rep("*", 40), "\n", sep="")
  
  # leverage constraints
  cat("Leverage Constraint:\n")
  if(!is.null(x$leverage_constraint)){
    cat("min_sum = ", x$leverage_constraint$min_sum, "\n", sep="")
    cat("max_sum = ", x$leverage_constraint$max_sum, "\n", sep="")
    cat("actual_leverage = ", x$leverage_constraint$actual, "\n", sep="")
    cat("\n")
  }
  
  # box constraints
  cat("Box Constraints:\n")
  if(!is.null(x$box_constraint)){
    cat("min:\n")
    print.default(x$box_constraint$min)
    cat("max:\n")
    print.default(x$box_constraint$max)
    cat("\n")
  }
  
  # group constraints
  group_weights <- NULL
  if(!is.null(x$group_constraint)){
    cat("Group Constraints:\n")
    cat("Groups:\n")
    print.default(x$group_constraint$groups)
    cat("\n")
    cat("Lower bound on group weights, group_min:\n")
    print.default(x$group_constraint$group_min)
    cat("\n")
    cat("Upper bound on group weights, group_max:\n")
    print.default(x$group_constraint$group_max)
    cat("\n")
#    cat("Group position limits, group_pos:\n")
#     group_pos <- constraints$group_pos
#     if(!is.null(group_pos)) names(group_pos) <- group_labels
#     print(group_pos)
#     cat("\n")
    
    cat("Group Weights:\n")
    print.default(x$group_constraint$group_weights_actual)
    cat("\n")
  }
  tolerance <- .Machine$double.eps^0.5
  
  # position limit constraints
  cat("Position Limit Constraints:\n")
  cat("Maximum number of non-zero weights, max_pos:\n")
  if(!is.null(x$position_limit_constraint[["max_pos"]])){
    print.default(x$position_limit_constraint[["max_pos"]])
  } else {
    print("Unconstrained")
  }
  cat("Realized number of non-zero weights (i.e. positions):\n")
  print.default(x$position_limit_constraint$max_pos_actual)
  cat("\n")
  
  cat("Maximum number of long positions, max_pos_long:\n")
  if(!is.null(x$position_limit_constraint[["max_pos_long"]])){
    print.default(x$position_limit_constraint[["max_pos_long"]])
  } else {
    print("Unconstrained")
  }
  cat("Realized number of long positions:\n")
  print.default(x$position_limit_constraint$max_pos_long_actual)
  cat("\n")
  
  cat("Maximum number of short positions, max_pos_short:\n")
  if(!is.null(x$position_limit_constraint[["max_pos_short"]])){
    print.default(x$position_limit_constraint[["max_pos_short"]])
  } else {
    print("Unconstrained")
  }
  cat("Realized number of short positions:\n")
  print.default(x$position_limit_constraint$max_pos_short_actual)
  cat("\n\n")
  
  # diversification
  cat("Diversification Target Constraint:\n")
  if(!is.null(x$diversification_constraint$diversification_target)){
    print.default(x$diversification_constraint$diversification_target)
  } else {
    print("Unconstrained")
  }
  cat("\n")
  cat("Realized diversification:\n")
  print.default(x$diversification_constraint$diversification_actual)
  cat("\n")
  
  # turnover
  cat("Turnover Target Constraint:\n")
  if(!is.null(x$turnover_constraint$turnover_target)){
    print.default(x$turnover_constraint$turnover_target)
  } else {
    print("Unconstrained")
  }
  cat("\n")
  cat("Realized turnover from initial weights:\n")
  print.default(x$turnover_constraint$turnover_actual)
  cat("\n")
  
  # Factor exposure constraint
  if(!is.null(x$factor_exposure_constraint)){
    cat("Factor Exposure Constraints:\n")
    cat("Factor Exposure B Matrix:\n")
    print.default(x$factor_exposure_constraint$B)
    cat("\n")
    cat("Lower bound on factor exposures, lower:\n")
    print.default(x$factor_exposure_constraint$lower)
    cat("\n")
    cat("Upper bound on group weights, upper:\n")
    print.default(x$factor_exposure_constraint$upper)
    cat("\n")
    cat("Realized Factor Exposures:\n")
    print.default(x$factor_exposure_constraint$exposure_actual)
    cat("\n\n")
  }
  
  # Objectives
  cat(rep("*", 40), "\n", sep="")
  cat("Objectives\n")
  cat(rep("*", 40), "\n\n", sep="")
  
  for(obj in x$portfolio$objectives){
    cat("Objective:", class(obj)[1], "\n")
    print.default(obj)
    cat("\n", rep("*", 40), "\n", sep="")
  }
  cat("\n")
  
  # show the elapsed time for the optimization
  cat("Elapsed Time:\n")
  print(x$elapsed_time)
  cat("\n")
}

#' Summarizing output of optimize.portfolio
#' 
#' summary method for class \code{optimize.portfolio}
#' 
#' @param object an object of class \code{optimize.portfolio}.
#' @param ... any other passthru parameters. Currently not used.
#' @seealso \code{\link{optimize.portfolio}}
#' @author Ross Bennett
#' @method summary optimize.portfolio
#' @S3method summary optimize.portfolio
summary.optimize.portfolio <- function(object, ...){
  
  out <- list()
  
  out$call <- object$call
  
  # optimal weights
  opt_weights <- extractWeights(object)
  out$weights <- opt_weights
  
  # objective measure values
  out$objective_values <- extractObjectiveMeasures(object)
  
  # optimization time
  out$elapsed_time <- object$elapsed_time
  
  # initial weights
  initial_weights <- object$portfolio$assets
  out$initial_weights <- initial_weights
  
  ### constraint realization
  constraints <- get_constraints(object$portfolio)
  # leverage
  leverage_constraint <- list()
  leverage_constraint$min_sum <- constraints$min_sum
  leverage_constraint$max_sum <- constraints$max_sum
  leverage_constraint$actual <- sum(opt_weights)
  out$leverage_constraint <- leverage_constraint
  
  # box
  box_constraint <- list()
  box_constraint$min <- constraints$min
  box_constraint$max <- constraints$max
  box_constraint$actual <- opt_weights
  out$box_constraint <- box_constraint
  
  # group
  if(!is.null(constraints$groups)){
    asset_names <- names(opt_weights)
    group_constraint <- list()
    group_constraint$groups <- list()
    groups <- constraints$groups
    for(i in 1:length(groups)){
      groups[[i]] <- asset_names[groups[[i]]]
    }
    group_constraint$groups <- groups
    group_constraint$group_min <- constraints$cLO
    group_constraint$group_max <- constraints$cUP
    group_constraint$group_pos <- constraints$group_pos
    
    # actual weights by group and/or category
    tmp_groups <- extractGroups(object)
    group_constraint$group_weights_actual <- tmp_groups$group_weights
    out$group_constraint <- group_constraint
  }
  
  # category weights
  if(is.null(constraints$groups) & !is.null(object$portfolio$category_labels)){
    category_weights <- list()
    category_weights$category_weights <- object$portfolio$category_labels
    tmp_groups <- extractGroups(object)
    category_weights$category_weights_actual <- tmp_groups$category_weights
    out$category_weights <- category_weights
  }
  
  # factor exposure
  if(!is.null(constraints$B) & !is.null(constraints$lower) & !is.null(constraints$upper)){
    factor_exposure_constraint <- list()
    factor_exposure_constraint$B <- constraints$B
    factor_exposure_constraint$lower <- constraints$lower
    names(factor_exposure_constraint$lower) <- colnames(constraints$B)
    factor_exposure_constraint$upper <- constraints$upper
    names(factor_exposure_constraint$upper) <- colnames(constraints$B)
    
    t.B <- t(constraints$B)
    tmpexp <- vector(mode="numeric", length=nrow(t.B))
    for(i in 1:nrow(t.B)){
      tmpexp[i] <- t(opt_weights) %*% t.B[i, ]
    }
    names(tmpexp) <- rownames(t.B)
    factor_exposure_constraint$exposure_actual <- tmpexp
    out$factor_exposure_constraint <- factor_exposure_constraint
  }
  
  # position limit
  tolerance <- .Machine$double.eps^0.5
  position_limit_constraint <- list()
  position_limit_constraint$max_pos <- constraints$max_pos
  position_limit_constraint$max_pos_long <- constraints$max_pos_long
  position_limit_constraint$max_pos_short <- constraints$max_pos_short
  # number of positions with non-zero weights
  position_limit_constraint$max_pos_actual <- sum(abs(object$weights) > tolerance)
  # actual long positions
  position_limit_constraint$max_pos_long_actual <- sum(object$weights > tolerance)
  # actual short positions
  position_limit_constraint$max_pos_short_actual <- sum(object$weights < -tolerance)
  out$position_limit_constraint <- position_limit_constraint
  
  # diversification
  diversification_constraint <- list()
  # target diversification
  diversification_constraint$diversification_target <- constraints$div_target
  # actual realized diversification
  diversification_constraint$diversification_actual <- diversification(opt_weights)
  out$diversification_constraint <- diversification_constraint
  
  # turnover
  turnover_constraint <- list()
  turnover_constraint$turnover_target <- constraints$turnover_target
  turnover_constraint$turnover_actual <- turnover(opt_weights, wts.init=initial_weights)
  out$turnover_constraint <- turnover_constraint
  
  # original portfolio object
  out$portfolio <- object$portfolio
  
  class(out) <- "summary.optimize.portfolio"
  return(out)
}

#' Print an efficient frontier object
#' 
#' Print method for efficient frontier objects. Display the call to create or
#' extract the efficient frontier object and the portfolio from which the 
#' efficient frontier was created or extracted.
#' 
#' @param x objective of class \code{efficient.frontier}
#' @param \dots any other passthru parameters
#' @seealso \code{\link{create.EfficientFrontier}}
#' @author Ross Bennett
#' @method print efficient.frontier
#' @S3method print efficient.frontier
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
#' @param object object of class \code{efficient.frontier}
#' @param ... passthrough parameters
#' @param digits number of digits to round to
#' @author Ross Bennett
#' @method summary efficient.frontier
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
  print(round(wts, digits=digits))
  cat("\n")
  
  # Risk and return
  cat("Risk and return metrics along the efficient frontier:\n")
  riskret <- object$frontier[, -wts_idx]
  rownames(riskret) <- 1:nrow(object$frontier)
  print(round(riskret, digits=digits))
  cat("\n")
  invisible(list(weights=wts, metrics=riskret))
}

#' @method print portfolio.list
#' @S3method print portfolio.list
#' @export
print.portfolio.list <- function(x, ...){
  for(i in 1:length(x)){
    cat("Portfolio ", i, "\n", sep="")
    print(x[[i]])
  }
}

#' @method print opt.list
#' @S3method print opt.list
#' @export
print.opt.list <- function(x, ...){
  for(i in 1:length(x)){
    cat("Optimization ", i, "\n", sep="")
    print(x[[i]])
  }
}

#' @method print opt.rebal.list
#' @S3method print opt.rebal.list
#' @export
print.opt.rebal.list <- function(x, ...){
  for(i in 1:length(x)){
    cat("Optimization ", i, "\n", sep="")
    print(x[[i]])
  }
}

#' @method print regime.portfolios
#' @S3method print regime.portfolios
#' @export
print.regime.portfolios <- function(x, ...){
  
  cat(rep("*", 50) ,"\n", sep="")
  cat("PortfolioAnalytics Regime Switching Specification", "\n")
  cat(rep("*", 50) ,"\n\n", sep="")
  
  # Should we print the regime object information?
  
  portf <- x$portfolio.list
  for(i in 1:length(portf)){
    cat("Regime ", i, " Portfolio", "\n", sep="")
    print(portf[[i]])
  }
}

#' @method summary optimize.portfolio.parallel
#' @S3method summary optimize.portfolio.parallel
#' @export
summary.optimize.portfolio.parallel <- function(object, ...){
  out <- list()
  out$call <- object$call
  out$elapsed_time <- object$elapsed_time
  out$n_optimizations <- length(object$optimizations)
  xx <- lapply(object$optimizations, function(x) {
    tmp <- extractStats(x)
    out <- tmp[which.min(tmp[,"out"]),]
    out})
  stats <- do.call(rbind, xx)
  out$stats <- stats
  out$obj_val <- stats[,"out"]
  class(out) <- "summary.optimize.portfolio.parallel"
  return(out)
}

#' @method print optimize.portfolio.parallel
#' @S3method print optimize.portfolio.parallel
#' @export
print.optimize.portfolio.parallel <- function(x, ..., probs = c(0.025, 0.975)){
  cat(rep("*", 35) ,"\n", sep="")
  cat("PortfolioAnalytics Optimization\n")
  cat(rep("*", 35) ,"\n", sep="")
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  # call the summary method
  xx <- summary(x)
  
  cat("Number of Optimizations:\n")
  print(xx$n_optimizations)
  
  cat("Objective Value Estimate:\n")
  print(mean(xx$obj_val))
  
  cat("Objective Value Estimate Percentiles:\n")
  print(quantile(xx$obj_val, probs = probs))
  
  cat("Elapsed Time:\n")
  print(xx$elapsed_time)
}
