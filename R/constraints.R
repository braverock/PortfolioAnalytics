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

#' @rdname constraint
#' @export
constraint_v1 <- function(assets=NULL, ... ,min,max,min_mult,max_mult,min_sum=.99,max_sum=1.01,weight_seq=NULL)
{ # based on GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
  if (hasArg(min) & hasArg(max)) {
    if (is.null(assets) & (!length(min)>1) & (!length(max)>1)) {
      stop("You must either specify the assets or pass a vector for both min and max")
    }
  }

  if(!is.null(assets)){
    # TODO FIXME this doesn't work quite right on matrix of assets
    if(is.numeric(assets)){
      if (length(assets) == 1) {
        nassets=assets
        #we passed in a number of assets, so we need to create the vector
        message("assuming equal weighted initial portfolio")
        assets<-rep(1/nassets,nassets)
      } else {
        nassets = length(assets)
      }
      # and now we may need to name them
      if (is.null(names(assets))) {
        for(i in 1:length(assets)){
          names(assets)[i]<-paste("Asset",i,sep=".")
        }
      }
    }
    if(is.character(assets)){
      nassets=length(assets)
      assetnames=assets
      message("assuming equal weighted initial portfolio")
      assets<-rep(1/nassets,nassets)
      names(assets)<-assetnames  # set names, so that other code can access it,
      # and doesn't have to know about the character vector
      # print(assets)
    }
    # if assets is a named vector, we'll assume it is current weights
  }

  if(hasArg(min) | hasArg(max)) {
    if (length(min)>1 & length(max)>1){
      if (length(min)!=length(max)) { stop("length of min and max must be the same") }
    } 

    if (length(min)==1) {
        message("min not passed in as vector, replicating min to length of length(assets)")
        min <- rep(min,nassets)
    }
    if (length(min)!=nassets) stop(paste("length of min must be equal to 1 or the number of assets",nassets))
    
    if (length(max)==1) {
        message("max not passed in as vector, replicating max to length of length(assets)")
        max <- rep(max,nassets)
    }
    if (length(max)!=nassets) stop(paste("length of max must be equal to 1 or the number of assets",nassets))
    
  } else {
    message("no min or max passed in, assuming 0 and 1")
    min <- rep(0,nassets)
    max <- rep(1,nassets)
  }

  names(min)<-names(assets)
  names(max)<-names(assets)
  
  if(hasArg(min_mult) | hasArg(max_mult)) {
    if (length(min_mult)>1 & length(max_mult)>1){
      if (length(min_mult)!=length(max_mult) ) { stop("length of min_mult and max_mult must be the same") }
    } else {
      message("min_mult and max_mult not passed in as vectors, replicating min_mult and max_mult to length of assets vector")
      min_mult = rep(min_mult,nassets)
      max_mult = rep(max_mult,nassets)
    }
  }

  if(!hasArg(min_sum) | !hasArg(max_sum)) {
    min_sum = NULL
    max_sum = NULL 
  }

  if (!is.null(names(assets))) {
    assetnames<-names(assets)
    if(hasArg(min)){
      names(min)<-assetnames
      names(max)<-assetnames
    } else {
      min = NULL
      max = NULL
    }
    if(hasArg(min_mult)){
      names(min_mult)<-assetnames
      names(max_mult)<-assetnames
    } else {
      min_mult = NULL
      max_mult = NULL
    }
  }
  ##now adjust min and max to account for min_mult and max_mult from initial
  if(!is.null(min_mult) & !is.null(min)) {
    tmp_min <- assets*min_mult
    #TODO FIXME this creates a list, and it should create a named vector or matrix
    min[which(tmp_min>min)]<-tmp_min[which(tmp_min>min)]
  }
  if(!is.null(max_mult) & !is.null(max)) {
    tmp_max <- assets*max_mult
    #TODO FIXME this creates a list, and it should create a named vector or matrix
    max[which(tmp_max<max)]<-tmp_max[which(tmp_max<max)]
  }

  ## now structure and return
  return(structure(
    list(
      assets = assets,
      min = min,
      max = max,
      min_mult = min_mult,
      max_mult = max_mult,
      min_sum  = min_sum,
      max_sum  = max_sum,
      weight_seq = weight_seq,
      objectives = list(),
      call = match.call()
    ),
    class=c("v1_constraint","constraint")
  ))
}


#' constructors for class constraint
#'
#' See main documentation entry in \code{\link{add.constraint}}.
#' 
#' This includes the deprecated constructor for the \code{v1_constraint} object for backwards compatibility.
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying initial weights
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying minimum weight box constraints
#' @param min_mult numeric or named vector specifying minimum multiplier box constraint from initial weight in \code{assets}
#' @param max_mult numeric or named vector specifying maximum multiplier box constraint from initial weight in \code{assets}
#' @param min_sum minimum sum of all asset weights, default .99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
#' @param type character type of the constraint to add or update
#' @param enabled TRUE/FALSE to enabled the constraint
#' @param \dots any other passthru parameters
#' @param constrclass name of class for the constraint
#' @author Peter Carl, Brian G. Peterson, Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @aliases constraint constraint_v2
#' @rdname constraint
#' @export constraint
#' @export constraint_v2
constraint <- constraint_v2 <- function(type, enabled=TRUE, ..., constrclass="v2_constraint"){
  if(!hasArg(type)) stop("you must specify a constraint type")
  if (hasArg(type)) if(is.null(type)) stop("you must specify a constraint type")
  
  ## now structure and return
  return(structure( c(list(type = type,
                           enabled=enabled),
                      list(...)),
                    class=c(constrclass, "constraint")
  ) # end structure
  )
}

#' General interface for adding and/or updating optimization constraints.
#' 
#' This is the main function for adding and/or updating constraints to the \code{\link{portfolio.spec}} object.
#' 
#' The following constraint types may be specified:
#' \itemize{
#' \item{\code{weight_sum}, \code{weight}, \code{leverage}}{ Specify constraint on the sum of the weights, see \code{\link{weight_sum_constraint}} }
#' \item{\code{full_investment}}{ Special case to set \code{min_sum=1} and \code{max_sum=1} of weight sum constraints }
#' \item{\code{dollar_neutral}, \code{active}}{ Special case to set \code{min_sum=0} and \code{max_sum=0} of weight sum constraints }
#' \item{\code{box}}{ box constraints for the individual asset weights, see \code{\link{box_constraint}} }
#' \item{\code{long_only}}{ Special case to set \code{min=0} and \code{max=1} of box constraints }
#' \item{\code{group}}{ specify the sum of weights within groups and the number of assets with non-zero weights in groups, see \code{\link{group_constraint}} }
#' \item{\code{turnover}}{ Specify a constraint for target turnover. Turnover is calculated from a set of initial weights, see \code{\link{turnover_constraint}} }
#' \item{\code{diversification}}{ target diversification of a set of weights, see \code{\link{diversification_constraint}} }
#' \item{\code{position_limit}}{ Specify the number of non-zero, long, and/or short positions, see \code{\link{position_limit_constraint}} }
#' \item{\code{return}}{ Specify the target mean return, see \code{\link{return_constraint}}}
#' \item{\code{factor_exposure}}{ Specify risk factor exposures, see \code{\link{factor_exposure_constraint}}}
#' \item{\code{leverage_exposure}}{ Specify a maximum leverage exposure, see \code{\link{leverage_exposure_constraint}}}
#' }
#' 
#' @param portfolio an object of class 'portfolio' to add the constraint to, specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param type character type of the constraint to add or update, currently 'weight_sum' (also 'leverage' or 'weight'), 'box', 'group', 'turnover', 'diversification', 'position_limit', 'return', 'factor_exposure', or 'leverage_exposure'
#' @param enabled TRUE/FALSE. The default is enabled=TRUE.
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify constraints
#' @param indexnum if you are updating a specific constraint, the index number in the $constraints list to update
#' @author Ross Bennett
#' @seealso 
#' \code{\link{portfolio.spec}}
#' \code{\link{weight_sum_constraint}}, 
#' \code{\link{box_constraint}}, 
#' \code{\link{group_constraint}}, 
#' \code{\link{turnover_constraint}}, 
#' \code{\link{diversification_constraint}}, 
#' \code{\link{position_limit_constraint}}, 
#' \code{\link{return_constraint}}, 
#' \code{\link{factor_exposure_constraint}},
#' \code{\link{leverage_exposure_constraint}}
#' @examples
#' data(edhec)
#' returns <- edhec[, 1:4]
#' fund.names <- colnames(returns)
#' pspec <- portfolio.spec(assets=fund.names)
#' 
#' # Add the full investment constraint that specifies the weights must sum to 1.
#' pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)
#' 
#' # The full investment constraint can also be specified with type="full_investment"
#' pspec <- add.constraint(portfolio=pspec, type="full_investment")
#' 
#' # Another common constraint is that portfolio weights sum to 0.
#' pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=0, max_sum=0)
#' pspec <- add.constraint(portfolio=pspec, type="dollar_neutral")
#' pspec <- add.constraint(portfolio=pspec, type="active")
#' 
#' # Add box constraints
#' pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4)
#' 
#' # min and max can also be specified per asset
#' pspec <- add.constraint(portfolio=pspec, 
#'                         type="box", 
#'                         min=c(0.05, 0, 0.08, 0.1), 
#'                         max=c(0.4, 0.3, 0.7, 0.55))
#'                         
#' # A special case of box constraints is long only where min=0 and max=1
#' # The default action is long only if min and max are not specified
#' pspec <- add.constraint(portfolio=pspec, type="box")
#' pspec <- add.constraint(portfolio=pspec, type="long_only")
#' 
#' # Add group constraints
#' pspec <- add.constraint(portfolio=pspec, 
#'                         type="group", 
#'                         groups=list(c(1, 2, 1), 4), 
#'                         group_min=c(0.1, 0.15), 
#'                         group_max=c(0.85, 0.55), 
#'                         group_labels=c("GroupA", "GroupB"), 
#'                         group_pos=c(2, 1))
#' 
#' # Add position limit constraint such that we have a maximum number 
#' # of three assets with non-zero weights.
#' pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
#' 
#' # Add diversification constraint
#' pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
#' 
#' # Add turnover constraint
#' pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)
#' 
#' # Add target mean return constraint
#' pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)
#' 
#' # Example using the indexnum argument
#' portf <- portfolio.spec(assets=fund.names)
#' portf <- add.constraint(portf, type="full_investment")
#' portf <- add.constraint(portf, type="long_only")
#' 
#' # indexnum corresponds to the index number of the constraint
#' # The full_investment constraint was the first constraint added and has 
#' # indexnum=1
#' portf$constraints[[1]]
#' 
#' # View the constraint with indexnum=2
#' portf$constraints[[2]]
#' 
#' # Update the constraint to relax the sum of weights constraint
#' portf <- add.constraint(portf, type="weight_sum", 
#' min_sum=0.99, max_sum=1.01, 
#' indexnum=1)
#' 
#' # Update the constraint to modify the box constraint
#' portf <- add.constraint(portf, type="box", 
#' min=0.1, max=0.8, 
#' indexnum=2)
#' @export
add.constraint <- function(portfolio, type, enabled=TRUE, message=FALSE, ..., indexnum=NULL){
  # Check to make sure that the portfolio passed in is a portfolio object
  if (!is.portfolio(portfolio)) {stop("portfolio passed in is not of class portfolio")}
  
  # Check to make sure a type is passed in as an argument
  if (!hasArg(type)) stop("you must supply a type of constraints to create")
  
  assets <- portfolio$assets
  tmp_constraint = NULL
  
  # Currently supports box and group constraints. Will add more later.
  switch(type,
         # Box constraints
         box = {tmp_constraint <- box_constraint(assets=assets,
                                                 type=type,
                                                 enabled=enabled,
                                                 message=message,
                                                 ...=...)
         },
         # special case of box constraints for long_only
         long_only = {tmp_constraint <- box_constraint(assets=assets,
                                                       type=type,
                                                       enabled=enabled,
                                                       message=message,
                                                       min=0,
                                                       max=1,
                                                       ...=...)
         },
         # Group constraints
         group = {tmp_constraint <- group_constraint(assets=assets, 
                                                     type=type,
                                                     enabled=enabled,
                                                     message=message,
                                                     ...=...)
         },
         # Sum of weights constraints
         weight=, leverage=, weight_sum = {tmp_constraint <- weight_sum_constraint(type=type,
                                                                                   enabled=enabled,
                                                                                   message=message,
                                                                                   ...=...)
         },
         # Special case of weight_sum constraint for full investment
         full_investment = {tmp_constraint <- weight_sum_constraint(type=type,
                                                                    min_sum=1,
                                                                    max_sum=1,
                                                                    enabled=enabled,
                                                                    message=message,
                                                                    ...=...)
         },
         # Special case of weight_sum constraint for dollar neutral or active
         dollar_neutral=, active= {tmp_constraint <- weight_sum_constraint(type=type,
                                                                           min_sum=0,
                                                                           max_sum=0,
                                                                           enabled=enabled,
                                                                           message=message,
                                                                           ...=...)
         },
         # Turnover constraint
         turnover = {tmp_constraint <- turnover_constraint(type=type,
                                                           enabled=enabled,
                                                           message=message,
                                                           ...=...)
         },
         # Diversification constraint
         diversification = {tmp_constraint <- diversification_constraint(type=type,
                                                                         enabled=enabled,
                                                                         message=message,
                                                                         ...=...)
         },
         # Position limit constraint
         position_limit = {tmp_constraint <- position_limit_constraint(assets=assets,
                                                                       type=type,
                                                                       enabled=enabled,
                                                                       message=message,
                                                                       ...=...)
         },
         # Return constraint
         return = {tmp_constraint <- return_constraint(type=type,
                                                       enabled=enabled,
                                                       message=message,
                                                       ...=...)
         },
         # factor exposure constraint
         factor_exposure=, factor_exposures = {tmp_constraint <- factor_exposure_constraint(assets=assets, 
                                                                         type=type, 
                                                                         enabled=enabled, 
                                                                         message=message, 
                                                                         ...=...)
         },
         # transaction cost  constraint
         transaction=, transaction_cost = {tmp_constraint <- transaction_cost_constraint(assets=assets, 
                                                                                            type=type, 
                                                                                            enabled=enabled, 
                                                                                            message=message, 
                                                                                            ...=...)
         },
         # leverage exposure constraint
         leverage_exposure = {tmp_constraint <- leverage_exposure_constraint( type=type,
                                                                              enabled=enabled,
                                                                              message=message,
                                                                              ...=...)
         },
         # Do nothing and return the portfolio object if type is NULL
         null = {return(portfolio)}
  )
  if(is.constraint(tmp_constraint)) {
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum <- length(portfolio$constraints)+1
    tmp_constraint$call <- match.call()
    portfolio$constraints[[indexnum]] <- tmp_constraint
  }
  return(portfolio)
}

#' constructor for box_constraint.
#' 
#' Box constraints specify the upper and lower bounds on the weights of the assets.
#' This function is called by add.constraint when type="box" is specified. See \code{\link{add.constraint}}.
#'
#' @param type character type of the constraint
#' @param assets number of assets, or optionally a named vector of assets specifying initial weights
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying minimum weight box constraints
#' @param min_mult numeric or named vector specifying minimum multiplier box constraint from initial weight in \code{assets}
#' @param max_mult numeric or named vector specifying maximum multiplier box constraint from initial weight in \code{assets}
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify box constraints
#' @return an object of class 'box_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' # defaults to min=0 and max=1
#' pspec <- add.constraint(pspec, type="box")
#' 
#' # specify box constraints as a scalar
#' pspec <- add.constraint(pspec, type="box", min=0.05, max=0.45)
#' 
#' # specify box constraints per asset
#' pspec <- add.constraint(pspec, 
#'                         type="box", 
#'                         min=c(0.05, 0.10, 0.08, 0.06), 
#'                         max=c(0.45, 0.55, 0.35, 0.65))
#'                         
#' @export
box_constraint <- function(type="box", assets, min, max, min_mult, max_mult, enabled=TRUE, message=FALSE, ...){
  # Based on the constraint function for object of class constraint_v1 that
  # included specifying box constraints.
  
  # Get the length of the assets vector
  nassets <- length(assets)
  
  if(type=="long_only"){
    min <- rep(0, nassets)
    max <- rep(1, nassets)
  }
  
  # Check that the length of min and max are the same
  if(hasArg(min) | hasArg(max)) {
    if (length(min) > 1 & length(max) > 1){
      if (length(min) != length(max)) { stop("length of min and max must be the same") }
    } 
    
    # If the user passes in a scalar for min, then create a min vector 
    if (length(min) == 1) {
      if(message) message("min not passed in as vector, replicating min to length of length(assets)")
      min <- rep(min, nassets)
    }
    if (length(min) != nassets) stop(paste("length of min must be equal to 1 or the number of assets:", nassets))
    
    # If the user passes in a scalar for max, then create a max vector
    if (length(max) == 1) {
      if(message) message("max not passed in as vector, replicating max to length of length(assets)")
      max <- rep(max, nassets)
    }
    if (length(max) != nassets) stop(paste("length of max must be equal to 1 or the number of assets:", nassets))
    
  } else {
    # Default to min=0 and max=1 if min or max are not passed in
    if(message) message("no min or max passed in, assuming 0 and 1")
    min <- rep(0, nassets)
    max <- rep(1, nassets)
  }
  
  # Set the names of the min and max vector to the names of the assets vector
  names(min) <- names(assets)
  names(max) <- names(assets)
  
  # Checks for min_mult and max_mult
  if(hasArg(min_mult) | hasArg(max_mult)) {
    if (length(min_mult) > 1 & length(max_mult) > 1){
      if (length(min_mult) != length(max_mult) ) { stop("length of min_mult and max_mult must be the same") }
    } else {
      if(message) message("min_mult and max_mult not passed in as vectors, replicating min_mult and max_mult to length of assets vector")
      min_mult = rep(min_mult, nassets)
      max_mult = rep(max_mult, nassets)
    }
  }
  
  if (!is.null(names(assets))) {
    assetnames <- names(assets)
    if(hasArg(min)){
      names(min) <- assetnames
      names(max) <- assetnames
    } else {
      min = NULL
      max = NULL
    }
    if(hasArg(min_mult)){
      names(min_mult) <- assetnames
      names(max_mult) <- assetnames
    } else {
      min_mult = NULL
      max_mult = NULL
    }
  }
  
  # now adjust min and max to account for min_mult and max_mult from initial
  if(!is.null(min_mult) & !is.null(min)) {
    tmp_min <- assets * min_mult
    #TODO FIXME this creates a list, and it should create a named vector or matrix
    min[which(tmp_min > min)] <- tmp_min[which(tmp_min > min)]
  }
  if(!is.null(max_mult) & !is.null(max)) {
    tmp_max <- assets * max_mult
    #TODO FIXME this creates a list, and it should create a named vector or matrix
    max[which(tmp_max < max)] <- tmp_max[which(tmp_max < max)]
  }
  
  Constraint <- constraint_v2(type=type, enabled=enabled, constrclass="box_constraint", ...)
  Constraint$min <- min
  Constraint$max <- max
  return(Constraint)
}

#' constructor for group_constraint
#' 
#' Group constraints specify the grouping of the assets, weights of the groups, and number of postions (i.e. non-zero weights) iof the groups.
#' This function is called by add.constraint when type="group" is specified. see \code{\link{add.constraint}}
#'
#' @param type character type of the constraint
#' @param assets number of assets, or optionally a named vector of assets specifying initial weights
#' @param groups list of vectors specifying the groups of the assets
#' @param group_labels character vector to label the groups (e.g. size, asset class, style, etc.)
#' @param group_min numeric or vector specifying minimum weight group constraints
#' @param group_max numeric or vector specifying minimum weight group constraints
#' @param group_pos vector specifying the number of non-zero weights per group
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify group constraints
#' @return an object of class 'group_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' # Assets 1 and 3 are groupA
#' # Assets 2 and 4 are groupB
#' pspec <- add.constraint(portfolio=pspec, 
#'                         type="group", 
#'                         groups=list(groupA=c(1, 3),
#'                                     groupB=c(2, 4)),
#'                         group_min=c(0.15, 0.25),
#'                         group_max=c(0.65, 0.55))
#' 
#' # 2 levels of grouping (e.g. by sector and geography)
#' pspec <- portfolio.spec(assets=5)
#' # Assets 1, 3, and 5 are Tech
#' # Assets 2 and 4 are Oil
#' # Assets 2, 4, and 5 are UK
#' # Assets 1 and are are US
#' group_list <- list(group1=c(1, 3, 5),
#'                    group2=c(2, 4),
#'                    groupA=c(2, 4, 5),
#'                    groupB=c(1, 3))
#' 
#' pspec <- add.constraint(portfolio=pspec, 
#'                         type="group", 
#'                         groups=group_list,
#'                         group_min=c(0.15, 0.25, 0.2, 0.1),
#'                         group_max=c(0.65, 0.55, 0.5, 0.4))
#'                         
#' @export
group_constraint <- function(type="group", assets, groups, group_labels=NULL, group_min, group_max, group_pos=NULL, enabled=TRUE, message=FALSE, ...) {
  if(!is.list(groups)) stop("groups must be passed in as a list")
  nassets <- length(assets)
  ngroups <- length(groups)
  groupnames <- names(groups)
  
  # comment out so the user can pass in multiple levels of groups
  # may want a warning message
  # count <- sum(sapply(groups, length))
  # if(count != nassets) {
  #   message("count of assets in groups must be equal to the number of assets")
  # }
  
  # Checks for group_min
  if (length(group_min) == 1) {
    if(message) message("group_min not passed in as vector, replicating group_min to length of groups")
    group_min <- rep(group_min, ngroups)
  }
  if (length(group_min) != ngroups) stop(paste("length of group_min must be equal to 1 or the length of groups:", ngroups))
  
  # Checks for group_max
  if (length(group_max) == 1) {
    if(message) message("group_max not passed in as vector, replicating group_max to length of groups")
    group_max <- rep(group_max, ngroups)
  }
  if (length(group_max) != ngroups) stop(paste("length of group_max must be equal to 1 or the length of groups:", ngroups))
  
  # construct the group_label vector if groups is a named list
  if(!is.null(groupnames)){
    group_labels <- groupnames
  }
  
  # Construct the group_label vector if it is not passed in
  if(is.null(group_labels) & is.null(groupnames)){
    group_labels <- paste(rep("group", ngroups), 1:ngroups, sep="")
  }
  
  if(length(group_labels) != length(groups)) stop("length of group_labels must be equal to the length of groups")
  
  # Construct group_pos vector
  if(!is.null(group_pos)){
    # Check the length of the group_pos vector
    if(length(group_pos) != length(groups)) stop("length of group_pos must be equal to the length of groups")
    # Check for negative values in group_pos
    if(any(group_pos < 0)) stop("all elements of group_pos must be positive")
    # Elements of group_pos cannot be greater than count of assets in groups
    if(any(group_pos > sapply(groups, length))){
      group_pos <- pmin(group_pos, sapply(groups, length))
    }
  }
  
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="group_constraint", ...)
  Constraint$groups <- groups
  Constraint$group_labels <- group_labels
  Constraint$cLO <- group_min
  Constraint$cUP <- group_max
  Constraint$group_pos <- group_pos
  return(Constraint)
}

#' constructor for weight_sum_constraint
#' 
#' The constraint specifies the upper and lower bound on the sum of the weights.
#' This function is called by add.constraint when "weight_sum", "leverage", "full_investment", "dollar_neutral", or "active" is specified as the type. see \code{\link{add.constraint}}
#' 
#' Special cases for the weight_sum constraint are "full_investment" and "dollar_nuetral" or "active"
#' 
#' If \code{type="full_investment"}, \code{min_sum=1} and \code{max_sum=1}
#' 
#' If \code{type="dollar_neutral"} or \code{type="active"}, \code{min_sum=0}, and \code{max_sum=0}
#' 
#' @param type character type of the constraint
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify weight_sum constraints
#' @return an object of class 'weight_sum_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' # min_sum and max_sum can be specified with type="weight_sum" or type="leverage"
#' pspec <- add.constraint(pspec, type="weight_sum", min_sum=1, max_sum=1)
#' 
#' # Specify type="full_investment" to set min_sum=1 and max_sum=1
#' pspec <- add.constraint(pspec, type="full_investment")
#' 
#' # Specify type="dollar_neutral" or type="active" to set min_sum=0 and max_sum=0
#' pspec <- add.constraint(pspec, type="dollar_neutral")
#' pspec <- add.constraint(pspec, type="active")
#' @export
weight_sum_constraint <- function(type="weight_sum", min_sum=0.99, max_sum=1.01, enabled=TRUE, ...){
  switch(type,
         full_investment = {
           max_sum <- 1
           min_sum <- 1
         },
         dollar_neutral = {
           max_sum <- 0
           min_sum <- 0
         },
         active = {
           max_sum <- 0
           min_sum <- 0
         }
  )
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="weight_sum_constraint", ...)
  Constraint$min_sum <- min_sum
  Constraint$max_sum <- max_sum
  return(Constraint)
}

#' check function for constraints
#' 
#' @param x object to test for type \code{constraint}
#' @author Brian G. Peterson
#' @export
is.constraint <- function( x ) {
  inherits( x, "constraint" )
}

#'  Helper function to get the enabled constraints out of the portfolio object
#'  
#'  When the v1_constraint object is instantiated via constraint, the arguments
#'  min_sum, max_sum, min, and max are either specified by the user or default
#'  values are assigned. These are required by other functions such as
#'  \code{optimize.portfolio} and \code{constrained_objective} . This function 
#'  will check that these variables are in the portfolio object in the 
#'  constraints list. We will default to \code{min_sum=1} and \code{max_sum=1}
#'  if leverage constraints are not specified. We will default to \code{min=-Inf}
#'  and \code{max=Inf} if box constraints are not specified.
#'  This function is used at the beginning of optimize.portfolio and other 
#'  functions to extract the constraints from the portfolio object. We Use the 
#'  same naming as the v1_constraint object.
#'  
#' @param portfolio an object of class 'portfolio'
#' @return an object of class 'constraint' which is a flattened list of enabled constraints
#' @author Ross Bennett
#' @seealso \code{\link{portfolio.spec}}
get_constraints <- function(portfolio){
  if(!is.portfolio(portfolio)) stop("portfolio passed in is not of class portfolio")
  
  if(length(portfolio$constraints) == 0) stop("No constraints passed in")
  
  out <- list()
  out$min_sum <- NA
  out$max_sum <- NA
  out$min <- NA
  out$max <- NA
  
  for(constraint in portfolio$constraints) {
    if(constraint$enabled){
      if(inherits(constraint, "weight_sum_constraint")){
        out$min_sum <- constraint$min_sum
        out$max_sum <- constraint$max_sum
      }
      if(inherits(constraint, "box_constraint")){
        out$min <- constraint$min
        out$max <- constraint$max
      }
      if(inherits(constraint, "group_constraint")){
        out$groups <- constraint$groups
        out$group_labels <- constraint$group_labels
        out$cLO <- constraint$cLO
        out$cUP <- constraint$cUP
        out$group_pos <- constraint$group_pos
      }
      if(inherits(constraint, "turnover_constraint")){
        out$turnover_target <- constraint$turnover_target
      }
      if(inherits(constraint, "diversification_constraint")){
        out$div_target <- constraint$div_target
        out$conc_aversion <- constraint$conc_aversion
      }
      if(inherits(constraint, "position_limit_constraint")){
        out$max_pos <- constraint$max_pos
        out$max_pos_long <- constraint$max_pos_long
        out$max_pos_short <- constraint$max_pos_short
      }
      if(inherits(constraint, "return_constraint")){
        out$return_target <- constraint$return_target
      }
      if(inherits(constraint, "factor_exposure_constraint")){
        out$B <- constraint$B
        out$lower <- constraint$lower
        out$upper <- constraint$upper
      }
      if(inherits(constraint, "transaction_cost_constraint")){
        out$ptc <- constraint$ptc
      }
      if(inherits(constraint, "leverage_exposure_constraint")){
        out$leverage <- constraint$leverage
      }
    }
  }
  
  # min_sum, max_sum, min, and max are required to be passed in and enabled
  if(is.na(out$min_sum) | is.na(out$max_sum)) {
    # return(NULL)
    # stop("Leverage constraint min_sum and max_sum are not enabled or passed in")
    # Default to full investment constraint
    out$min_sum <- 1
    out$max_sum <- 1
  }
  if(length(out$min) == 1 | length(out$max) == 1) {
    if(is.na(out$min) | is.na(out$max)){
      # return(NULL)
      # stop("Box constraints min and max are not enabled or passed in")
      # Default to min=-Inf and max=Inf for unconstrained weights
      nassets <- length(portfolio$assets)
      out$min <- rep(-Inf, nassets)
      out$max <- rep(Inf, nassets)
    }
  }
  # structure and return class of type constraint
  return(structure(out, class="constraint"))
}

#' constructor for turnover_constraint
#' 
#' The turnover constraint specifies a target turnover value. 
#' This function is called by add.constraint when type="turnover" is specified, see \code{\link{add.constraint}}.
#' Turnover is calculated from a set of initial weights. Turnover is 
#' computed as \code{sum(abs(initial_weights - weights)) / N} where \code{N} is
#' the number of assets.
#' 
#' Note that with the ROI solvers, turnover constraint is currently only 
#' supported for the global minimum variance and quadratic utility problems 
#' with ROI quadprog plugin.
#' 
#' @param type character type of the constraint
#' @param turnover_target target turnover value
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @return an object of class 'turnover_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.6)
#' @export
turnover_constraint <- function(type="turnover", turnover_target, enabled=TRUE, message=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="turnover_constraint", ...)
  Constraint$turnover_target <- turnover_target
  return(Constraint)
}

#' constructor for diversification_constraint
#' 
#' The diversification constraint specifies a target diversification value. 
#' This function is called by add.constraint when type="diversification" is 
#' specified, see \code{\link{add.constraint}}. Diversification is computed
#' as \code{1 - sum(weights^2)}.
#' 
#' @param type character type of the constraint
#' @param div_target diversification target value
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify diversification constraint
#' an object of class 'diversification_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)
#' @export
diversification_constraint <- function(type="diversification", div_target=NULL, enabled=TRUE, message=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="diversification_constraint", ...)
  Constraint$div_target <- div_target
  return(Constraint)
}

#' constructor for return_constraint
#' 
#' The return constraint specifes a target mean return value.
#' This function is called by add.constraint when type="return" is specified, \code{\link{add.constraint}}
#' 
#' @param type character type of the constraint
#' @param return_target return target value
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters
#' @return an object of class 'return_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="return", return_target=mean(colMeans(ret)))
#' @export
return_constraint <- function(type="return", return_target, enabled=TRUE, message=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="return_constraint", ...)
  Constraint$return_target <- return_target
  return(Constraint)
}

#' constructor for position_limit_constraint
#' 
#' This function is called by add.constraint when type="position_limit" is specified, \code{\link{add.constraint}}
#' Allows the user to specify the maximum number of positions (i.e. number of assets with non-zero weights)
#' as well as the maximum number of long and short positions.
#' 
#' @param type character type of the constraint
#' @param assets named vector of assets specifying initial weights
#' @param max_pos maximum number of assets with non-zero weights
#' @param max_pos_long maximum number of assets with long (i.e. buy) positions
#' @param max_pos_short maximum number of assets with short (i.e. sell) positions
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify position limit constraints
#' @return an object of class 'position_limit_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)
#' pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos_long=3, max_pos_short=1)
#' @export
position_limit_constraint <- function(type="position_limit", assets, max_pos=NULL, max_pos_long=NULL, max_pos_short=NULL, enabled=TRUE, message=FALSE, ...){
  # Get the length of the assets vector
  nassets <- length(assets)
  
  # Checks for max_pos
  if(!is.null(max_pos)){
    if(length(max_pos) != 1) stop("max_pos must be a scalar value of length 1")
    if(max_pos < 0) stop("max_pos must be a positive value")
    if(max_pos > nassets){
      warning("max_pos must be less than or equal to the number of assets")
      max_pos <- nassets
    }
    # coerce to integer
    max_pos <- as.integer(max_pos)
  }
  
  # Checks for max_pos_long
  if(!is.null(max_pos_long)){
    if(length(max_pos_long) != 1) stop("max_pos_long must be a scalar value of length 1")
    if(max_pos_long < 0) stop("max_pos_long must be a positive value")
    if(max_pos_long > nassets){
      warning("max_pos_long must be less than or equal to the number of assets")
      max_pos_long <- nassets
    }
    # coerce to integer
    max_pos_long <- as.integer(max_pos_long)
  }
  
  # Checks for max_pos_short
  if(!is.null(max_pos_short)){
    if(length(max_pos_short) != 1) stop("max_pos_short must be a scalar value of length 1")
    if(max_pos_short < 0) stop("max_pos_short must be a positive value")
    if(max_pos_short > nassets){
      warning("max_pos_short must be less than or equal to the number of assets")
      max_pos_short <- nassets
    }
    # coerce to integer
    max_pos_short <- as.integer(max_pos_short)
  }
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="position_limit_constraint", ...)
  Constraint$max_pos <- max_pos
  Constraint$max_pos_long <- max_pos_long
  Constraint$max_pos_short <- max_pos_short
  return(Constraint)
}

#' Constructor for factor exposure constraint
#' 
#' The factor exposure constraint sets upper and lower bounds on exposures to risk factors.
#' This function is called by add.constraint when type="factor_exposure" is specified, see \code{\link{add.constraint}}
#' 
#' \code{B} can be either a vector or matrix of risk factor exposures (i.e. betas).
#' If \code{B} is a vector, the length of \code{B} must be equal to the number of 
#' assets and lower and upper must be scalars. If \code{B} is passed in as a vector,
#' it will be converted to a matrix with one column.
#' 
#' If \code{B} is a matrix, the number of rows must be equal to the number 
#' of assets and the number of columns represent the number of  factors. The length
#' of lower and upper must be equal to the number of factors. The \code{B} matrix should
#' have column names specifying the factors and row names specifying the assets.
#' Default column names and row names will be assigned if the user passes in a 
#' \code{B} matrix without column names or row names.
#' 
#' @param type character type of the constraint
#' @param assets named vector of assets specifying initial weights
#' @param B vector or matrix of risk factor exposures
#' @param lower vector of lower bounds of constraints for risk factor exposures
#' @param upper vector of upper bounds of constraints for risk factor exposures
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify risk factor exposure constraints
#' @return an object of class 'factor_exposure_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @export
factor_exposure_constraint <- function(type="factor_exposure", assets, B, lower, upper, enabled=TRUE, message=FALSE, ...){
  # Number of assets
  nassets <- length(assets)
  
  # Assume the user has passed in a vector of betas
  if(is.vector(B)){
    # The number of betas must be equal to the number of assets
    if(length(B) != nassets) stop("length of B must be equal to number of assets")
    # The user passed in a vector of betas, lower and upper must be scalars
    if(length(lower) != 1) stop("lower must be a scalar")
    if(length(upper) != 1) stop("upper must be a scalar")
    bnames <- names(B)
    B <- matrix(B, ncol=1, dimnames=list(bnames))
  }
  # The user has passed in a matrix for B
  if(is.matrix(B)){
    # The number of rows in B must be equal to the number of assets
    if(nrow(B) != nassets) stop("number of rows of B must be equal to number of assets")
    # The user passed in a matrix for B --> lower and upper must be equal to the number of columns in the beta matrix
    if(length(lower) != ncol(B)) stop("length of lower must be equal to the number of columns in the B matrix")
    if(length(upper) != ncol(B)) stop("length of upper must be equal to the number of columns in the B matrix")
    if(is.null(colnames(B))){
      # The user has passed in a B matrix without column names specifying factors
      colnames(B) <- paste("factor", 1:ncol(B), sep="")
    }
    if(is.null(rownames(B))){
      # The user has passed in a B matrix without row names specifying assets
      rownames(B) <- names(assets)
    }
  }
  
  Constraint <- constraint_v2(type=type, enabled=enabled, constrclass="factor_exposure_constraint", ...)
  Constraint$B <- B
  Constraint$lower <- lower
  Constraint$upper <- upper
  return(Constraint)
}

#' constructor for transaction_cost_constraint
#' 
#' The transaction cost constraint specifies a proportional cost value. 
#' This function is called by add.constraint when type="transaction_cost" is specified, see \code{\link{add.constraint}}.
#' 
#' Note that with the ROI solvers, proportional transaction cost constraint is 
#' currently only supported for the global minimum variance and quadratic 
#' utility problems with ROI quadprog plugin.
#' 
#' @param type character type of the constraint
#' @param assets number of assets, or optionally a named vector of assets specifying initial weights
#' @param ptc proportional transaction cost value
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @return an object of class 'transaction_cost_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="transaction_cost", ptc=0.01)
#' @export
transaction_cost_constraint <- function(type="transaction_cost", assets, ptc, enabled=TRUE, message=FALSE, ...){
  nassets <- length(assets)
  if(length(ptc) == 1) ptc <- rep(ptc, nassets)
  if(length(ptc) != nassets) stop("length of ptc must be equal to number of assets")
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="transaction_cost_constraint", ...)
  Constraint$ptc <- ptc
  return(Constraint)
}

#' constructor for leverage_exposure_constraint
#' 
#' The leverage_exposure constraint specifies a maximum leverage where 
#' leverage is defined as the sum of the absolute value of the weights. 
#' Leverage exposure is computed as the sum of the absolute value of the
#' weights, \code{sum(abs(weights))}.
#' 
#' 
#' This should be used for constructing, for example, 130/30 portfolios or 
#' dollar neutral portfolios with 2:1 leverage. For the ROI solvers, this is 
#' implemented as a MILP problem and is not supported for problems formulated 
#' as a quadratic programming problem. This may change in the future if a MIQP
#' solver is added.
#' 
#' This function is called by add.constraint when type="leverage_exposure" 
#' is specified, see \code{\link{add.constraint}}.
#' 
#' @param type character type of the constraint
#' @param leverage maximum leverage value
#' @param enabled TRUE/FALSE
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @param \dots any other passthru parameters to specify diversification constraint
#' an object of class 'diversification_constraint'
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @examples
#' data(edhec)
#' ret <- edhec[, 1:4]
#' 
#' pspec <- portfolio.spec(assets=colnames(ret))
#' 
#' pspec <- add.constraint(portfolio=pspec, type="leverage_exposure", leverage=1.6)
#' @export
leverage_exposure_constraint <- function(type="leverage_exposure", leverage=NULL, enabled=TRUE, message=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="leverage_exposure_constraint", ...)
  Constraint$leverage <- leverage
  return(Constraint)
}

#' function for updating constrints, not well tested, may be broken
#' 
#' can we use the generic update.default function?
#' @param object object of type \code{\link{constraint}} to update
#' @param ... any other passthru parameters, used to call \code{\link{constraint}}
#' @author bpeterson
#' @method update constraint
#' @S3method update constraint
#' @export
update.constraint <- function(object, ...){
  constraints <- object
  if (is.null(constraints) | !is.constraint(constraints)){
    stop("you must pass in an object of class constraints to modify")
  }
  call <- object$call
  if (is.null(call))
      stop("need an object with call component")
  extras <- match.call(expand.dots = FALSE)$...
#   if (!missing(formula.))
#       call$formula <- update.formula(formula(object), formula.)
  if (length(extras)) {
      existing <- !is.na(match(names(extras), names(call)))
      for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
      if (any(!existing)) {
          call <- c(as.list(call), extras[!existing])
          call <- as.call(call)
      }
  }
#   if (hasArg(nassets)){
#     warning("changing number of assets may modify other constraints")
#     constraints$nassets<-nassets
#   }
#   if(hasArg(min)) {
#     if (is.vector(min) & length(min)!=nassets){
#       warning(paste("length of min !=",nassets))
#       if (length(min)<nassets) {stop("length of min must be equal to lor longer than nassets")}
#       constraints$min<-min[1:nassets]
#     }
#   }
#   if(hasArg(max)) {
#     if (is.vector(max) & length(max)!=nassets){
#       warning(paste("length of max !=",nassets))
#       if (length(max)<nassets) {stop("length of max must be equal to lor longer than nassets")}
#       constraints$max<-max[1:nassets]
#     }
#   }
#   if(hasArg(min_mult)){constrains$min_mult=min_mult}
#   if(hasArg(max_mult)){constrains$max_mult=max_mult}
  return(constraints)
}

#' Insert a list of constraints into the constraints slot of a portfolio object
#' 
#' This is a helper function primarily for backwards compatibility to insert
#' constraints from a 'v1_constraint' object into the v2 'portfolio' object.
#' 
#' @param portfolio object of class 'portfolio'
#' @param constraints list of constraint objects
#' @author Ross Bennett
insert_constraints <- function(portfolio, constraints){
  # Check portfolio object
  if (is.null(portfolio) | !is.portfolio(portfolio)){
    stop("you must pass in an object of class portfolio")
  }
  
  # Check that constraints is a list
  if(!is.list(constraints)) stop("constraints must be passed in as a list")
  
  # Check that all objects in the list are of class constraint
  for(i in 1:length(constraints)){
    if(!is.constraint(constraints[[i]]))
      stop("constraints must be passed in as a list and all objects in constraints must be of class 'constraint'")
  }
  
  portfolio$constraints <- constraints
  return(portfolio)
}

#' Helper function to update v1_constraint objects to v2 specification in the portfolio object
#' 
#' The function takes the constraints and objectives specified in the v1_constraint
#' object and updates the portfolio object with those constraints and objectives. This
#' function is used inside optimize.portfolio to maintain backwards compatibility
#' if the user passes in a v1_constraint object for the constraint arg in
#' optimize.portfolio.
#' 
#' @param portfolio portfolio object passed into optimize.portfolio
#' @param v1_constraint object of type v1_constraint passed into optimize.portfolio
#' @return portfolio object containing constraints and objectives from v1_constraint
#' @author Ross Bennett
#' @seealso \code{\link{portfolio.spec}}, \code{\link{add.constraint}}
#' @export
update_constraint_v1tov2 <- function(portfolio, v1_constraint){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  if(!inherits(v1_constraint, "v1_constraint")) stop("v1_constraint object must be of class 'v1_constraint'")
  # Put the assets and weight_seq into slots in portfolio object
  portfolio$assets <- v1_constraint$assets
  portfolio$weight_seq <- v1_constraint$weight_seq
  
  # The v1_constraint object supported 3 constraint types (weight_sum, box, and group)
  # Add weight_sum/leverage constraints from v1_constraint to portfolio
  if(!is.null(v1_constraint$min_sum) & !is.null(v1_constraint$max_sum)){
    portfolio <- add.constraint(portfolio=portfolio, type='weight_sum', min_sum=v1_constraint$min_sum, max_sum=v1_constraint$max_sum)
  }
  # Add box constraints from v1_constraint to portfolio
  if(!is.null(v1_constraint$min) & !is.null(v1_constraint$max)){
    portfolio <- add.constraint(portfolio=portfolio, type='box', min=v1_constraint$min, max=v1_constraint$max)
  }
  # Add group constraints from v1_constraint to portfolio
  if(!is.null(v1_constraint$groups) & !is.null(v1_constraint$cLO) & !is.null(v1_constraint$cUP)){
    portfolio <- add.constraint(portfolio=portfolio, type='group', groups=v1_constraint$groups, group_min=v1_constraint$cLO, group_max=v1_constraint$cUP)
  }
  
  # Put the objectives from v1_constraint into the objectives slot in the portfolio 
  # object. This overwrites what might already be in portfolio$objectives assuming 
  # the user is using the v1_constraint object to specify the objectives
  portfolio$objectives <- v1_constraint$objectives
  return(portfolio)
}

#' check if a set of weights satisfies the constraints
#' 
#' This function checks if a set of weights satisfies all constraints. This is
#' used as a helper function for random portfolios created with \code{rp_simplex}
#' and \code{rp_grid} to eliminate portfolios that do not satisfy the constraints.
#' 
#' @param weights vector of weights
#' @param portfolio object of class 'portfolio'
#' @return TRUE if all constraints are satisfied, FALSE if any constraint is violated
#' @author Ross Bennett
check_constraints <- function(weights, portfolio){
  
  # get the constraints to check
  # We will check leverage, box, group, and position limit constraints
  constraints <- get_constraints(portfolio)
  min_sum <- constraints$min_sum
  max_sum <- constraints$max_sum
  min <- constraints$min
  max <- constraints$max
  groups <- constraints$groups
  cLO <- constraints$cLO
  cUP <- constraints$cUP
  group_pos <- constraints$group_pos
  div_target <- constraints$div_target
  turnover_target <- constraints$turnover_target
  max_pos <- constraints$max_pos
  max_pos_long <- constraints$max_pos_long
  max_pos_short <- constraints$max_pos_short
  leverage_exposure <- constraints$leverage
  tolerance <- .Machine$double.eps^0.5
  
  log_vec <- c()
  # check leverage constraints
  if(!is.null(min_sum) & !is.null(max_sum)){
    # TRUE if constraints are satisfied
    log_vec <- c(log_vec, ((sum(weights) >= min_sum) & (sum(weights) <= max_sum)))
  }
  
  # check box constraints
  if(!is.null(min) & !is.null(max)){
    # TRUE if constraints are satisfied
    log_vec <- c(log_vec, (all(weights >= min) & all(weights <= max)))
  }
  
  # check group constraints
  if(!is.null(groups) & !is.null(cLO) & !is.null(cUP)){
    log_vec <- c(log_vec, all(!group_fail(weights, groups, cLO, cUP, group_pos)))
  }
  
  # check position limit constraints
  if(!is.null(max_pos) | !is.null(max_pos_long) | !is.null(max_pos_short)){
    log_vec <- c(log_vec, !pos_limit_fail(weights, max_pos, max_pos_long, max_pos_short))
  }
  
  # check leverage exposure constraints
  if(!is.null(leverage_exposure)){
    log_vec <- c(log_vec, sum(abs(weights)) <= leverage_exposure)
  }
  # return TRUE if all constraints are satisfied, FALSE if any constraint is violated
  return(all(log_vec))
}

# #' constructor for class constraint_ROI
# #' 
# #' @param assets number of assets, or optionally a named vector of assets specifying seed weights
# #' @param op.problem an object of type "OP" (optimization problem, of \code{ROI}) specifying the complete optimization problem, see ROI help pages for proper construction of OP object.
# #' @param solver string argument for what solver package to use, must have ROI plugin installed for that solver.  Currently support is for \code{glpk} and \code{quadprog}.
# #' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
# #' @author Hezky Varon
# #' @export
# constraint_ROI <- function(assets, op.problem, solver=c("glpk", "quadprog"), weight_seq=NULL) 
# {
#   if(op.problem == NULL || inherits(op.problem, "OP")) {
#     stop("Need to pass in optimiztion problem of ROI:::OP type.")
#   if() stop("Need to be ROI:::OP")
#   return(structure(
#     list(
#       assets = assets,
#       constrainted_objective = op.problem,
#       solver = solver[1],
#       weight_seq = weight_seq,
#       objectives = list(),
#       call = match.call()
#     ), 
#     class=c("constraint_ROI","constraint")
#   ))
# }

