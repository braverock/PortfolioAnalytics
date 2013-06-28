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

#' constructor for class constraint
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param ... any other passthru parameters
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying minimum weight box constraints
#' @param min_mult numeric or named vector specifying minimum multiplier box constraint from seed weight in \code{assets}
#' @param max_mult numeric or named vector specifying maximum multiplier box constraint from seed weight in \code{assets}
#' @param min_sum minimum sum of all asset weights, default .99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
#' @author Peter Carl and Brian G. Peterson
#' @examples 
#' exconstr <- constraint(assets=10, min_sum=1, max_sum=1, min=.01, max=.35, weight_seq=generatesequence())
#' @export
constraint <- function(assets=NULL, ... ,min,max,min_mult,max_mult,min_sum=.99,max_sum=1.01,weight_seq=NULL)
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
        message("assuming equal weighted seed portfolio")
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
      message("assuming equal weighted seed portfolio")
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
  ##now adjust min and max to account for min_mult and max_mult from seed
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


#' constructor for class v2_constraint
#' 
#' @param type character type of the constraint to add or update, currently 'weight_sum', 'box', or 'group'
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param ... any other passthru parameters
#' @param constrclass character to name the constraint class
#' @author Ross Bennett
#' @export
constraint_v2 <- function(type, enabled=FALSE, ..., constrclass="v2_constraint"){
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
#' This is the main function for adding and/or updating constraints in an object of type \code{\link{portfolio}}.
#' 
#' In general, you will define your constraints as: 'weight_sum', 'box', 'group', 'turnover', 'diversification', 'volatility', or 'position_limit'.  
#' 
#' @param portfolio an object of class 'portfolio' to add the constraint to, specifying the constraints for the optimization, see \code{\link{portfolio.spec}}
#' @param type character type of the constraint to add or update, currently 'weight_sum', 'box', 'group', 'turnover', 'diversification', or 'volatility'
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @param indexnum if you are updating a specific constraint, the index number in the $objectives list to update
#' @author Ross Bennett
#' @seealso \code{\link{constraint_v2}}, \code{\link{weight_sum_constraint}}, \code{\link{box_constraint}}, \code{\link{group_constraint}}, \code{\link{turnover_constraint}}, \code{\link{diversification_constraint}}, \code{\link{volatility_constraint}}, \code{\link{position_limit_constraint}}
#' @export
add.constraint <- function(portfolio, type, enabled=FALSE, ..., indexnum=NULL){
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
                                                 ...=...)
         },
         # Group constraints
         group = {tmp_constraint <- group_constraint(assets=assets, 
                                                     type=type,
                                                     enabled=enabled,
                                                     ...=...)
         },
         # Sum of weights constraints
         weight=, weight_sum = {tmp_constraint <- weight_sum_constraint(type=type,
                                                                        enabled=enabled,
                                                                        ...=...)
         },
         # Turnover constraint
         turnover = {tmp_constraint <- turnover_constraint(type=type,
                                                           enabled=enabled,
                                                           ...=...)
         },
         # Diversification constraint
         diversification = {tmp_constraint <- diversification_constraint(type=type,
                                                                         enabled=enabled,
                                                                         ...=...)
         },
         # Volatility constraint
         volatility = {tmp_constraint <- volatility_constraint(type=type,
                                                               enabled=enabled,
                                                               ...=...)
         },
         # Position limit constraint
         position_limit = {tmp_constraint <- position_limit_constraint(type=type,
                                                                       enabled=enabled,
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
#' This function is called by add.constraint when type="box" is specified. see \code{\link{add.constraint}}
#'
#' @param type character type of the constraint
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param min numeric or named vector specifying minimum weight box constraints
#' @param max numeric or named vector specifying minimum weight box constraints
#' @param min_mult numeric or named vector specifying minimum multiplier box constraint from seed weight in \code{assets}
#' @param max_mult numeric or named vector specifying maximum multiplier box constraint from seed weight in \code{assets}
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @export
box_constraint <- function(type, assets, min, max, min_mult, max_mult, enabled=FALSE, ...){
  # Based on the constraint function for object of class constraint_v1 that
  # included specifying box constraints.
  
  # Get the length of the assets vector
  nassets <- length(assets)
  
  # Check that the length of min and max are the same
  if(hasArg(min) | hasArg(max)) {
    if (length(min) > 1 & length(max) > 1){
      if (length(min) != length(max)) { stop("length of min and max must be the same") }
    } 
    
    # If the user passes in a scalar for min, then create a min vector 
    if (length(min) == 1) {
      message("min not passed in as vector, replicating min to length of length(assets)")
      min <- rep(min, nassets)
    }
    if (length(min) != nassets) stop(paste("length of min must be equal to 1 or the number of assets:", nassets))
    
    # If the user passes in a scalar for max, then create a max vector
    if (length(max) == 1) {
      message("max not passed in as vector, replicating max to length of length(assets)")
      max <- rep(max, nassets)
    }
    if (length(max) != nassets) stop(paste("length of max must be equal to 1 or the number of assets:", nassets))
    
  } else {
    # Default to min=0 and max=1 if min or max are not passed in
    message("no min or max passed in, assuming 0 and 1")
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
      message("min_mult and max_mult not passed in as vectors, replicating min_mult and max_mult to length of assets vector")
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
  
  # now adjust min and max to account for min_mult and max_mult from seed
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
#' This function is called by add.constraint when type="group" is specified. see \code{\link{add.constraint}}
#'
#' @param type character type of the constraint
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param groups vector specifying the groups of the assets
#' @param group_min numeric or vector specifying minimum weight group constraints
#' @param group_max numeric or vector specifying minimum weight group constraints
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @seealso \code{\link{add.constraint}}
#' @export
group_constraint <- function(type, assets, groups, group_min, group_max, enabled=FALSE, ...) {
  nassets <- length(assets)
  ngroups <- length(groups)
  
  if(sum(groups) != nassets) {
    stop("sum of groups must be equal to the number of assets")
  }
  
  # Checks for group_min
  if (length(group_min) == 1) {
    message("group_min not passed in as vector, replicating group_min to length of groups")
    group_min <- rep(group_min, ngroups)
  }
  if (length(group_min) != ngroups) stop(paste("length of group_min must be equal to 1 or the length of groups:", ngroups))
  
  # Checks for group_max
  if (length(group_max) == 1) {
    message("group_max not passed in as vector, replicating group_max to length of groups")
    group_max <- rep(group_max, ngroups)
  }
  if (length(group_max) != ngroups) stop(paste("length of group_max must be equal to 1 or the length of groups:", ngroups))
  
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="group_constraint", ...)
  Constraint$groups <- groups
  Constraint$cLO <- group_min
  Constraint$cUP <- group_max
  return(Constraint)
}

#' constructor for weight_sum_constraint
#' 
#' This function is called by add.constraint when type="weight_sum" is specified. see \code{\link{add.constraint}}
#' This function allows the user to specify the minimum and maximum that the weights sum to
#' 
#' @param type character type of the constraint
#' @param min_sum minimum sum of all asset weights, default 0.99
#' @param max_sum maximum sum of all asset weights, default 1.01
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @export
weight_sum_constraint <- function(type, min_sum=0.99, max_sum=1.01, enabled=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="weight_sum_constraint", ...)
  Constraint$min_sum <- min_sum
  Constraint$max_sum <- max_sum
  return(Constraint)
}

#' check function for constraints
#' 
#' @param x object to test for type \code{constraint}
#' @author bpeterson
#' @export
is.constraint <- function( x ) {
  inherits( x, "constraint" )
}

#'  Helper function to get the enabled constraints out of the portfolio object, see \code{\link{portfolio.spec}}
#'  
#'  When the v1_constraint object is instantiated via constraint, the arguments
#'  min_sum, max_sum, min, and max are either specified by the user or default
#'  values are assigned. These are required by other functions such as
#'  optimize.portfolio. This function will check that these variables are in
#'  the portfolio object in the constraints list. This function could be used
#'  at the beginning of optimize.portfolio to check the constraints in the 
#'  portfolio object.
#' 
#'  Returns an object of class constraint which is a flat list of weight_sum, box, and group constraints.
#'  Uses the same naming as the v1_constraint object which may be useful when passed to other functions.
#'  @param portfolio an object of class 'portfolio'
#'  @author Ross Bennett
#'  @seealso \code{\link{portfolio.spec}}, \code{\link{constraint_v2}}
#'  @export
get.constraints <- function(portfolio){
  # Check that object passed in is a portfolio objec
  if(!is.portfolio(portfolio)) stop("portfolio passed in is not of class portfolio")
  
  tmp.constraints <- portfolio$constraints
  
  # Check that constraints are passed in
  if(length(tmp.constraints) == 0) stop("No constraints passed in")
  
  out <- list()
  
  # Required constraints
  out$min_sum <- NULL
  out$max_sum <- NULL
  out$min <- NULL
  out$max <- NULL
  
  for(i in 1:length(tmp.constraints)){
    if(tmp.constraints[[i]]$enabled){
      # weight_sum constraint
      if(tmp.constraints[[i]]$type == "weight_sum"){
        # Extract min_sum and max_sum
        out$min_sum <- tmp.constraints[[i]]$min_sum
        out$max_sum <- tmp.constraints[[i]]$max_sum
      }
      # box constraints
      if(tmp.constraints[[i]]$type == "box"){
        # Extract min and max
        out$min <- tmp.constraints[[i]]$min
        out$max <- tmp.constraints[[i]]$max
      }
      # group constraints
      if(tmp.constraints[[i]]$type == "group"){
        # Extract groups, cLO, and cUP
        out$groups <- tmp.constraints[[i]]$groups
        out$cLO <- tmp.constraints[[i]]$cLO
        out$cUP <- tmp.constraints[[i]]$cUP
      }
    }
  }
  # Error if no constraints are enabled
  if(length(out) == 0) stop("No constraints are enabled")
  
  # Error if required constraints are not specified
  if(is.null(out$min) | is.null(out$max) | is.null(out$max_sum) | is.null(out$min_sum)) {
    stop("Must specify weight_sum constraints (min_sum and max_sum) and box constraints ( min and max")
  }
  return(structure(out, class="constraint"))
}

#' constructor for turnover_constraint
#' 
#' This function is called by add.constraint when type="turnover" is specified. see \code{\link{add.constraint}}
#' This function allows the user to specify a target turnover value
#' 
#' Note that turnover constraint is currently only supported for global minimum 
#' variance problem with ROI quadprog plugin
#' 
#' @param type character type of the constraint
#' @param turnover.target target turnover value
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @export
turnover_constraint <- function(type, turnover.target, enabled=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="turnover_constraint", ...)
  Constraint$toc <- turnover.target
  return(Constraint)
}

#' constructor for diversification_constraint
#' 
#' This function is called by add.constraint when type="diversification" is specified, \code{\link{add.constraint}}
#' 
#' @param type character type of the constraint
#' @param div.target diversification target value
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @export
diversification_constraint <- function(type, div.target, enabled=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="diversification_constraint", ...)
  Constraint$div <- div.target
  return(Constraint)
}

#' constructor for volatility_constraint
#' 
#' This function is called by add.constraint when type="volatility" is specified, \code{\link{add.constraint}}
#' Penalize if portfolio standard deviation deviates from volatility target
#' 
#' @param type character type of the constraint
#' @param vol.target target volatilty constraint
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @export
volatility_constraint <- function(type, vol.target, enabled=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="volatility_constraint", ...)
  # Constraint$min.vol <- min.vol
  # Constraint$max.vol <- max.vol
  Constraint$vol.target <- vol.target
  return(Constraint)
}

#' constructor for position_limit_constraint
#' 
#' This function is called by add.constraint when type="position_limit" is specified, \code{\link{add.constraint}}
#' Allows the user to specify the maximum number of positions (i.e. number of assets with non-zero weights)
#' 
#' @param type character type of the constraint
#' @param max.pos maximum number of positions
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters to specify box and/or group constraints
#' @author Ross Bennett
#' @export
position_limit_constraint <- function(type, max.pos, enabled=FALSE, ...){
  Constraint <- constraint_v2(type, enabled=enabled, constrclass="position_limit_constraint", ...)
  Constraint$max.pos <- max.pos
  return(Constraint)
}

#' function for updating constrints, not well tested, may be broken
#' 
#' can we use the generic update.default function?
#' @param object object of type \code{\link{constraint}} to update
#' @param ... any other passthru parameters, used to call \code{\link{constraint}}
#' @author bpeterson
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

