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

#' 
#' @param assets 
#' @param ... 
#' @param min 
#' @param max 
#' @param min_mult 
#' @param max_mult 
#' @param min_sum 
#' @param max_sum 
#' @param weight_seq 
#' @author Peter Carl and Brian G. Peterson
#' @examples 
#' exconstr <- constraint(assets=10, min_sum=1, max_sum=1, min=.01, max=.35, weight_seq=generatesequence())
#' @export
#' @callGraph
constraint <- function(assets=NULL, ... ,min,max,min_mult,max_mult,min_sum,max_sum,weight_seq)
{ # based on GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
  if (hasArg(min) & hasArg(max)) {
    if (is.null(assets) & (!length(min)>1) & (!length(max)>1)) {
      stop("You must either specify the assets or pass a vector for both min and max")
    }
  }

  if(!is.null(assets)){
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
    } else {
      message("min and max not passed in as vectors, replicating min and max to length of length(assets)")
      min <- rep(min,nassets)
      max <- rep(max,nassets)
    }
  } else {
    message("no min or max passed in, assuming 0 and 1")
    min <- rep(0,nassets)
    max <- rep(1,nassets)
    
  }

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
    min[which(tmp_min>min)]<-tmp_min[which(tmp_min>min)]
  }
  if(!is.null(max_mult) & !is.null(max)) {
    tmp_max <- assets*max_mult
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

#' 
#' @param x 
#' @author bpeterson
#' @export
is.constraint <- function( x ) {
  inherits( x, "constraint" )
}

# can we use the generic update.default function?
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

# assume a structure that includes column names for each asset
# constraints should be a list by constraint type
# we have min/max constraints on each asset
# constraint <- function (x){
#
# structure( list(L   = L,
#                   dir = dir,
#                   rhs = rhs,
#                   n_L_constraints = n_L_constraints),
#             class = c("L_constraint", "constraint"))
# }
# ultimately, I'd like to have a real portfolio class, and real constraints classes, but for now we can build Burns-style random portfolios by specifying our box constriants
