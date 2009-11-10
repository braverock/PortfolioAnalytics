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

constraint <- function(assets=NULL, ... ,min,max,min_mult,max_mult,min_sum,max_sum,weight_seq)
{ # based on GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
  if (hasArg(min) & hasArg(max)) {
    if (is.null(assets) & (!length(min)>1) & (!length(max)>1)) {
      stop("You must either specify the assets or pass a vector for both min and max")
    }
  }

  if(!is.null(assets)){
    if(is.numeric(assets)){
      nassets=assets
      #we passed in a number of assets, so we need to name them and create the vector
      for(i in 1:assets){
        assets[i]<-paste("Asset",i,sep=".")
      }
    }
    if(is.character(assets)){
      nassets=length(assets)
      assetnames=assets
      warning("assuming equal weighted seed portfolio")
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
      warning("min and max not passed in as vectors, replicating min and max to length of length(assets)")
      min <- rep(min,nassets)
      max <- rep(max,nassets)
    }
  }

  if(hasArg(min_mult) | hasArg(max_mult)) {
    if (length(min_mult)>1 & length(max_mult)>1){
      if (length(min_mult)!=length(max_mult) ) { stop("length of min_mult and max_mult must be the same") }
    } else {
      warning("min_mult and max_mult not passed in as vectors, replicating min_mult and max_mult to length of assets vector")
      min_mult = rep(min_mult,nassets)
      max_mult = rep(max_mult,nassets)
    }
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
      objectives = list()
    ),
    class=c("v1_constraint","constraint")
  ))
}

is.constraint <- function( x ) {
  inherits( x, "constraint" )
}


modify_constraint <- function(constraints, ...){
  if (is.null(constraints) | class(constraints)!="constraint"){
    stop("you must pass in an object of class constraints to modify")
  }
  if (hasArg(nassets)){
    warning("changing number of assets may modify other constraints")
    constraints$nassets<-nassets
  }
  if(hasArg(min)) {
    if (is.vector(min) & length(min)!=nassets){
      warning(paste("length of min !=",nassets))
      if (length(min)<nassets) {stop("length of min must be equal to lor longer than nassets")}
      constraints$min<-min[1:nassets]
    }
  }
  if(hasArg(max)) {
    if (is.vector(max) & length(max)!=nassets){
      warning(paste("length of max !=",nassets))
      if (length(max)<nassets) {stop("length of max must be equal to lor longer than nassets")}
      constraints$max<-max[1:nassets]
    }
  }
  if(hasArg(min_mult)){constrains$min_mult=min_mult}
  if(hasArg(max_mult)){constrains$max_mult=max_mult}
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
