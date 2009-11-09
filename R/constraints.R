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

contraint_v1 <- function(nassets=NULL,minc,maxc,min_mult=NULL,max_mult=NULL)
{ # based on GPL R-Forge pkg roi by Stefan Thuessel,Kurt Hornik,David Meyer
  if (is.null(nassets) & !is.vector(min) & !is.vector(max)) {
    stop("You must either specify the number of assets or pass a vector for both min and max")
  }

  if (is.vector(min) & is.vector(max)){
    if (length(min)!=length(max)) { stop("length of min and max must be the same") }
    nassets = length(min)
  } else {
    warning("min and max not passed in as vectors, replicating min and max to length of nassets")
    min = rep(min,nassets)
    max = rep(max,nassets)
  }

  structure(
    list(
      nassets = nassets,
      min = min,
      max = max,
      min_mult = min_mult,
      max_mult = max_mult,
      class = c("constraint_v1","constraint")
    )
  )
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