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