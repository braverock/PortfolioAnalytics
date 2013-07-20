###############################################################################
# R (http://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2004-2012 Kris Boudt, Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: constraints.R 2362 2013-06-18 23:48:13Z rossbennett34 $
#
###############################################################################

#' constructor for class portfolio
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights. If seed weights are not specified, an equal weight portfolio will be assumed.
#' @param category_labels character vector to categorize assets by sector, industry, geography, market-cap, currency, etc.
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @author Ross Bennett
#' @examples 
#' pspec <- portfolio.spec(assets=10, weight_seq=generatesequence())
#' @export
portfolio.spec <- function(assets=NULL, category_labels=NULL, weight_seq=NULL, message=FALSE) {
  # portfolio.spec is based on the v1_constraint object, but removes
  # constraint specification
  if (is.null(assets)) {
    stop("You must specify the assets")
  }
  
  if(!is.null(assets)){
    # TODO FIXME this doesn't work quite right on matrix of assets
    if(is.numeric(assets)){
      if (length(assets) == 1) {
        nassets = assets
        # we passed in a number of assets, so we need to create the vector
        if(message) message("assuming equal weighted seed portfolio")
        assets <- rep(1 / nassets, nassets)
      } else {
        nassets = length(assets)
      }
      # and now we may need to name them
      if (is.null(names(assets))) {
        for(i in 1:length(assets)){
          names(assets)[i] <- paste("Asset",i,sep=".")
        }
      }
    }
    if(is.character(assets)){
      nassets = length(assets)
      assetnames = assets
      if(message) message("assuming equal weighted seed portfolio")
      assets <- rep(1 / nassets, nassets)
      names(assets) <- assetnames  # set names, so that other code can access it,
      # and doesn't have to know about the character vector
      # print(assets)
    }
    # if assets is a named vector, we'll assume it is current weights
  }
  
  # If category_labels is not null then the user has passed in category_labels
  if(!is.null(category_labels)){
    if(!is.character(category_labels)){
      stop("category_labels must be a character vector")
    }
    if(length(category_labels) != length(assets)) {
      stop("length(category_labels) must be equal to length(assets)")
    }
  }
  
  ## now structure and return
  return(structure(
    list(
      assets = assets,
      category_labels = category_labels,
      weight_seq = weight_seq,
      constraints = list(),
      objectives = list(),
      call = match.call()
    ),
    class=c("portfolio.spec","portfolio")
  ))
}

#' check function for portfolio
#' 
#' @param x object to test for type \code{portfolio}
#' @author Ross Bennett
#' @export
is.portfolio <- function( x ) {
  inherits( x, "portfolio" )
}