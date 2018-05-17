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

#' constructor for class portfolio
#' 
#' The portfolio object is created with \code{portfolio.spec}. The portfolio
#' object is an S3 object of class 'portfolio' used to hold the initial asset weights,
#' constraints, objectives, and other information about the portfolio. The only
#' required argument to \code{portfolio.spec} is \code{assets}.
#' 
#' The portfolio object contains the following elements:
#' \itemize{
#' \item{\code{assets}}{ named vector of the seed weights}
#' \item{\code{category_labels}}{ character vector to categorize the assets by sector, geography, etc.}
#' \item{\code{weight_seq}}{ sequence of weights used by \code{\link{random_portfolios}}. See \code{\link{generatesequence}}}
#' \item{\code{constraints}}{ a list of constraints added to the portfolio object with \code{\link{add.constraint}}}
#' \item{\code{objectives}}{ a list of objectives added to the portfolio object with \code{\link{add.objective}}}
#' \item{\code{call}}{ the call to \code{portfolio.spec} with all of the specified arguments}
#' }
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights. If seed weights are not specified, an equal weight portfolio will be assumed.
#' @param category_labels character vector to categorize assets by sector, industry, geography, market-cap, currency, etc. Default NULL
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}} Default NULL
#' @param message TRUE/FALSE. The default is message=FALSE. Display messages if TRUE.
#' @return an object of class \code{portfolio}
#' @author Ross Bennett, Brian G. Peterson
#' @aliases portfolio
#' @seealso 
#'  \code{\link{add.constraint}},
#'  \code{\link{add.objective}},
#'  \code{\link{optimize.portfolio}}
#' @examples 
#' data(edhec)
#' pspec <- portfolio.spec(assets=colnames(edhec))
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
    # Turn category_labels into a list that can be used with group constraints
    unique_labels <- unique(category_labels)
    tmp <- list()
    for(i in 1:length(unique_labels)){
      tmp[[unique_labels[i]]] <- which(category_labels == unique_labels[i])
    }
    category_labels <- tmp
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

#' Regime Portfolios
#' 
#' Construct a \code{regime.portfolios} object that contains a time series of 
#' regimes and portfolios corresponding to the regimes.
#' 
#' Create a \code{regime.portfolios} object to support regime switching
#' optimization. This object is then passed in as the \code{portfolio}
#' argument in \code{optimize.portfolio}. The regime is detected and the
#' corresponding portfolio is selected. For example, if the current
#' regime is 1, then portfolio 1 will be selected and used in the 
#' optimization.
#' 
#' @param regime xts or zoo object specifying the regime
#' @param portfolios list of portfolios created by
#' \code{combine.portfolios} with corresponding regimes
#' @return a \code{regime.portfolios} object with the following elements
#' \itemize{
#'     \item{regime: }{An xts object of the regime}
#'     \item{portfolio: }{List of portfolios corresponding to the regime}
#'   }
#' @author Ross Bennett
#' @export
regime.portfolios <- function(regime, portfolios){
  if(!inherits(regime, c("xts", "zoo"))) stop("regime object must be an xts or zoo object")
  if(!inherits(portfolios, "portfolio.list")) stop("portfolios object must be a portfolio.list object")
  
  n.regimes <- length(unique(regime))
  n.portfolios <- length(portfolios)
  if(n.regimes != n.portfolios) stop("Number of portfolios must match the number of regimes")
  
  # Check to ensure the assets in each portfolio are equal
  for(i in 2:length(portfolios)){
    if(!identical(portfolios[[1]]$assets, portfolios[[i]]$assets)){
      stop("The assets in each portfolio must be identical")
    }
  }
  # get the unique asset names of each portfolio
  # asset names matter in hierarchical optimization
  asset.names <- unique(unlist(lapply(portfolios, function(x) names(x$assets))))
  assets <- rep(1 / length(asset.names), length(asset.names))
  names(assets) <- asset.names
  # structure and return
  return(structure(list(regime=regime, portfolio.list=portfolios, assets=assets), 
                   class=c("regime.portfolios", "portfolio")))
}
