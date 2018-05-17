#' constructor for class constraint_ROI
#' 
#' @param assets number of assets, or optionally a named vector of assets specifying seed weights
#' @param op.problem an object of type "OP" (optimization problem, of \code{ROI}) specifying the complete optimization problem, see ROI help pages for proper construction of OP object.
#' @param solver string argument for what solver package to use, must have ROI plugin installed for that solver.  Currently support is for \code{glpk} and \code{quadprog}.
#' @param weight_seq seed sequence of weights, see \code{\link{generatesequence}}
#' @author Hezky Varon
#' @export
constraint_ROI <- function(assets=NULL, op.problem, solver=c("glpk", "quadprog"), weight_seq=NULL) 
{
  #
  # Structure for this constructor function borrowed from "constraints.R"
  #
  if(is.null(op.problem) | !inherits(op.problem, "OP")) 
    stop("Need to pass in optimization problem of ROI:::OP object type.")
  
  if (is.null(assets)) {
    stop("You must specify the assets")
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
  }
  print(paste("You chose to use the ",solver[1]," solver", sep=""))
  return(structure(
    list(
         assets = assets,
         constrainted_objective = op.problem,
         solver = solver[1],
         weight_seq = weight_seq,
         objectives = list(),
         call = match.call()
        ), 
    class=c("constraint_ROI","constraint")
    ))
}


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
