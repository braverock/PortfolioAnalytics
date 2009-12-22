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
#' @param name name of the objective which will be used to call a function, like 'ES', 'VaR', 'mean'
#' @param enabled TRUE/FALSE
#' @param \dots any other passthrough parameters
#' @param multiplier 
#' @author bpeterson
#' @export
#' @callGraph
objective<-function(name , enabled=FALSE , ..., multiplier=1){
  if(!hasArg(name)) stop("you must specify an objective name")
  if (hasArg(name)) if(is.null(name)) stop("you must specify an objective name")
  
  ## now structure and return
  return(structure( list(name = name,
                         enabled = enabled,
                         multiplier = multiplier
                         #call = match.call()
                        ),
                    class="objective"
                  ) # end structure
  )
}


#' @param x 
#' @author bpeterson
#' @export
is.objective <- function( x ) {
  inherits( x, "objective" )
}

#' General interface for adding optimization objectives, including risk, return, and risk budget
#' 
#' @param constraints 
#' @param type 
#' @param name 
#' @param enabled 
#' @param ... 
#' @param indexnum 
#' @author bpeterson
#' @export
#' @callGraph
add.objective <- function(constraints, type, name, enabled=FALSE, ..., indexnum=NULL)
{
    if (!is.constraint(constraints)) {stop("constraints passed in are not of class constraint")}

    if (!hasArg(name)) stop("you must supply a name for the objective")
    if (!hasArg(type)) stop("you must supply a type of objective to create")
    if (!hasArg(enabled)) enabled=FALSE
 
    

    assets=constraints$assets
    
    tmp_objective=NULL
	
    switch(type,
        return=, return_objective=
          {tmp_objective = return_objective(name=name,
                                            enabled=enabled,
                                            if (hasArg(target)) target=match.call(expand.dots=TRUE)$target else target = NULL,
                                            if (hasArg(multiplier)) multiplier=match.call(expand.dots=TRUE)$multiplier else multiplier = -1,
                                            ... = ...
                                            )
          },

        risk=, portfiolio_risk=, portfolio_risk_objective =
          {tmp_objective = portfolio_risk_objective(name=name,
                                                    enabled=enabled,
                                                    if (hasArg(p)) p=match.call(expand.dots=TRUE)$p else p=.95,
                                                    if (hasArg(multiplier)) multiplier=match.call(expand.dots=TRUE)$multiplier else multiplier = 1,
                                                    if (hasArg(target)) target=match.call(expand.dots=TRUE)$target else target = NULL,
                                                    ...=...
                                                   )
          },

        risk_budget=, risk_budget_objective=
          {tmp_objective = risk_budget_objective(name=name,
                                                 enabled=enabled,
                                                 if (hasArg(p)) p=match.call(expand.dots=TRUE)$p else p=.95,
                                                 if (hasArg(multiplier)) multiplier=match.call(expand.dots=TRUE)$multiplier else multiplier = 1,
                                                 if (hasArg(target)) target=match.call(expand.dots=TRUE)$target else target = NULL,
                                                 ...=...
                                                )
          },
          
        null =
          {return(constraints)} # got nothing, default to simply returning
    ) # end objective type switch
    if(is.objective(tmp_objective)) {
       if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(constraints$objectives)+1
       tmp_objective$call<-match.call()
       constraints$objectives[[indexnum]]<-tmp_objective
    }
    return(constraints)
}

# update.objective <- function(object, ...) {
#   # here we do a bunch of magic to update the correct index'd objective
# 
#   constraints <- object
# 
#   if (is.null(constraints) | !is.constraint(constraints)){
#     stop("you must pass in an object of class constraints to modify")
#   }
# 
#   
# }


#' 
#' @param name 
#' @param enabled 
#' @param ... 
#' @param multiplier 
#' @param target 
#' @author bpeterson
#' @export
return_objective <- function(name, enabled=FALSE, ... ,multiplier=-1, target=NULL)
{
  if(!hasArg(target)) target = NULL
  if(!hasArg(multiplier)) multiplier=1
  if(!hasArg(method)) method = NULL
  Objective <- objective(name=name,enabled=enabled, multiplier=multiplier)
  ##' if target is null, we'll try to maximize the return metric
  ##' if target is set, we'll try to meet or exceed the metric, penalizing a shortfall
  ## now structure and return
  return(structure( list(name = Objective$name,
                         enabled = Objective$enabled,
                         method = method,
                         multiplier= Objective$multiplier,
                         target=target
                         #call = Objective$call
                      ), # end of list
                    class=c("return_objective","objective")
                  ) # end structure
  )
  
} # end return_objective constructor

#' 
#' @param name 
#' @param enabled 
#' @param ... 
#' @param multiplier 
#' @param target 
#' @param p 
#' @author bpeterson
#' @export
portfolio_risk_objective <- function(name, enabled=FALSE, ... ,  multiplier=1, target=NULL, p=.95)
{
  Objective <- objective(name=name,enabled=enabled, multiplier=multiplier)
  ##' if target is null, we'll try to minimize the risk metric
  if (!is.numeric(p)) stop("your p value must be numeric")
  if (p>1) stop("p must be less than 1")
  if (p<0) stop("p must be greater than zero")
  if(!hasArg(clean)) clean = NULL else clean = match.call(expand.dots=TRUE)$clean
  if(!hasArg(method)) method = NULL else method = match.call(expand.dots=TRUE)$method
  if(!hasArg(portfolio_method)) portfolio_method = NULL else portfolio_method = match.call(expand.dots=TRUE)$portfolio_method
  if(!hasArg(target)) target = NULL else target = match.call(expand.dots=TRUE)$target
  if(!hasArg(multiplier)) multiplier=1 else multiplier = match.call(expand.dots=TRUE)$multiplier
  
  ## now structure and return
  return(structure( list(name = Objective$name,
                         enabled = Objective$enabled,
                         p = p,
                         clean = clean,
                         method = method,
                         portfolio_method = portfolio_method,
                         multiplier= Objective$multiplier,
                         target=target
                         #call = Objective$call
                        ), # end of list
                    class=c("portfolio_risk_objective","objective")
                  ) # end structure
  )
} # end portfolio_risk_objective constructor

#' 
#' @param assets 
#' @param name 
#' @param enabled 
#' @param ... 
#' @param multiplier 
#' @param target 
#' @param p 
#' @param min_prisk 
#' @param max_prisk 
#' @author bpeterson
#' @export
risk_budget_objective <- function(assets, name, enabled=FALSE, ..., multiplier=1, target=NULL, p=.95, min_prisk, max_prisk )
{
  if(!hasArg(target)) target=NULL else target=match.call(expand.dots=TRUE)$target
  Objective <- portfolio_risk_objective(name=name,enabled=enabled,p=p,target=target, multiplier=multiplier, ...=...)
  if(!hasArg(method)) method="modified" else method=match.call(expand.dots=TRUE)$method
  if(!hasArg(portfolio_method)) portfolio_method="component" else portfolio_method=match.call(expand.dots=TRUE)$portfolio_method
  
  #if( is.null(RBlower) ){ RBlower = rep(-Inf,N) }  ; if( is.null(RBupper) ){ RBupper = rep(Inf,N) }
  nassets=length(assets)
  if(hasArg(min_prisk) & hasArg(max_prisk)) {
    if (length(min_prisk)>1 & length(max_prisk)>1){
      if (length(min_prisk)!=length(max_prisk)) { stop("length of min_prisk and max_prisk must be the same") }
    }
  }
  if(hasArg(min_prisk)){
    if (length(min_prisk)==1) {
      min_prisk <- rep(min_prisk,nassets)
      names(min_prisk)<-names(assets)
    }
    if (length(min_prisk)!=nassets) stop(paste("length of min_prisk must be equal to 1 or the number of assets",nassets))
  }
  if(hasArg(max_prisk)){
    if (length(max_prisk)==1) {
      max_prisk <- rep(max_prisk,nassets)
      names(max_prisk)<-names(assets)
    }
    if (length(max_prisk)!=nassets) stop(paste("length of max_prisk must be equal to 1 or the number of assets",nassets))
  }
  
  if(!hasArg(max_prisk)) max_prisk = NULL
  if(!hasArg(min_prisk)) min_prisk = NULL
  
  return(structure( list(name = Objective$name,
                         enabled = Objective$enabled,
                         p = Objective$p,
                         clean = Objective$clean,
                         method = method,
                         portfolio_method = portfolio_method,
                         target= Objective$target,
                         multiplier= Objective$multiplier,
                         min_prisk = min_prisk,
                         max_prisk = max_prisk
                         #call = Objective$call
                      ), # end of list
                    class=c("risk_budget_objective","objective")
                  ) # end structure
  )
} # end risk_budget_objective constructor