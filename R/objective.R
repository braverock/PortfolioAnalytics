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

#' constructor for class 'objective'
#' 
#' @param name name of the objective which will be used to call a function, like 'ES', 'VaR', 'mean'
#' @param target univariate target for the objective, default NULL
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthrough parameters
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param objclass string class to apply, default 'objective'
#' @author Brian G. Peterson
#' @export
objective<-function(name , target=NULL , arguments, enabled=FALSE , ..., multiplier=1, objclass='objective'){
  if(!hasArg(name)) stop("you must specify an objective name")
  if (hasArg(name)) if(is.null(name)) stop("you must specify an objective name")
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  
  ## now structure and return
  return(structure( c(list(name = name,
                         target = target, 
                         arguments=arguments, 
                         enabled = enabled,
                         multiplier = multiplier
                         #call = match.call()
                        ),
						list(...)),
                    class=objclass
                  ) # end structure
  )
}


#' check class of an objective object
#' @param x an object potentially of type 'objective' to test
#' @author Brian G. Peterson
#' @export
is.objective <- function( x ) {
  inherits( x, "objective" )
}

#' General interface for adding optimization objectives, including risk, return, and risk budget
#' 
#' This function is the main function for adding and updating business objectives in an object of type \code{\link{constraint}}.
#' 
#' In general, you will define your objective as one of three types: 'return', 'risk', or 'risk_budget'.  
#' These have special handling and intelligent defaults for dealing with the function most likely to be 
#' used as objectives, including mean, median, VaR, ES, etc.
#' 
#' @param constraints an object of type "constraints" to add the objective to, specifying the constraints for the optimization, see \code{\link{constraint}}
#' @param type character type of the objective to add or update, currently 'return','risk', or 'risk_budget'
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @param indexnum if you are updating a specific constraint, the index number in the $objectives list to update
#' @author Brian G. Peterson
#' 
#' @seealso \code{\link{constraint}}
#' 
#' @export
add.objective <- function(constraints, type, name, arguments=NULL, enabled=FALSE, ..., indexnum=NULL)
{
    if (!is.constraint(constraints)) {stop("constraints passed in are not of class constraint")}

    if (!hasArg(name)) stop("you must supply a name for the objective")
    if (!hasArg(type)) stop("you must supply a type of objective to create")
    if (!hasArg(enabled)) enabled=FALSE
    if (!hasArg(arguments) | is.null(arguments)) arguments<-list()
    if (!is.list(arguments)) stop("arguments must be passed as a named list")

    assets=constraints$assets
    
    tmp_objective=NULL
    
    switch(type,
        return=, return_objective=
          {tmp_objective = return_objective(name=name,
                                            enabled=enabled,
                                            arguments=arguments,
                                            ... = ...
                                            )
          },

        risk=, portfolio_risk=, portfolio_risk_objective =
          {tmp_objective = portfolio_risk_objective(name=name,
                                                    enabled=enabled,
                                                    arguments=arguments,
                                                    ...=...
                                                   )
          },

        risk_budget=, risk_budget_objective=
          {tmp_objective = risk_budget_objective(assets=constraints$assets,
                                                 name=name,
                                                 enabled=enabled,
                                                 arguments=arguments,
                                                 ...=...
                                                )
          },
           turnover = {tmp_objective = turnover_objective(name=name,
                                                          enabled=enabled,
                                                          arguments=arguments,
                                                          ...=...)
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

#' General interface for adding optimization objectives, including risk, return, and risk budget
#' 
#' This function is the main function for adding and updating business objectives in an object of type \code{\link{portfolio}}.
#' 
#' In general, you will define your objective as one of three types: 'return', 'risk', or 'risk_budget'.  
#' These have special handling and intelligent defaults for dealing with the function most likely to be 
#' used as objectives, including mean, median, VaR, ES, etc.
#' 
#' @param portfolio an object of type 'portfolio' to add the objective to, specifying the portfolio for the optimization, see \code{\link{portfolio}}
#' @param type character type of the objective to add or update, currently 'return','risk', or 'risk_budget'
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @param indexnum if you are updating a specific constraint, the index number in the $objectives list to update
#' @author Brian G. Peterson and Ross Bennett
#' 
#' @seealso \code{\link{objective}}
#' 
#' @export
add.objective_v2 <- function(portfolio, type, name, arguments=NULL, enabled=FALSE, ..., indexnum=NULL){
  # This function is based on the original add.objective function, but modified
  # to add objectives to a portfolio object instead of a constraint object.
  if (!is.portfolio(portfolio)) {stop("portfolio passed in is not of class portfolio")}
  
  if (!hasArg(name)) stop("you must supply a name for the objective")
  if (!hasArg(type)) stop("you must supply a type of objective to create")
  if (!hasArg(enabled)) enabled=FALSE
  if (!hasArg(arguments) | is.null(arguments)) arguments<-list()
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  
  assets=portfolio$assets
  
  tmp_objective=NULL
  
  switch(type,
         return=, return_objective= 
           {tmp_objective = return_objective(name=name,
                                  enabled=enabled,
                                  arguments=arguments,
                                  ... = ...
                                             )
            },
         
         risk=, portfolio_risk=, portfolio_risk_objective =
           {tmp_objective = portfolio_risk_objective(name=name,
                                          enabled=enabled,
                                          arguments=arguments,
                                          ...=...
                                                     )
            },
         
         risk_budget=, risk_budget_objective=
           {tmp_objective = risk_budget_objective(assets=portfolio$assets,
                                       name=name,
                                       enabled=enabled,
                                       arguments=arguments,
                                       ...=...
                                                  )
            },
         
         turnover = {tmp_objective = turnover_objective(name=name,
                                                        enabled=enabled,
                                                        arguments=arguments,
                                                        ...=...)
         },
         
         null = 
           {return(portfolio)} # got nothing, default to simply returning
         ) # end objective type switch
  
  if(is.objective(tmp_objective)) {
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(portfolio$objectives)+1
    tmp_objective$call <- match.call()
    portfolio$objectives[[indexnum]] <- tmp_objective
  }
  return(portfolio)
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


#' constructor for class return_objective
#'
#' if target is null, we'll try to maximize the return metric
#' 
#' if target is set, we'll try to meet or exceed the metric, penalizing a shortfall
#'  
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param target univariate target for the objective
#' @param arguments default arguments to be passed to an objective function when executed
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @author Brian G. Peterson
#' @export
return_objective <- function(name, target=NULL, arguments=NULL, multiplier=-1, enabled=FALSE, ... )
{
  if(!hasArg(target)) target = NULL
  ##' if target is null, we'll try to maximize the return metric
  if(!hasArg(multiplier)) multiplier=-1
  return(objective(name=name, target=target, arguments=arguments, enabled=enabled, multiplier=multiplier,objclass=c("return_objective","objective"), ... ))
} # end return_objective constructor

#' constructor for class portfolio_risk_objective
#' 
#' if target is null, we'll try to minimize the risk metric
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param target univariate target for the objective
#' @param arguments default arguments to be passed to an objective function when executed
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @author Brian G. Peterson
#' @export
portfolio_risk_objective <- function(name, target=NULL, arguments=NULL, multiplier=1, enabled=FALSE, ... )
{
    if(is.null(arguments$portfolio_method)) arguments$portfolio_method="single" #use multivariate risk calcs
    return(objective(name=name,target=target, arguments=arguments, multiplier=multiplier,enabled=enabled, objclass=c("portfolio_risk_objective","objective"), ... ))
} # end portfolio_risk_objective constructor

#' constructor for class risk_budget_objective
#' 
#' @param assets vector of assets to use, should come from constraints object
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param target univariate target for the objective
#' @param arguments default arguments to be passed to an objective function when executed
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @param min_prisk minimum percentage contribution to risk
#' @param max_prisk maximum percentage contribution to risk
#' @param min_concentration TRUE/FALSE whether to minimize concentration, default FALSE, always TRUE if min_prisk and max_prisk are NULL
#' @param min_difference TRUE/FALSE whether to minimize difference between concentration, default FALSE
#' @author Brian G. Peterson
#' @export
risk_budget_objective <- function(assets, name, target=NULL, arguments=NULL, multiplier=1, enabled=FALSE, ..., min_prisk, max_prisk, min_concentration=FALSE, min_difference=FALSE )
{
  if(is.null(arguments$portfolio_method)) arguments$portfolio_method="component"
  
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
  
  if (is.null(min_prisk) & is.null(max_prisk)) 
      min_concentration<-TRUE
  
  Objective<-objective(name=name,target=target, arguments=arguments, multiplier=multiplier,enabled=enabled, objclass=c("risk_budget_objective","objective"), ... )
  Objective$min_prisk = min_prisk
  Objective$max_prisk = max_prisk
  Objective$min_concentration<-min_concentration
  Objective$min_difference<-min_difference
  
  return(Objective)
} # end risk_budget_objective constructor

#' constructor for class turnover_objective
#'
#' if target is null, we'll try to minimize the turnover metric
#' 
#' if target is set, we'll try to meet the metric
#'  
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param target univariate target for the objective
#' @param arguments default arguments to be passed to an objective function when executed
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @author Ross Bennett
#' @export
turnover_objective <- function(name, target=NULL, arguments=NULL, multiplier=1, enabled=FALSE, ... )
{
  if(!hasArg(target)) target = NULL
  ##' if target is null, we'll try to minimize the turnover metric
  if(!hasArg(multiplier)) multiplier=1
  return(objective(name=name, target=target, arguments=arguments, enabled=enabled, multiplier=multiplier,objclass=c("turnover_objective","objective"), ... ))
} # end turnover_objective constructor
