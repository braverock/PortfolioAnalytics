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

#' constructor for class 'objective'
#' 
#' Typically called as a sub-function by the user function \code{\link{add.objective}}.
#' See main documentation there.
#' 
#' @param name name of the objective which will be used to call a function, like 'ES', 'VaR', 'mean'
#' @param target univariate target for the objective, default NULL
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthrough parameters
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param objclass string class to apply, default 'objective'
#' @seealso \code{\link{add.objective}}, \code{\link{portfolio.spec}}
#' @author Brian G. Peterson
#' @export
objective<-function(name , target=NULL , arguments, enabled=TRUE , ..., multiplier=1, objclass='objective'){
  if(!hasArg(name)) stop("you must specify an objective name")
  if (hasArg(name)) if(is.null(name)) stop("you must specify an objective name")
  if (!hasArg(arguments) | is.null(arguments)) arguments<-list()
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

#' @rdname add.objective
#' @name add.objective
#' @export
add.objective_v1 <- function(constraints, type, name, arguments=NULL, enabled=TRUE, ..., indexnum=NULL)
{
    if (!is.constraint(constraints)) {stop("constraints passed in are not of class constraint")}

    if (!hasArg(name)) stop("you must supply a name for the objective")
    if (!hasArg(type)) stop("you must supply a type of objective to create")
    if (!hasArg(enabled)) enabled=TRUE
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
           tmp_minmax = {tmp_objective = minmax_objective(name=name,
                                                          enabled=enabled,
                                                          arguments=arguments,
                                                          ...=...)
           },
           weight_conc=, weight_concentration = 
             {tmp_objective = weight_concentration_objective(name=name, 
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
#' This function is the main function for adding and updating business objectives in an object of type \code{\link{portfolio.spec}}.
#' 
#' In general, you will define your objective as one of the following types: 'return', 'risk', 'risk_budget', 'quadratic utility', or 'weight_concentration'.  
#' These have special handling and intelligent defaults for dealing with the function most likely to be 
#' used as objectives, including mean, median, VaR, ES, etc.
#' 
#' Objectives of type 'turnover' and 'minmax' are also supported.
#' 
#' @param portfolio an object of type 'portfolio' to add the objective to, specifying the portfolio for the optimization, see \code{\link{portfolio}}
#' @param constraints a 'v1_constraint' object for backwards compatibility, see \code{\link{constraint}}
#' @param type character type of the objective to add or update, currently 'return','risk', 'risk_budget', 'quadratic_utility', or 'weight_concentration'
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters 
#' @param indexnum if you are updating a specific objective, the index number in the $objectives list to update
#' @author Brian G. Peterson and Ross Bennett
#' @aliases add.objective_v2 add.objective_v1
#' @seealso \code{\link{objective}}, \code{\link{portfolio.spec}}
#' @rdname add.objective
#' @name add.objective
#' @examples
#' data(edhec)
#' returns <- edhec[,1:4]
#' fund.names <- colnames(returns)
#' portf <- portfolio.spec(assets=fund.names)
#' # Add some basic constraints
#' portf <- add.constraint(portf, type="full_investment")
#' portf <- add.constraint(portf, type="long_only")
#' 
#' # Creates a new portfolio object using portf and adds a quadratic utility
#' # objective. This will add two objectives to the portfolio object; 1) mean and
#' # 2) var. The risk aversion parameter is commonly referred to as lambda in the
#' # quadratic utility formulation that controls how much the portfolio variance 
#' # is penalized.
#' portf.maxQU <- add.objective(portf, type="quadratic_utility", 
#'                              risk_aversion=0.25)
#' 
#' # Creates a new portfolio object using portf and adds mean as an objective
#' portf.maxMean <- add.objective(portf, type="return", name="mean")
#' 
#' # Creates a new portfolio object using portf and adds StdDev as an objective
#' portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")
#' 
#' # Creates a new portfolio object using portf and adds ES as an objective. 
#' # Note that arguments to ES are passed in as a named list.
#' portf.minES <- add.objective(portf, type="risk", name="ES", 
#'                              arguments=list(p=0.925, clean="boudt"))
#' 
#' # Creates a new portfolio object using portf.minES and adds a risk budget 
#' # objective with limits on component risk contribution. 
#' # Note that arguments to ES are passed in as a named list.
#' portf.RiskBudgetES <- add.objective(portf.minES, type="risk_budget", name="ES", 
#'                              arguments=list(p=0.925, clean="boudt"),
#'                              min_prisk=0, max_prisk=0.6)
#' 
#' # Creates a new portfolio object using portf.minES and adds a risk budget 
#' # objective with equal component risk contribution. 
#' # Note that arguments to ES are passed in as a named list.
#' portf.EqRiskES <- add.objective(portf.minES, type="risk_budget", name="ES", 
#'                                     arguments=list(p=0.925, clean="boudt"),
#'                                     min_concentration=TRUE)
#' 
#' # Creates a new portfolio object using portf and adds a weight_concentration 
#' # objective. The conc_aversion parameter controls how much concentration is
#' # penalized. The portfolio concentration is defined as the Herfindahl Hirschman
#' # Index of the weights.
#' portf.conc <- add.objective(portf, type="weight_concentration", 
#'                             name="HHI", conc_aversion=0.01)
#' @rdname add.objective
#' @name add.objective
#' @export add.objective 
#' @export add.objective_v2
add.objective <- add.objective_v2 <- function(portfolio, constraints=NULL, type, name, arguments=NULL, enabled=TRUE, ..., indexnum=NULL){
  if(!is.null(constraints) & inherits(constraints, "v1_constraint")){
    return(add.objective_v1(constraints=constraints, type=type, name=name, arguments=arguments, enabled=enabled, ...=..., indexnum=indexnum))
  }
  
  # This function is based on the original add.objective function, but modified
  # to add objectives to a portfolio object instead of a constraint object.
  if (!is.portfolio(portfolio)) {stop("portfolio passed in is not of class portfolio")}
  
  if (type != "quadratic_utility" & !hasArg(name)) stop("you must supply a name for the objective")
  if (!hasArg(type)) stop("you must supply a type of objective to create")
  if (!hasArg(enabled)) enabled=TRUE
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
         tmp_minmax = {tmp_objective = minmax_objective(name=name,
                                                        enabled=enabled,
                                                        arguments=arguments,
                                                        ...=...)
         },
         qu=, quadratic_utility = {tmp_objective = quadratic_utility_objective(enabled=enabled, ...=...)
                              # quadratic_utility_objective returns a list of a return_objective and a portfolio_risk_objective
                              # we just need to combine it to the portfolio$objectives slot and return the portfolio
                              portfolio$objectives <- c(portfolio$objectives, tmp_objective)
                              return(portfolio)
         },
         weight_conc=, weight_concentration = 
           {tmp_objective = weight_concentration_objective(name=name, 
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
#' @return object of class 'return_objective'
#' @author Brian G. Peterson
#' @export
return_objective <- function(name, target=NULL, arguments=NULL, multiplier=-1, enabled=TRUE, ... )
{
  if(!hasArg(target)) target = NULL
  ## if target is null, we'll try to maximize the return metric
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
#' @return object of class 'portfolio_risk_objective'
#' @author Brian G. Peterson
#' @export
portfolio_risk_objective <- function(name, target=NULL, arguments=NULL, multiplier=1, enabled=TRUE, ... )
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
#' @return object of class 'risk_budget_objective'
#' @author Brian G. Peterson
#' @export
risk_budget_objective <- function(assets, name, target=NULL, arguments=NULL, multiplier=1, enabled=TRUE, ..., min_prisk, max_prisk, min_concentration=FALSE, min_difference=FALSE )
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
#' @return an objective of class 'turnover_objective'
#' @author Ross Bennett
#' @export
turnover_objective <- function(name, target=NULL, arguments=NULL, multiplier=1, enabled=TRUE, ... )
{
  if(!hasArg(target)) target = NULL
  ## if target is null, we'll try to minimize the turnover metric
  if(!hasArg(multiplier)) multiplier=1
  return(objective(name=name, target=target, arguments=arguments, enabled=enabled, multiplier=multiplier,objclass=c("turnover_objective","objective"), ... ))
} # end turnover_objective constructor

#' constructor for class tmp_minmax_objective
#'
#' This objective allows for min and max targets to be specified.
#' 
#' If target is set, we'll try to meet the metric
#' 
#' If target is NULL and min and max are specified, then do the following:
#' 
#' If max is violated to the upside, penalize the metric. If min is violated to 
#' the downside, penalize the metric. The purpose of this objective is to try
#' to meet the range between min and max
#'  
#' @param name name of the objective, should correspond to a function, though we will try to make allowances
#' @param target univariate target for the objective
#' @param min minimum value
#' @param max maximum value
#' @param arguments default arguments to be passed to an objective function when executed
#' @param multiplier multiplier to apply to the objective, usually 1 or -1
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters
#' @return object of class 'minmax_objective'
#' @author Ross Bennett
#' @export
minmax_objective <- function(name, target=NULL, arguments=NULL, multiplier=1, enabled=TRUE, ..., min, max )
{
  if(!hasArg(target)) target = NULL
  ## if target is null, we'll try to minimize the metric
  if(!hasArg(multiplier)) multiplier=1
  Objective <- objective(name=name, target=target, arguments=arguments, enabled=enabled, multiplier=multiplier,objclass=c("minmax_objective","objective"), ... )
  Objective$min <- min
  Objective$max <- max
  return(Objective)
} # end minmax_objective constructor

#' constructor for quadratic utility objective
#' 
#' This function calls \code{\link{return_objective}} and \code{\link{portfolio_risk_objective}}
#' to create a list of the objectives to be added to the portfolio.
#' 
#' @param risk_aversion risk_aversion (i.e. lambda) parameter to penalize variance
#' @param target target mean return value
#' @param enabled TRUE/FALSE, default enabled=TRUE
#' @return a list of two elements
#' \itemize{
#'   \item{\code{return_objective}}
#'   \item{\code{portfolio_risk_objective}}
#' }
#' @author Ross Bennett
#' @export
quadratic_utility_objective <- function(risk_aversion=1, target=NULL, enabled=TRUE){
  qu <- list()
  qu[[1]] <- return_objective(name="mean", target=target, enabled=enabled)
  qu[[2]] <- portfolio_risk_objective(name="var", risk_aversion=risk_aversion, enabled=enabled)
  return(qu)
} # end quadratic utility objective constructor

#' Constructor for weight concentration objective
#' 
#' This function penalizes weight concentration using the Herfindahl-Hirschman Index
#' as a measure of concentration.
#' 
#' The \code{conc_aversion} argument can be a scalar or vector of concentration
#' aversion values. If \code{conc_aversion} is a scalar and \code{conc_groups} is
#' \code{NULL}, then the concentration aversion value will be applied to the overall
#' weights.
#' 
#' If \code{conc_groups} is specified as an argument, then the concentration
#' aversion value(s) will be applied to each group.
#' 
#' @param name name of concentration measure, currently only "HHI" is supported.
#' @param conc_aversion concentration aversion value(s)
#' @param conc_groups list of vectors specifying the groups of the assets. Similar
#' to \code{groups} in \code{\link{group_constraint}}
#' @param arguments default arguments to be passed to an objective function when executed
#' @param enabled TRUE/FALSE
#' @param \dots any other passthru parameters
#' @return an object of class 'weight_concentration_objective'
#' @author Ross Bennett
#' @export
weight_concentration_objective <- function(name, conc_aversion, conc_groups=NULL, arguments=NULL, enabled=TRUE, ...){
  # TODO: write HHI function to be used by global solvers in constrained_objective
  
  # check if conc_groups is specified as an argument
  if(!is.null(conc_groups)){
    arguments$groups <- conc_groups
    if(!is.list(conc_groups)) stop("conc_groups must be passed in as a list")
    
    if(length(conc_aversion) == 1){
      # if conc_aversion is a scalar, replicate to the number of groups
      conc_aversion <- rep(conc_aversion, length(conc_groups))
    }
    # length of conc_aversion must be equal to the length of conc_groups
    if(length(conc_aversion) != length(conc_groups)) stop("length of conc_aversion must be equal to length of groups")
  } else if(is.null(conc_groups)){
    if(length(conc_aversion) != 1) stop("conc_aversion must be a scalar value when conc_groups are not specified")
  }
  Objective <- objective(name=name, enabled=enabled, arguments=arguments, objclass=c("weight_concentration_objective","objective"), ... )
  Objective$conc_aversion <- conc_aversion
  Objective$conc_groups <- conc_groups
  return(Objective)
}

#' Insert a list of objectives into the objectives slot of a portfolio object
#' 
#' This is a helper function primarily for backwards compatibility to insert
#' objectives from a 'v1_constraint' object into the v2 'portfolio' object.
#' 
#' @param portfolio object of class 'portfolio'
#' @param objectives list of objective objects
#' @author Ross Bennett
#' @export
insert_objectives <- function(portfolio, objectives){
  # Check portfolio object
  if (is.null(portfolio) | !is.portfolio(portfolio)){
    stop("you must pass in an object of class portfolio")
  }
  
  # Check that objectives is a list
  if(!is.list(objectives)) stop("objectives must be passed in as a list")
  
  # Check that all objects in the list are of class objective
  for(i in 1:length(objectives)){
    if(!is.objective(objectives[[i]]))
      stop("objectives must be passed in as a list and all objects in objectives must be of class 'objective'")
  }
  
  portfolio$objectives <- objectives
  return(portfolio)
}
