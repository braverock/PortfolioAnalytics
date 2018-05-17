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

# TODO add examples
 
# TODO add more details about the nuances of the optimization engines


#' @rdname constrained_objective
#' @name constrained_objective
#' @export 
constrained_objective_v1 <- function(w, R, constraints, ..., trace=FALSE, normalize=TRUE, storage=FALSE)
{ 
    if (ncol(R)>length(w)) {
        R=R[,1:length(w)]
    }
    if(!hasArg(penalty)) penalty = 1e4
    N = length(w)
    T = nrow(R)
    if(hasArg(optimize_method)) 
    	optimize_method=match.call(expand.dots=TRUE)$optimize_method else optimize_method='' 
    if(hasArg(verbose)) 
    	verbose=match.call(expand.dots=TRUE)$verbose 
    else verbose=FALSE 
    
    # check for valid constraints
    if (!is.constraint(constraints)) {
    	stop("constraints passed in are not of class constraint")
    }

    # check that the constraints and the weighting vector have the same length
    if (N != length(constraints$assets)){
      warning("length of constraints asset list and weights vector do not match, results may be bogus")
    }

    out=0

    # do the get here
    store_output <- try(get('.objectivestorage',envir=.storage),silent=TRUE)
    if(inherits(store_output,"try-error")) storage=FALSE else storage=TRUE        
    
    if(isTRUE(normalize)){
        if(!is.null(constraints$min_sum) | !is.null(constraints$max_sum)){
            # the user has passed in either min_sum or max_sum constraints for the portfolio, or both.
            # we'll normalize the weights passed in to whichever boundary condition has been violated
            # NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim
            # might violate your constraints, so you'd need to renormalize them after optimizing
            # we'll create functions for that so the user is less likely to mess it up.
            
            # NOTE: need to normalize in the optimization wrapper too before we return, since we've normalized in here
            # In Kris' original function, this was manifested as a full investment constraint
            # the normalization process produces much faster convergence, 
            # and then we penalize parameters outside the constraints in the next block
            if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
                max_sum=constraints$max_sum
                if(sum(w)>max_sum) { w<-(max_sum/sum(w))*w } # normalize to max_sum
            }
            
            if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
                min_sum=constraints$min_sum
                if(sum(w)<min_sum) { w<-(min_sum/sum(w))*w } # normalize to min_sum
            }
            
        } # end min_sum and max_sum normalization
    } else {
        # the user wants the optimization algorithm to figure it out
        if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
            max_sum=constraints$max_sum
            if(sum(w)>max_sum) { out = out + penalty*(sum(w) - max_sum)  } # penalize difference to max_sum
        }
        if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
            min_sum=constraints$min_sum
            if(sum(w)<min_sum) { out = out + penalty*(min_sum - sum(w)) } # penalize difference to min_sum
        }
    }

    # penalize weights outside my constraints (can be caused by normalization)
    if (!is.null(constraints$max)){
      max = constraints$max
      out = out + sum(w[which(w>max[1:N])]- constraints$max[which(w>max[1:N])])*penalty
    }
    if (!is.null(constraints$min)){
      min = constraints$min
      out = out + sum(constraints$min[which(w<min[1:N])] - w[which(w<min[1:N])])*penalty
    }
    
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        # rm('...')
        nargs=NULL
    }

    nargs<-set.portfolio.moments(R, constraints, momentargs=nargs)
    
    if(is.null(constraints$objectives)) {
      warning("no objectives specified in constraints")
    } else{
      if(isTRUE(trace) | isTRUE(storage)) tmp_return<-list()
      for (objective in constraints$objectives){
        #check for clean bits to pass in
        if(objective$enabled){
          tmp_measure = NULL
          multiplier  = objective$multiplier
          #if(is.null(objective$arguments) | !is.list(objective$arguments)) objective$arguments<-list()
          switch(objective$name,
              mean =,
              median = {
                  fun = match.fun(objective$name)  
                  nargs$x <- ( R %*% w ) #do the multivariate mean/median with Kroneker product
              },
              sd =,
              StdDev = { 
                  fun= match.fun(StdDev)
              },
              mVaR =,
              VaR = {
                  fun= match.fun(VaR) 
                  if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method) & is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
                  if(is.null(objective$arguments$invert)) objective$arguments$invert = FALSE
              },
              es =,
              mES =,
              CVaR =,
              cVaR =,
              ES = {
                  fun = match.fun(ES)
                  if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method)& is.null(nargs$portfolio_method)) nargs$portfolio_method='single'
                  if(is.null(objective$arguments$invert)) objective$arguments$invert = FALSE
              },
                 turnover = {
                   fun = match.fun(turnover) # turnover function included in objectiveFUN.R
                   },
              {   # see 'S Programming p. 67 for this matching
                  fun<-try(match.fun(objective$name))
              }
          )
          if(is.function(fun)){
              .formals  <- formals(fun)
              onames <- names(.formals)
              if(is.list(objective$arguments)){
                  #TODO FIXME only do this if R and weights are in the argument list of the fn
                  if(is.null(nargs$R) | !length(nargs$R)==length(R)) nargs$R <- R
                  
                  if(is.null(nargs$weights)) nargs$weights <- w
                  
                  pm <- pmatch(names(objective$arguments), onames, nomatch = 0L)
                  if (any(pm == 0L))
                      warning(paste("some arguments stored for",objective$name,"do not match"))
                  # this line overwrites the names of things stored in $arguments with names from formals.
                  # I'm not sure it's a good idea, so commenting for now, until we prove we need it
                  #names(objective$arguments[pm > 0L]) <- onames[pm]
                  .formals[pm] <- objective$arguments[pm > 0L]
                  #now add dots
                  if (length(nargs)) {
                      dargs<-nargs
                      pm <- pmatch(names(dargs), onames, nomatch = 0L)
                      names(dargs[pm > 0L]) <- onames[pm]
                      .formals[pm] <- dargs[pm > 0L]
                  }
                  .formals$... <- NULL
              }
          } # TODO do some funky return magic here on try-error
          
          tmp_measure = try((do.call(fun,.formals)) ,silent=TRUE)
          
          if(isTRUE(trace) | isTRUE(storage)) {
              if(is.null(names(tmp_measure))) names(tmp_measure)<-objective$name
              tmp_return[[objective$name]]<-tmp_measure
          }
          
          if(inherits(tmp_measure,"try-error")) { 
              message(paste("objective name",objective$name,"generated an error or warning:",tmp_measure))
          } 
          
          # now set the new value of the objective output
          if(inherits(objective,"return_objective")){ 
              if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
                  out = out + penalty*abs(objective$multiplier)*abs(tmp_measure-objective$target)
              }  
              # target is null or doesn't exist, just maximize, or minimize violation of constraint
              out = out + objective$multiplier*tmp_measure
          } # end handling for return objectives

          if(inherits(objective,"portfolio_risk_objective")){
            if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
                out = out + penalty*abs(objective$multiplier)*abs(tmp_measure-objective$target)
                #should we also penalize risk too low for risk targets? or is a range another objective?
                #    # half penalty for risk lower than target
                #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
            }  
            # target is null or doesn't exist, just maximize, or minimize violation of constraint
            out = out + abs(objective$multiplier)*tmp_measure
          } #  univariate risk objectives
          
          if(inherits(objective,"turnover_objective")){
            if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
              out = out + penalty*abs(objective$multiplier)*abs(tmp_measure-objective$target)
            }  
            # target is null or doesn't exist, just maximize, or minimize violation of constraint
            out = out + abs(objective$multiplier)*tmp_measure
          } #  univariate turnover objectives
          
          if(inherits(objective,"minmax_objective")){
            if (!is.null(objective$min) & !is.null(objective$max)){ # we have a min and max
              if(tmp_measure > objective$max){
                out = out + penalty * objective$multiplier * (tmp_measure - objective$max)
              }
              if(tmp_measure < objective$min){
                out = out + penalty * objective$multiplier * (objective$min - tmp_measure)
              }
            }
          } # temporary minmax objective
          
          if(inherits(objective,"risk_budget_objective")){
            # setup
            
            # out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
            # add risk budget constraint
            if(!is.null(objective$target) & is.numeric(objective$target)){
              #in addition to a risk budget constraint, we have a univariate target
              # the first element of the returned list is the univariate measure
              # we'll use the  univariate measure exactly like we would as a separate objective
                out = out + penalty*abs(objective$multiplier)*abs(tmp_measure[[1]]-objective$target)
              #should we also penalize risk too low for risk targets? or is a range another objective?
              #    # half penalty for risk lower than target
              #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
            }
            percrisk = tmp_measure[[3]] # third element is percent component contribution
            RBupper = objective$max_prisk
            RBlower = objective$min_prisk
            if(!is.null(RBupper) | !is.null(RBlower)){
                out = out + penalty * objective$multiplier * sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
            }
#             if(!is.null(objective$min_concentration)){
#                 if(isTRUE(objective$min_concentration)){
#                     max_conc<-max(tmp_measure[[2]]) #second element is the contribution in absolute terms
#                     # out=out + penalty * objective$multiplier * max_conc
#                     out = out + objective$multiplier * max_conc
#                 }
#             }
            # Combined min_con and min_dif to take advantage of a better concentration obj measure
            if(!is.null(objective$min_difference) || !is.null(objective$min_concentration)){
                if(isTRUE(objective$min_difference)){
#                     max_diff<-max(tmp_measure[[2]]-(sum(tmp_measure[[2]])/length(tmp_measure[[2]]))) #second element is the contribution in absolute terms
                  # Uses Herfindahl index to calculate concentration; added scaling perc diffs back to univariate numbers
                    max_diff <- sqrt(sum(tmp_measure[[3]]^2))/100  #third element is the contribution in percentage terms
                    # out = out + penalty * objective$multiplier * max_diff
                    out = out + penalty*objective$multiplier * max_diff
                }
            }
          } # end handling of risk_budget objective

        } # end enabled check
      } # end loop over objectives
    } # end objectives processing

    if(isTRUE(verbose)) {
        print('weights: ')
        print(paste(w,' '))
        print(paste("output of objective function",out))
        print(unlist(tmp_return))
    }
    
    if(is.na(out) | is.nan(out) | is.null(out)){
        #this should never happen
        warning('NA or NaN produced in objective function for weights ',w)
        out<-penalty
    }
    
    #return
    if (isTRUE(storage)){
        #add the new objective results
        store_output[[length(store_output)+1]]<-list(out=as.numeric(out),weights=w,objective_measures=tmp_return)
        # do the assign here
        assign('.objectivestorage', store_output, envir=.storage)
    }
    if(!isTRUE(trace)){
        return(out)
    } else {
        return(list(out=as.numeric(out),weights=w,objective_measures=tmp_return))
    }
}

#' calculate a numeric return value for a portfolio based on a set of constraints and objectives
#' 
#' Function to calculate a numeric return value for a portfolio based on a set of constraints and objectives.
#' We'll try to make as few assumptions as possible and only run objectives that are enabled by the user.
#' 
#' If the user has passed in either min_sum or max_sum constraints for the portfolio, or both, 
#' and are using a numerical optimization method like DEoptim, and normalize=TRUE,
#' we'll normalize the weights passed in to whichever boundary condition has been violated.  
#' If using random portfolios, all the portfolios generated will meet the constraints by construction.
#' NOTE: this means that the weights produced by a numeric optimization algorithm like DEoptim, pso, or GenSA
#' might violate constraints, and will need to be renormalized after optimizing.
#' We apply the same normalization in \code{\link{optimize.portfolio}} so that the weights you see have been 
#' normalized to min_sum if the generated portfolio is smaller than min_sum or max_sum if the 
#' generated portfolio is larger than max_sum.  
#' This normalization increases the speed of optimization and convergence by several orders of magnitude in many cases.
#' 
#' You may find that for some portfolios, normalization is not desirable, if the algorithm 
#' cannot find a direction in which to move to head towards an optimal portfolio.  In these cases, 
#' it may be best to set normalize=FALSE, and penalize the portfolios if the sum of the weighting 
#' vector lies outside the min_sum and/or max_sum.
#' 
#' Whether or not we normalize the weights using min_sum and max_sum, and are using a numerical optimization 
#' engine like DEoptim, we will penalize portfolios that violate weight constraints in much the same way
#' we penalize other constraints.  If a min_sum/max_sum normalization has not occurred, convergence
#' can take a very long time.  We currently do not allow for a non-normalized full investment constraint.  
#' Future version of this function could include this additional constraint penalty. 
#'  
#' When you are optimizing a return objective, you must specify a negative multiplier 
#' for the return objective so that the function will maximize return.  If you specify a target return,
#' any return that deviates from your target will be penalized.  If you do not specify a target return, 
#' you may need to specify a negative VTR (value to reach) , or the function will not converge.  
#' Try the maximum expected return times the multiplier (e.g. -1 or -10).  
#' Adding a return objective defaults the multiplier to -1.
#' 
#' Additional parameters for other solvers 
#' (e.g. random portfolios or 
#' \code{\link[DEoptim]{DEoptim.control}} or pso or GenSA 
#' may be passed in via \dots
#' 
#'    
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns.
#' @param w a vector of weights to test.
#' @param portfolio an object of class \code{portfolio} specifying the constraints and objectives for the optimization, see \code{\link{portfolio}}.
#' @param \dots any other passthru parameters.
#' @param trace TRUE/FALSE whether to include debugging and additional detail in the output list. The default is FALSE. Several charting functions require that \code{trace=TRUE}.
#' @param normalize TRUE/FALSE whether to normalize results to min/max sum (TRUE), or let the optimizer penalize portfolios that do not conform (FALSE)
#' @param storage TRUE/FALSE default TRUE for DEoptim with trace, otherwise FALSE. not typically user-called.
#' @param constraints a v1_constraint object for backwards compatibility with \code{constrained_objective_v1}.
#' @param env environment of moments calculated in \code{optimize.portfolio}
#' @seealso \code{\link{constraint}}, \code{\link{objective}}, \code{\link[DEoptim]{DEoptim.control}} 
#' @author Kris Boudt, Peter Carl, Brian G. Peterson, Ross Bennett
#' @aliases constrained_objective constrained_objective_v1 constrained_objective_v2
#' @rdname constrained_objective
#' @export constrained_objective
#' @export constrained_objective_v2
constrained_objective <- constrained_objective_v2 <- function(w, R, portfolio, ..., trace=FALSE, normalize=TRUE, storage=FALSE, env=NULL)
{ 
  if (ncol(R) > length(w)) {
    R <- R[ ,1:length(w)]
  }
  if(!hasArg(penalty)) penalty <- 1e4
  N <- length(w)
  T <- nrow(R)
  if(hasArg(optimize_method)) 
    optimize_method <- match.call(expand.dots=TRUE)$optimize_method else optimize_method <- '' 
  if(hasArg(verbose)) 
    verbose <- match.call(expand.dots=TRUE)$verbose 
  else verbose <- FALSE 
  
  # initial weights
  init_weights <- w
  
  # get the constraints from the portfolio object
  constraints <- get_constraints(portfolio)
  
  # check for valid portfolio
  if (!is.portfolio(portfolio)) {
    stop("portfolio object passed in is not of class portfolio")
  }
  
  # check that the assets and the weighting vector have the same length
  if (N != length(portfolio$assets)){
    warning("length of portfolio asset list and weights vector do not match, results may be bogus")
  }
  
  out <- 0
  
  # do the get here
  store_output <- try(get('.objectivestorage',envir=.storage), silent=TRUE)
  if(inherits(store_output,"try-error")) {
    storage <- FALSE
    # warning("could not get .objectivestorage")
  } else {
    storage <- TRUE
  }
  
  # use fn_map to normalize the weights
  if(isTRUE(normalize)){
    w <- fn_map(weights=w, portfolio=portfolio)$weights
    # end fn_map transformation
  } else {
    # the user wants the optimization algorithm to figure it out
    if(!is.null(constraints$max_sum) & constraints$max_sum != Inf ) {
      max_sum <- constraints$max_sum
      if(sum(w) > max_sum) { out <- out + penalty * (sum(w) - max_sum)  } # penalize difference to max_sum
    }
    if(!is.null(constraints$min_sum) & constraints$min_sum != -Inf ) {
      min_sum <- constraints$min_sum
      if(sum(w) < min_sum) { out <- out + penalty * (min_sum - sum(w)) } # penalize difference to min_sum
    }
  }
  
  # penalize weights outside min and max box constraints (can be caused by normalization)
  if (!is.null(constraints$max)){
    max <- constraints$max
    # Only go to penalty term if any of the weights violate max
    if(any(w > max)){
      out <- out + sum(w[which(w > max[1:N])] - constraints$max[which(w > max[1:N])]) * penalty
    }
  }
  if (!is.null(constraints$min)){
    min <- constraints$min
    # Only go to penalty term if any of the weights violate min
    if(any(w < min)){
      out <- out + sum(constraints$min[which(w < min[1:N])] - w[which(w < min[1:N])]) * penalty
    }
  }

  # penalize weights that violate group constraints
  if(!is.null(constraints$groups) & !is.null(constraints$cLO) & !is.null(constraints$cUP)){
    groups <- constraints$groups
    cLO <- constraints$cLO
    cUP <- constraints$cUP
    # Only go to penalty term if group constraint is violated
    if(any(group_fail(w, groups, cLO, cUP))){
      ngroups <- length(groups)
      for(i in 1:ngroups){
        tmp_w <- w[groups[[i]]]
        # penalize for weights that are below cLO
        if(sum(tmp_w) < cLO[i]){
          out <- out + penalty * (cLO[i] - sum(tmp_w))
        }
        if(sum(tmp_w) > cUP[i]){
          out <- out + penalty * (sum(tmp_w) - cUP[i])
        }
      }
    }
  } # End group constraint penalty
    
  # penalize weights that violate max_pos constraints
    if(!is.null(constraints$max_pos)){
      max_pos <- constraints$max_pos
      tolerance <- .Machine$double.eps^0.5
      mult <- 1
      # sum(abs(w) > tolerance) is the number of non-zero assets
      nzassets <- sum(abs(w) > tolerance)
      if(nzassets > max_pos){
        # Do we need a small multiplier term here since (nzassets - max_pos) 
        # will be an integer and much larger than the weight penalty terms
        out <- out + penalty * mult * (nzassets - max_pos)
      }
    } # End position_limit constraint penalty
    
  # penalize weights that violate diversification constraint
    if(!is.null(constraints$div_target)){
      div_target <- constraints$div_target
      div <- diversification(w)
      mult <- 1
      # only penalize if not within +/- 5% of target
      if((div < div_target * 0.95) | (div > div_target * 1.05)){
        out <- out + penalty * mult * abs(div - div_target)
      }
    } # End diversification constraint penalty
    
  # penalize weights that violate turnover constraint
    if(!is.null(constraints$turnover_target)){
      turnover_target <- constraints$turnover_target
      to <- turnover(w)
      mult <- 1
      # only penalize if not within +/- 5% of target
      if((to < turnover_target * 0.95) | (to > turnover_target * 1.05)){
        # print("transform or penalize to meet turnover target")
        out = out + penalty * mult * abs(to - turnover_target)
      }
    } # End turnover constraint penalty
  
  # penalize weights that violate return target constraint
  if(!is.null(constraints$return_target)){
    return_target <- constraints$return_target
    mean_return <- port.mean(weights=w, mu=env$mu)
    mult <- 1
    out = out + penalty * mult * abs(mean_return - return_target)
  } # End return constraint penalty
  
  # penalize weights that violate factor exposure constraints
  if(!is.null(constraints$B)){
    t.B <- t(constraints$B)
    lower <- constraints$lower
    upper <- constraints$upper
    mult <- 1
    for(i in 1:nrow(t.B)){
      tmpexp <- as.numeric(t(w) %*% t.B[i, ])
      if(tmpexp < lower[i]){
        out <- out + penalty * mult * (lower[i] - tmpexp)
      }
      if(tmpexp > upper[i]){
        out <- out + penalty * mult * (tmpexp - upper[i])
      }
    }
  } # End factor exposure constraint penalty
  
  # Add penalty for transaction costs
  if(!is.null(constraints$ptc)){
    # calculate total transaction cost using portfolio$assets as initial set of weights
    tc <- sum(abs(w - portfolio$assets) * constraints$ptc)
    # for now use a multiplier of 1, may need to adjust this later
    mult <- 1
    out <- out + mult * tc
  } # End transaction cost penalty
  
  # Add penalty for leverage exposure
  # This could potentially be added to random portfolios
  if(!is.null(constraints$leverage)){
    if((sum(abs(w)) > constraints$leverage)){
      # only penalize if leverage is exceeded
      mult <- 1/100
      out <- out + penalty * mult * abs(sum(abs(w)) - constraints$leverage)
    }
  } # End leverage exposure penalty
  
  # The "..." are passed in from optimize.portfolio and contain the output of
  # momentFUN. The default is momentFUN=set.portfolio.moments and returns
  # moments$mu, moments$sigma, moments$m3, moments$m4, etc. depending on the
  # the functions corresponding to portfolio$objective$name. Would it be better
  # to make this a formal argument for constrained_objective? This means that
  # we completely avoid evaluating the set.portfolio.moments function. Can we
  # trust that all the moments are correctly set in optimize.portfolio through
  # momentFUN?
  
  # Add R and w to the environment with the moments
  # env$R <- R
  # env$weights <- w
  
  if(!is.null(env)){
    nargs <- env
  } else {
    # print("calculating moments")
    # calculating the moments
    # nargs are used as the arguments for functions corresponding to 
    # objective$name called in the objective loop later
    momentargs <- eval(substitute(alist(...)))
    .formals <- formals(set.portfolio.moments)
    .formals <- modify.args(formals=.formals, arglist=alist(momentargs=momentargs), dots=TRUE)
    .formals <- modify.args(formals=.formals, arglist=NULL, R=R, dots=TRUE)
    .formals <- modify.args(formals=.formals, arglist=NULL, portfolio=portfolio, dots=TRUE)
    .formals$... <- NULL
    # print(.formals)
    nargs <- do.call(set.portfolio.moments, .formals)
  }
  
  # We should avoid modifying nargs in the loop below.
  # If we modify nargs with something like nargs$x, nargs is copied and this
  # should be avoided because nargs could be large because it contains the moments.
  tmp_args <- list()
  
  # JMU: Add all the variables in 'env' to tmp_args as names/symbols
  # tmp_args[ls(env)] <- lapply(ls(env), as.name)
  
  if(is.null(portfolio$objectives)) {
    warning("no objectives specified in portfolio")
  } else{
    if(isTRUE(trace) | isTRUE(storage)) tmp_return <- list()
    for (objective in portfolio$objectives){
      #check for clean bits to pass in
      if(objective$enabled){
        tmp_measure <- NULL
        multiplier <- objective$multiplier
        #if(is.null(objective$arguments) | !is.list(objective$arguments)) objective$arguments<-list()
        switch(objective$name,
               mean =,
               median = {
                 fun = match.fun(port.mean)
                 # would it be better to do crossprod(w, moments$mu)?
                 # tmp_args$x <- ( R %*% w ) #do the multivariate mean/median with Kroneker product
               },
               median = {
                 fun = match.fun(objective$name)
                 tmp_args$x <- ( R %*% w ) #do the multivariate mean/median with Kroneker product
               },
               sd =,
               var =,
               StdDev = { 
                 fun = match.fun(StdDev)
               },
               mVaR =,
               VaR = {
                 fun = match.fun(VaR) 
                 if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method) & is.null(nargs$portfolio_method)) tmp_args$portfolio_method='single'
                 if(is.null(objective$arguments$invert)) tmp_args$invert = FALSE
               },
               es =,
               mES =,
               CVaR =,
               cVaR =,
               ETL=,
               mETL=,
               ES = {
                 fun = match.fun(ES)
                 if(!inherits(objective,"risk_budget_objective") & is.null(objective$arguments$portfolio_method) & is.null(nargs$portfolio_method)) tmp_args$portfolio_method='single'
                 if(is.null(objective$arguments$invert)) tmp_args$invert = FALSE
               },
               turnover = {
                 fun = match.fun(turnover) # turnover function included in objectiveFUN.R
               },
{   # see 'S Programming p. 67 for this matching
  fun <- try(match.fun(objective$name))
}
        )
        
        if(is.function(fun)){
          .formals <- formals(fun)
          # Add the moments from the nargs object
          # nargs contains the moments, these are being evaluated
          .formals <- modify.args(formals=.formals, arglist=nargs, dots=TRUE)
          # Add anything from tmp_args
          .formals <- modify.args(formals=.formals, arglist=tmp_args, dots=TRUE)
          # Now add the objective$arguments
          .formals <- modify.args(formals=.formals, arglist=objective$arguments, dots=TRUE)
          # Add R and weights if necessary
          if("R" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, R=R, dots=TRUE)
          if("weights" %in% names(.formals)) .formals <- modify.args(formals=.formals, arglist=NULL, weights=w, dots=TRUE)
          # .formals <- modify.args(formals=.formals, arglist=tmp_args, dots=TRUE)
          .formals$... <- NULL
        }
        
        # tmp_measure <- try(do.call(fun, .formals, envir=env), silent=TRUE)
        tmp_measure <- try(do.call(fun, .formals), silent=TRUE)
        
        if(isTRUE(trace) | isTRUE(storage)) {
          # Subsitute 'StdDev' if the objective name is 'var'
          # if the user passes in var as an objective name, we are actually
          # calculating StdDev, so we need to change the name here.
          tmp_objname <- objective$name
          if(tmp_objname == "var") tmp_objname <- "StdDev"
          if(is.null(names(tmp_measure))) names(tmp_measure) <- tmp_objname
          tmp_return[[tmp_objname]] <- tmp_measure
        }
        
        if(inherits(tmp_measure, "try-error")) { 
          message(paste("objective name", objective$name, "generated an error or warning:", tmp_measure))
        } 
        
        # now set the new value of the objective output
        if(inherits(objective, "return_objective")){ 
          if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
            out <- out + penalty*abs(objective$multiplier)*abs(tmp_measure - objective$target)
          }  
          # target is null or doesn't exist, just maximize, or minimize violation of constraint
          out <- out + objective$multiplier*tmp_measure
        } # end handling for return objectives
        
        if(inherits(objective, "portfolio_risk_objective")){
          if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
            out <- out + penalty*abs(objective$multiplier)*abs(tmp_measure - objective$target)
            #should we also penalize risk too low for risk targets? or is a range another objective?
            #    # half penalty for risk lower than target
            #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
          }  
          # target is null or doesn't exist, just maximize, or minimize violation of constraint
          out <- out + abs(objective$multiplier)*tmp_measure
        } #  univariate risk objectives
        
        if(inherits(objective, "turnover_objective")){
          if (!is.null(objective$target) & is.numeric(objective$target)){ # we have a target
            out <- out + penalty*abs(objective$multiplier)*abs(tmp_measure - objective$target)
          }  
          # target is null or doesn't exist, just maximize, or minimize violation of constraint
          out <- out + abs(objective$multiplier)*tmp_measure
        } #  univariate turnover objectives
        
        if(inherits(objective, "minmax_objective")){
          if (!is.null(objective$min) & !is.null(objective$max)){ # we have a min and max
            if(tmp_measure > objective$max){
              out <- out + penalty * objective$multiplier * (tmp_measure - objective$max)
            }
            if(tmp_measure < objective$min){
              out <- out + penalty * objective$multiplier * (objective$min - tmp_measure)
            }
          }
        } # temporary minmax objective
        
        if(inherits(objective, "risk_budget_objective")){
          # setup
          
          # out = out + penalty*sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
          # add risk budget constraint
          if(!is.null(objective$target) & is.numeric(objective$target)){
            #in addition to a risk budget constraint, we have a univariate target
            # the first element of the returned list is the univariate measure
            # we'll use the  univariate measure exactly like we would as a separate objective
            out = out + penalty*abs(objective$multiplier)*abs(tmp_measure[[1]]-objective$target)
            #should we also penalize risk too low for risk targets? or is a range another objective?
            #    # half penalty for risk lower than target
            #    if(  prw < (.9*Riskupper) ){ out = out + .5*(penalty*( prw - Riskupper)) }
          }
          percrisk = tmp_measure[[3]] # third element is percent component contribution
          RBupper = objective$max_prisk
          RBlower = objective$min_prisk
          if(!is.null(RBupper) | !is.null(RBlower)){
            out = out + penalty * objective$multiplier * sum( (percrisk-RBupper)*( percrisk > RBupper ),na.rm=TRUE ) + penalty*sum( (RBlower-percrisk)*( percrisk < RBlower  ),na.rm=TRUE  )
          }
          #             if(!is.null(objective$min_concentration)){
          #                 if(isTRUE(objective$min_concentration)){
          #                     max_conc<-max(tmp_measure[[2]]) #second element is the contribution in absolute terms
          #                     # out=out + penalty * objective$multiplier * max_conc
          #                     out = out + objective$multiplier * max_conc
          #                 }
          #             }
          # Combined min_con and min_dif to take advantage of a better concentration obj measure
          if(!is.null(objective$min_difference) || !is.null(objective$min_concentration)){
            if(isTRUE(objective$min_difference)){
              # max_diff<-max(tmp_measure[[2]]-(sum(tmp_measure[[2]])/length(tmp_measure[[2]]))) #second element is the contribution in absolute terms
              # Uses Herfindahl index to calculate concentration; added scaling perc diffs back to univariate numbers
              max_diff <- sqrt(sum(tmp_measure[[3]]^2))/100  #third element is the contribution in percentage terms
              # out = out + penalty * objective$multiplier * max_diff
              out = out + penalty*objective$multiplier * max_diff
            }
            if(isTRUE(objective$min_concentration)){
              # use HHI to calculate concentration
              # actual HHI
              act_hhi <- sum(tmp_measure[[3]]^2)/100
              # minimum possible HHI
              min_hhi <- sum(rep(1/length(tmp_measure[[3]]), length(tmp_measure[[3]]))^2)/100
              out <- out + penalty * objective$multiplier * abs(act_hhi - min_hhi)
            }
          }
        } # end handling of risk_budget objective
        
        if(inherits(objective, "weight_concentration_objective")){
          # If the user does not pass in conc_groups, the output of HHI will be a scalar
          if((length(objective$conc_aversion) == 1) & is.null(objective$conc_groups)){
            # treat conc_aversion as a multiplier
            out <- out + penalty * objective$conc_aversion * tmp_measure
          }
          # If the user passes in conc_groups, the output of HHI will be a list
          # The second element of the list will be the group HHI
          if(length(objective$conc_aversion > 1) & !is.null(objective$conc_groups)){
            if(length(objective$conc_aversion) == length(tmp_measure[[2]])){
              # treat the conc_aversion vector as a multiplier per group hhi
              out <- out + penalty * sum(objective$conc_aversion * tmp_measure[[2]])
            }
          }
        } # weight concentration objective
        
      } # end enabled check
    } # end loop over objectives
  } # end objectives processing
  
  if(isTRUE(verbose)) {
    print('weights: ')
    print(paste(w,' '))
    print(paste("output of objective function", out))
    print(unlist(tmp_return))
  }
  
  if(is.na(out) | is.nan(out) | is.null(out)){
    #this should never happen
    warning('NA or NaN produced in objective function for weights ',w)
    out <- penalty
  }
  
  #return
  if (isTRUE(storage)){
    #add the new objective results
    store_output[[length(store_output)+1]] <- list(out=as.numeric(out), weights=w, init_weights=init_weights, objective_measures=tmp_return)
    # do the assign here
    assign('.objectivestorage', store_output, envir=.storage)
  }
  if(!isTRUE(trace)){
    return(out)
  } else {
    return(list(out=as.numeric(out), weights=w, objective_measures=tmp_return))
  }
}
