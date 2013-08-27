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

#' Extract the efficient frontier of portfolios that meet your objectives over a range of risks
#' 
#' The efficient frontier is extracted from the set of portfolios created by 
#' \code{optimize.portfolio} with \code{trace=TRUE}.
#' 
#' If you do not have an optimal portfolio object created by 
#' \code{\link{optimize.portfolio}}, you can pass in a portfolio object and an
#' optimization will be run via \code{\link{optimize.portfolio}}
#' 
#' @note
#' Note that this function will be extremely sensitive to the objectives in your
#' \code{\link{portfolio}} object.  It will be especially obvious if you 
#' are looking at a risk budget objective and your return preference is not set high enough.
#' 
#'  
#' @param object optimial portfolio object as created by \code{\link{optimize.portfolio}}
#' @param from minimum value of the sequence
#' @param to maximum value of the sequence
#' @param by number to increment the sequence by
#' @param match.col string name of column to use for risk (horizontal axis)
#' @param \dots any other passthru parameters to \code{optimize.portfolio}
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization, see \code{\link{portfolio.spec}}
#' @param optimize_method one of "DEoptim", "random", "ROI", "pso", or "GenSA"
#' @export
extract.efficient.frontier <- function (object=NULL, match.col='ES', from=NULL, to=NULL, by=0.005, n.portfolios=NULL, ..., R=NULL, portfolio=NULL, optimize_method='random')
{
    #TODO add a threshold argument for how close it has to be to count
    # do we need to recalc the constrained_objective too?  I don't think so.
    if(!inherits(object, "optimize.portfolio")) stop("object passed in must of of class 'portfolio'")
    
    #set<-seq(from=from,to=to,by=by)
    #set<-cbind(quantmod::Lag(set,1),as.matrix(set))[-1,]
    if(is.null(object)){
        if(!is.null(R) & !is.null(portfolio)){
            portfolios<-optimize.portfolio(portfolio=portfolio, R=R, optimize_method=optimize_method[1], trace=TRUE, ...)
        } else {
            stop('you must specify a portfolio object and a return series or an objective of class optimize.portfolio')
        }
    }
    
    xtract<-extractStats(object)
    columnnames=colnames(xtract)
    # optimal portfolio stats from xtract
    opt <- xtract[which.min(xtract[, "out"]),]
    #if("package:multicore" %in% search() || require("multicore",quietly = TRUE)){
    #    mclapply
    #}
    stopifnot("package:foreach" %in% search() || require("foreach",quietly = TRUE))
#    rtc = pmatch(return.col,columnnames)
#    if(is.na(rtc)) {
#        rtc = pmatch(paste(return.col,return.col,sep='.'),columnnames)
#    }
    mtc = pmatch(match.col,columnnames)
    if(is.na(mtc)) {
        mtc = pmatch(paste(match.col,match.col,sep='.'),columnnames)
    }
    if(is.na(mtc)) stop("could not match match.col with column name of extractStats output")
    
    if(is.null(from)){
      from <- min(xtract[, mtc])
    }
    if(is.null(to)){
      to <- max(xtract[, mtc])
    }
    if(!is.null(n.portfolios)){
      # create the sequence using length.out if the user has specified a value for the n.portfolios arg
      set<-seq(from=from, to=to, length.out=n.portfolios)
    } else {
      # fall back to using by to create the sequence
      set<-seq(from=from, to=to, by=by)
    }
    
    set<-cbind(quantmod::Lag(set,1),as.matrix(set))[-1,]
    result <- foreach(i=1:nrow(set),.inorder=TRUE, .combine=rbind, .errorhandling='remove') %do% {
        tmp<-xtract[which(xtract[,mtc]>=set[i,1] & xtract[,mtc]<set[i,2]),]
        #tmp<-tmp[which.min(tmp[,'out']),]
        tmp<-tmp[which.max(tmp[,'mean']),]
        #tmp
    }
    # combine the stats from the optimal portfolio to result matrix
    result <- rbind(opt, result)
    return(structure(result, class="efficient.frontier"))
}

#' Generate the efficient frontier for a mean-variance portfolio
#' 
#' This function generates the mean-variance efficient frontier of a portfolio
#' specifying constraints and objectives. To generate the mean-var efficient 
#' frontier, the portfolio must have two objectives 1) "mean" and 2) "var".
#' 
#' @param portfolio a portfolio object with constraints and objectives created via \code{\link{portfolio.spec}}
#' @param R an xts or matrix of asset returns
#' @param n.portfolios number of portfolios to plot along the efficient frontier
#' @return a matrix of objective measure values and weights along the efficient frontier
#' @author Ross Bennett
#' @export
meanvar.efficient.frontier <- function(portfolio, R, n.portfolios=25){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  # step 1: find the minimum return given the constraints
  # step 2: find the maximum return given the constraints
  # step 3: 'step' along the returns and run the optimization to calculate
  # the weights and objective measures along the efficient frontier
  
  # get the names of the objectives
  objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  
  if(length(objnames) == 1){
    if(objnames == "mean"){
      # The user has only passed in a mean objective, add a var objective to the portfolio
      portfolio <- add.objective(portfolio=portfolio, type="risk", name="var")
    } else if(objnames == "var"){
      # The user has only passed in a var objective, add a mean objective
      portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")
    }
    # get the objective names again after we add an objective to the portfolio
    objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  }
  
  # for a mean-var efficient frontier, there must be two objectives 1) "mean" and 2) "var"
  if(!((length(objnames) == 2) & ("var" %in% objnames) & ("mean" %in% objnames))){
    stop("The portfolio object must have both 'mean' and 'var' specified as objectives")
  }
  
  # If the user has passed in a portfolio object with return_constraint, we need to disable it
  for(i in 1:length(portfolio$constraints)){
    if(inherits(portfolio$constraints[[i]], "return_constraint")){
      portfolio$constraints[[i]]$enabled <- FALSE
    }
  }
  
  # get the index number of the var objective 
  var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "var")
  # get the index number of the mean objective
  mean_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "mean")
  
  # set the risk_aversion to a very small number for equivalent to max return portfolio
  portfolio$objectives[[var_idx]]$risk_aversion <- 1e-6
  
  # run the optimization to get the maximum return
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI")
  maxret <- extractObjectiveMeasures(tmp)$mean
  
  # set the risk_aversion to a very large number equivalent to a minvar portfolio
  portfolio$objectives[[var_idx]]$risk_aversion <- 1e6
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI")
  stats <- extractStats(tmp)
  minret <- stats["mean"]
  
  # length.out is the number of portfolios to create
  ret_seq <- seq(from=minret, to=maxret, length.out=n.portfolios)
  
  out <- matrix(0, nrow=length(ret_seq), ncol=length(extractStats(tmp)))
  
  for(i in 1:length(ret_seq)){
    portfolio$objectives[[mean_idx]]$target <- ret_seq[i]
    out[i, ] <- extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI"))
  }
  colnames(out) <- names(stats)
  return(structure(out, class="efficient.frontier"))
}

#' Generate the efficient frontier for a mean-etl portfolio
#' 
#' This function generates the mean-etl efficient frontier of a portfolio
#' specifying constraints and objectives. To generate the mean-var efficient 
#' frontier, the portfolio must have two objectives 1) "mean" and 2) "ETL/ES/CVaR". If
#' the only objective in the \code{portfolio} object is ETL/ES/CVaR, the we will
#' add a mean objective.
#' 
#' @param portfolio a portfolio object with constraints and objectives created via \code{\link{portfolio.spec}}
#' @param R an xts or matrix of asset returns
#' @param n.portfolios number of portfolios to plot along the efficient frontier
#' @return a matrix of objective measure values and weights along the efficient frontier
#' @author Ross Bennett
#' @export
meanetl.efficient.frontier <- function(portfolio, R, n.portfolios=25){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  # step 1: find the minimum return given the constraints
  # step 2: find the maximum return given the constraints
  # step 3: 'step' along the returns and run the optimization to calculate
  # the weights and objective measures along the efficient frontier
  
  objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  
  if(length(objnames) == 1){
    if(objnames == "mean"){
      # The user has only passed in a mean objective, add ES objective to the portfolio
      portfolio <- add.objective(portfolio=portfolio, type="risk", name="ES")
    } else if(objnames %in% c("ETL", "ES", "CVaR")){
      # The user has only passed in ETL/ES/CVaR objective, add a mean objective
      portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")
    }
    # get the objective names again after we add an objective to the portfolio
    objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  }
  
  # for a mean-etl efficient frontier, there must be two objectives 1) "mean" and 2) "ETL/ES/CVaR"
  # get the names of the objectives
  if(!((length(objnames) == 2) & any(objnames %in% c("ETL", "ES", "CVaR")) & ("mean" %in% objnames))){
    stop("The portfolio object must have both 'mean' and 'var' specified as objectives")
  }
  
  # If the user has passed in a portfolio object with return_constraint, we need to disable it
  for(i in 1:length(portfolio$constraints)){
    if(inherits(portfolio$constraints[[i]], "return_constraint")){
      portfolio$constraints[[i]]$enabled <- FALSE
    }
  }
  
  # get the index number of the etl objective
  etl_idx <- which(objnames %in% c("ETL", "ES", "CVaR"))
  # get the index number of the mean objective
  mean_idx <- which(objnames == "mean")
  
  # create a temporary portfolio to find the max mean return
  ret_obj <- return_objective(name="mean")
  tportf <- insert_objectives(portfolio, list(ret_obj))
  
  # run the optimization to get the maximum return
  tmp <- optimize.portfolio(R=R, portfolio=tportf, optimize_method="ROI")
  maxret <- extractObjectiveMeasures(tmp)$mean
  
  # run the optimization to get the return at the min ETL portfolio
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI")
  stats <- extractStats(tmp)
  minret <- stats["mean"]
  
  # length.out is the number of portfolios to create
  ret_seq <- seq(from=minret, to=maxret, length.out=n.portfolios)
  
  out <- matrix(0, nrow=length(ret_seq), ncol=length(extractStats(tmp)))
  
  for(i in 1:length(ret_seq)){
    portfolio$objectives[[mean_idx]]$target <- ret_seq[i]
    out[i, ] <- extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI"))
  }
  colnames(out) <- names(stats)
  return(structure(out, class="efficient.frontier"))
}

#' create an efficient frontier
#' 
#' @details currently there are 4 'types' supported to create an efficient frontier
#' \itemize{
#'   \item{"mean-var", "mean-sd", or "mean-StdDev":}{ This is a special case for 
#'   an efficient frontier that can be created by a QP solver.
#'   The \code{portfolio} object should have two
#'   objectives: 1) mean and 2) var. The efficient frontier will be created via
#'   \code{\link{meanvar.efficient.frontier}}.}
#'   \item{"mean-ETL", "mean-ES", "mean-CVaR", "mean-etl"}{ This is a special 
#'   case for an efficient frontier that can be created by an LP solver.
#'   The \code{portfolio} object should have two objectives: 1) mean
#'   and 2) ETL/ES/CVaR. The efficient frontier will be created via
#'   \code{\link{meanetl.efficient.frontier}}.}
#'   \item{"DEoptim"}{ This can handle more complex constraints and objectives
#'   than the simple mean-var and mean-ETL cases. For this type, we actually 
#'   call \code{\link{optimize.portfolio}} with \code{optimize_method="DEoptim"}
#'   and then extract the efficient frontier with 
#'   \code{\link{extract.efficient.frontier}}.}
#'   \item{"random"}{ This can handle more complex constraints and objectives
#'   than the simple mean-var and mean-ETL cases. For this type, we actually 
#'   call \code{\link{optimize.portfolio}} with \code{optimize_method="random"}
#'   and then extract the efficient frontier with 
#'   \code{\link{extract.efficient.frontier}}.}
#' }
#' 
#' @param R xts of asset returns
#' @param portfolio object of class 'portfolio' specifying the constraints and objectives, see \code{\link{portfolio.spec}}
#' @param type type of efficient frontier, see details
#' @param n.portfolios number of portfolios to calculate along the efficient frontier
#' @param match.col column to match when extracting the efficient frontier from an objected created by optimize.portfolio
#' @param seach_size passed to optimize.portfolio for type="DEoptim" or type="random"
#' @param ... passthrough parameters to \code{\link{optimize.portfolio}}
#' @return an object of class 'efficient.frontier' with the objective measures 
#' and weights of portfolios along the efficient frontier
#' @author Ross Bennett
#' @seealso \code{\link{optimize.portfolio}}, 
#' \code{\link{portfolio.spec}}, 
#' \code{\link{meanvar.efficient.frontier}}, 
#' \code{\link{meanetl.efficient.frontier}}, 
#' \code{\link{extract.efficient.frontier}}
#' @export
create.EfficientFrontier <- function(R, portfolio, type, n.portfolios=25, match.col="ES", search_size=2000, ...){
  # This is just a wrapper around a few functions to easily create efficient frontiers
  # given a portfolio object and other parameters
  call <- match.call()
  if(!is.portfolio(portfolio)) stop("portfolio must be of class 'portfolio'")
  type <- type[1]
  switch(type,
         "mean-sd"=,
         "mean-StdDev"=,
         "mean-var" = {frontier <- meanvar.efficient.frontier(portfolio=portfolio,
                                                              R=R, 
                                                              n.portfolios=n.portfolios)
         },
         "mean-ETL"=,
         "mean-CVaR"=,
         "mean-ES"=,
         "mean-etl" = {frontier <- meanetl.efficient.frontier(portfolio=portfolio, 
                                                              R=R, 
                                                              n.portfolios=n.portfolios)
         },
         "random" = {tmp <- optimize.portfolio(R=R, 
                                               portfolio=portfolio, 
                                               optimize_method=type, 
                                               trace=TRUE, 
                                               search_size=search_size,
                                               ...=...)
                     frontier <- extract.efficient.frontier(object=tmp, 
                                                            match.col=match.col, 
                                                            n.portfolios=n.portfolios)
         },
         "DEoptim" = {tmp <- optimize.portfolio(R=R, 
                                                portfolio=portfolio, 
                                                optimize_method=type, 
                                                trace=TRUE, 
                                                search_size=search_size,
                                                ...=...)
                      frontier <- extract.efficient.frontier(object=tmp, 
                                                             match.col=match.col, 
                                                             n.portfolios=n.portfolios)
         }
  )
  return(structure(list(call=call,
                        frontier=frontier,
                        R=R,
                        portfolio=portfolio), class="efficient.frontier"))
}

#' Extract the efficient frontier data points
#' 
#' This function extracts the efficient frontier from an object created by
#' \code{\link{optimize.portfolio}}.
#' 
#' If the object is an \code{optimize.portfolio.ROI} object and \code{match.col}
#'  is "ES", "ETL", or "CVaR", then the mean-ETL efficient frontier will be 
#'  created via \code{meanetl.efficient.frontier}. 
#' 
#' If the object is an \code{optimize.portfolio.ROI} object and \code{match.col}
#'  is "StdDev", then the mean-StdDev efficient frontier will be created via 
#' \code{meanvar.efficient.frontier}. Note that if 'var' is specified as the
#' name of an objective, the value returned will be 'StdDev'.
#' 
#' For objects created by \code{optimize.portfolo} with the DEoptim, random, or
#' pso solvers, the efficient frontier will be extracted from the object via
#' \code{extract.efficient.frontier}. This means that \code{optimize.portfolio} must
#' be run with \code{trace=TRUE}
#' 
#' @param object an optimal portfolio object created by \code{optimize.portfolio}
#' @param match.col string name of column to use for risk (horizontal axis).
#' \code{match.col} must match the name of an objective measure in the 
#' \code{objective_measures} or \code{opt_values} slot in the object created 
#' by \code{\link{optimize.portfolio}}.
#' @param n.portfolios number of portfolios to use to plot the efficient frontier
#' @return an \code{efficient.frontier} object with weights and other metrics along the efficient frontier
#' @author Ross Bennett
#' @export
extractEfficientFrontier <- function(object, match.col="ES", n.portfolios=25){
  # extract the efficient frontier from an optimize.portfolio output object
  call <- match.call()
  if(!inherits(object, "optimize.portfolio")) stop("object must be of class 'optimize.portfolio'")
  
  if(inherits(object, "optimize.portfolio.GenSA")){
    stop("GenSA does not return any useable trace information for portfolios tested, thus we cannot extract an efficient frontier.")
  }
  
  # get the portfolio and returns 
  portf <- object$portfolio
  R <- object$R
  if(is.null(R)) stop(paste("Not able to get asset returns from", object, "run optimize.portfolio with trace=TRUE"))
  
  # get the objective names and check if match.col is an objective name
  # objnames <- unlist(lapply(portf$objectives, function(x) x$name))
  # if(!(match.col %in% objnames)){
  #   stop("match.col must match an objective name")
  # }
  
  # We need to create the efficient frontier if object is of class optimize.portfolio.ROI
  if(inherits(object, "optimize.portfolio.ROI")){
    if(match.col %in% c("ETL", "ES", "CVaR")){
      frontier <- meanetl.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
    }
    if(match.col == "StdDev"){
      frontier <- meanvar.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
    }
  } # end optimize.portfolio.ROI
  
  # use extract.efficient.frontier for otpimize.portfolio output objects with global solvers
  if(inherits(object, c("optimize.portfolio.random", "optimize.portfolio.DEoptim", "optimize.portfolio.pso"))){
    frontier <- extract.efficient.frontier(object=object, match.col=match.col, n.portfolios=n.portfolios)
  }
  return(structure(list(call=call,
                        frontier=frontier,
                        R=R,
                        portfolio=portfolio), class="efficient.frontier"))
}

