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
  #if("package:multicore" %in% search() || requireNamespace("multicore",quietly = TRUE)){
  #    mclapply
  #}
  stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
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
  i <- 1
  result <- foreach::foreach(i=1:nrow(set),.inorder=TRUE, .combine=rbind, .errorhandling='remove') %do% {
    tmp<-xtract[which(xtract[,mtc]>=set[i,1] & xtract[,mtc]<set[i,2]),]
    #tmp<-tmp[which.min(tmp[,'out']),]
    tmp<-tmp[which.max(tmp[,'mean']),]
    #tmp
  }
  # combine the stats from the optimal portfolio to result matrix
  result <- rbind(opt, result)
  return(structure(result, class="frontier"))
}

#' Generate the efficient frontier for a mean-variance portfolio
#' 
#' This function generates the mean-variance efficient frontier of a portfolio
#' specifying the constraints and objectives. The \code{portfolio} object 
#' should have two objectives: 1) mean and 2) var (or sd or StdDev). If the 
#' portfolio object does not contain these objectives, they will be added 
#' using default parameters.
#' 
#' @param portfolio a portfolio object with constraints created via \code{\link{portfolio.spec}}
#' @param R an xts or matrix of asset returns
#' @param n.portfolios number of portfolios to plot along the efficient frontier
#' @param risk_aversion vector of risk_aversion values to construct the efficient frontier.
#' \code{n.portfolios} is ignored if \code{risk_aversion} is specified and the number
#' of points along the efficient frontier is equal to the length of \code{risk_aversion}.
#' @param \dots passthru parameters to \code{\link{optimize.portfolio}}
#' @return a matrix of objective measure values and weights along the efficient frontier
#' @author Ross Bennett
#' @export
meanvar.efficient.frontier <- function(portfolio, R, n.portfolios=25, risk_aversion=NULL, ...){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  # step 1: find the minimum return given the constraints
  # step 2: find the maximum return given the constraints
  # step 3: 'step' along the returns and run the optimization to calculate
  # the weights and objective measures along the efficient frontier
  
  # Use the portfolio_risk_objective from the portfolio if they have it
  # check for a var, StdDev, or sd objective
  var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name))  %in% c("var", "StdDev", "sd"))
  if(length(var_idx) >= 1){
    # the portfolio object has a var, StdDev, or sd objective
    var_obj <- portfolio$objectives[[var_idx[1]]]
  } else {
    var_obj <- portfolio_risk_objective(name="var")
  }
  
  hhi_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name))  == "HHI")
  if(length(hhi_idx) >= 1){
    # the portfolio object has an HHI objective
    hhi_obj <- portfolio$objectives[[hhi_idx[1]]]
  } else {
    hhi_obj <- NULL
  }
  
  # Clear out the objectives in portfolio and add them here to simplify checks
  # and so we can control the optimization along the efficient frontier.
  portfolio$objectives <- list()
  portfolio$objectives[[1]] <- var_obj
  portfolio$objectives[[2]] <- hhi_obj
  portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")
  
  # If the user has passed in a portfolio object with return_constraint, we need to disable it
  for(i in 1:length(portfolio$constraints)){
    if(inherits(portfolio$constraints[[i]], "return_constraint")){
      portfolio$constraints[[i]]$enabled <- FALSE
    }
  }
  
  # get the index number of the var objective 
  var_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name))  %in% c("var", "StdDev", "sd"))
  # get the index number of the mean objective
  mean_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "mean")
  # get the index number of the hhi objective
  hhi_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name)) == "HHI")
  
  ##### get the maximum return #####
  
  # Disable the risk objective and hhi objective if applicable
  portfolio$objectives[[var_idx]]$enabled <- FALSE
  if(length(hhi_idx) >= 1) portfolio$objectives[[hhi_idx]]$enabled <- FALSE
  
  # run the optimization to get the maximum return
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ...=...)
  mean_ret <- colMeans(R)
  maxret <- sum(extractWeights(tmp) * mean_ret)
  
  ##### Get the return at the minimum variance portfolio #####
  
  # Disable the return objective
  portfolio$objectives[[mean_idx]]$enabled <- FALSE
  
  # Enable the risk objective and hhi objective if applicable
  portfolio$objectives[[var_idx]]$enabled <- TRUE
  if(length(hhi_idx) >= 1) portfolio$objectives[[hhi_idx]]$enabled <- TRUE
  
  # Run the optimization to get the global minimum variance portfolio with the
  # given constraints.
  # Do we want to disable the turnover or transaction costs constraints here?
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ...=...)
  stats <- extractStats(tmp)
  minret <- sum(extractWeights(tmp) * mean_ret)
  
  # length.out is the number of portfolios to create
  ret_seq <- seq(from=minret, to=maxret, length.out=n.portfolios)
  
  # Add target return constraint to step along the efficient frontier for target returns
  portfolio <- add.constraint(portfolio=portfolio, type="return", return_target=minret, enabled=FALSE)
  ret_constr_idx <- which(unlist(lapply(portfolio$constraints, function(x) inherits(x, "return_constraint"))))
  
  stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
  stopifnot("package:iterators" %in% search() || requireNamespace("iterators",quietly = TRUE))
  if(!is.null(risk_aversion)){
    # Enable the return objective so we are doing quadratic utility
    portfolio$objectives[[mean_idx]]$enabled <- TRUE
    lambda <- risk_aversion[1]
    out <- foreach::foreach(lambda=iterators::iter(risk_aversion), .inorder=TRUE, .combine=rbind, .errorhandling='remove', .packages='PortfolioAnalytics') %dopar% {
      portfolio$objectives[[var_idx]]$risk_aversion <- lambda
      extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ...=...))
    }
    out <- cbind(out, risk_aversion)
    colnames(out) <- c(names(stats), "lambda")
  } else {
    # Enable the return constraint
    portfolio$constraints[[ret_constr_idx]]$enabled <- TRUE
    ret <- ret_seq[1]
    out <- foreach::foreach(ret=iterators::iter(ret_seq), .inorder=TRUE, .combine=rbind, .errorhandling='remove', .packages='PortfolioAnalytics') %dopar% {
      portfolio$constraints[[ret_constr_idx]]$return_target <- ret
      opt <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ...=...)
      c(sum(extractWeights(opt) * mean_ret), extractStats(opt))
    }
    colnames(out) <- c("mean", names(stats))
  }
  out <- na.omit(out)
  return(structure(out, class="frontier"))
}

#' Generate the efficient frontier for a mean-etl portfolio
#' 
#' This function generates the mean-ETL efficient frontier of a portfolio
#' specifying the constraints and objectives. The \code{portfolio} object 
#' should have two objectives: 1) mean and 2) ES (or ETL or cVaR). If the 
#' portfolio object does not contain these objectives, they will be added 
#' using default parameters.
#' 
#' @param portfolio a portfolio object with constraints and objectives created via \code{\link{portfolio.spec}}
#' @param R an xts or matrix of asset returns
#' @param n.portfolios number of portfolios to generate the efficient frontier
#' @param \dots passthru parameters to \code{\link{optimize.portfolio}}
#' @return a matrix of objective measure values and weights along the efficient frontier
#' @author Ross Bennett
#' @export
meanetl.efficient.frontier <- function(portfolio, R, n.portfolios=25, ...){
  if(!is.portfolio(portfolio)) stop("portfolio object must be of class 'portfolio'")
  # step 1: find the minimum return given the constraints
  # step 2: find the maximum return given the constraints
  # step 3: 'step' along the returns and run the optimization to calculate
  # the weights and objective measures along the efficient frontier
  
  # Use the portfolio_risk_objective from the portfolio if they have it
  # check for a ETL, ES, or cVaR objective
  etl_idx <- which(unlist(lapply(portfolio$objectives, function(x) x$name))  %in% c("ETL", "ES", "CVaR"))
  if(length(etl_idx) >= 1){
    # the portfolio object has a ETL, ES, CVaR objective
    etl_obj <- portfolio$objectives[[etl_idx[1]]]
  } else {
    etl_obj <- portfolio_risk_objective(name="ES", arguments=list(p=0.95))
  }
  
  # Clear out the objectives in portfolio and add them here to simplify checks
  # and so we can control the optimization along the efficient frontier.
  portfolio$objectives <- list()
  portfolio$objectives[[1]] <- etl_obj
  portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")
  
  # get the objective names from the portfolio object
  objnames <- unlist(lapply(portfolio$objectives, function(x) x$name))
  
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
  tmp <- optimize.portfolio(R=R, portfolio=tportf, optimize_method="ROI", ...)
  maxret <- extractObjectiveMeasures(tmp)$mean
  
  # run the optimization to get the return at the min ETL portfolio
  tmp <- optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ef=TRUE, ...)
  stats <- extractStats(tmp)
  minret <- stats["mean"]
  
  # length.out is the number of portfolios to create
  ret_seq <- seq(from=minret, to=maxret, length.out=n.portfolios)
  
  #   out <- matrix(0, nrow=length(ret_seq), ncol=length(extractStats(tmp)))
  #   for(i in 1:length(ret_seq)){
  #     portfolio$objectives[[mean_idx]]$target <- ret_seq[i]
  #     out[i, ] <- extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI"))
  #   }
  stopifnot("package:foreach" %in% search() || requireNamespace("foreach",quietly = TRUE))
  stopifnot("package:iterators" %in% search() || requireNamespace("iterators",quietly = TRUE))
  ret <- ret_seq[1]
  out <- foreach::foreach(ret=iterators::iter(ret_seq), .inorder=TRUE, .combine=rbind, .errorhandling='remove', .packages='PortfolioAnalytics') %dopar% {
    portfolio$objectives[[mean_idx]]$target <- ret
    extractStats(optimize.portfolio(R=R, portfolio=portfolio, optimize_method="ROI", ef=TRUE, ...=...))
  }
  colnames(out) <- names(stats)
  out <- na.omit(out)
  return(structure(out, class="frontier"))
}

#' create an efficient frontier
#' 
#' @details Currently there are 4 'types' supported to create an efficient frontier:
#' \itemize{
#'   \item{"mean-var", "mean-sd", or "mean-StdDev":}{ This is a special case for 
#'   an efficient frontier that can be created by a QP solver.
#'   The \code{portfolio} object should have two
#'   objectives: 1) mean and 2) var. If the portfolio object does not contain these 
#'   objectives, they will be added using default parameters.
#'   The efficient frontier will be created via
#'   \code{\link{meanvar.efficient.frontier}}.}
#'   \item{"mean-ETL", "mean-ES", "mean-CVaR", "mean-etl":}{ This is a special 
#'   case for an efficient frontier that can be created by an LP solver.
#'   The \code{portfolio} object should have two objectives: 1) mean
#'   and 2) ETL/ES/CVaR. If the portfolio object does not contain these 
#'   objectives, they will be added using default parameters.
#'   The efficient frontier is created via 
#'   \code{\link{meanetl.efficient.frontier}}.}
#'   \item{"DEoptim":}{ This can handle more complex constraints and objectives
#'   than the simple mean-var and mean-ETL cases. For this type, we actually 
#'   call \code{\link{optimize.portfolio}} with \code{optimize_method="DEoptim"}
#'   and then extract the efficient frontier with 
#'   \code{extract.efficient.frontier}.}
#'   \item{"random":}{ This can handle more complex constraints and objectives
#'   than the simple mean-var and mean-ETL cases. For this type, we actually 
#'   call \code{\link{optimize.portfolio}} with \code{optimize_method="random"}
#'   and then extract the efficient frontier with 
#'   \code{extract.efficient.frontier}.}
#' }
#' 
#' @param R xts object of asset returns
#' @param portfolio object of class 'portfolio' specifying the constraints and objectives, see \code{\link{portfolio.spec}}.
#' @param type type of efficient frontier, see Details.
#' @param n.portfolios number of portfolios to calculate along the efficient frontier
#' @param risk_aversion vector of risk_aversion values to construct the efficient frontier.
#' \code{n.portfolios} is ignored if \code{risk_aversion} is specified and the number
#' of points along the efficient frontier will be equal to the length of \code{risk_aversion}.
#' @param match.col column to match when extracting the efficient frontier from an objected created by \code{\link{optimize.portfolio}}.
#' @param search_size passed to \code{\link{optimize.portfolio}} for type="DEoptim" or type="random".
#' @param \dots passthrough parameters to \code{\link{optimize.portfolio}}.
#' @return an object of class 'efficient.frontier' with the objective measures 
#' and weights of portfolios along the efficient frontier.
#' @author Ross Bennett
#' @seealso \code{\link{optimize.portfolio}}, 
#' \code{\link{portfolio.spec}}, 
#' \code{\link{meanvar.efficient.frontier}}, 
#' \code{\link{meanetl.efficient.frontier}}
#' @export
create.EfficientFrontier <- function(R, portfolio, type, n.portfolios=25, risk_aversion=NULL, match.col="ES", search_size=2000, ...){
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
                                                              n.portfolios=n.portfolios,
                                                              risk_aversion=risk_aversion,
                                                              ...=...)
         },
         "mean-ETL"=,
         "mean-CVaR"=,
         "mean-ES"=,
         "mean-etl" = {frontier <- meanetl.efficient.frontier(portfolio=portfolio, 
                                                              R=R, 
                                                              n.portfolios=n.portfolios,
                                                              ...=...)
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
#' be run with \code{trace=TRUE}.
#' 
#' @param object an optimal portfolio object created by \code{optimize.portfolio}
#' @param match.col string name of column to use for risk (horizontal axis).
#' \code{match.col} must match the name of an objective measure in the 
#' \code{objective_measures} or \code{opt_values} slot in the object created 
#' by \code{\link{optimize.portfolio}}.
#' @param n.portfolios number of portfolios to use to plot the efficient frontier
#' @param risk_aversion vector of risk_aversion values to construct the efficient frontier.
#' \code{n.portfolios} is ignored if \code{risk_aversion} is specified and the number
#' of points along the efficient frontier is equal to the length of \code{risk_aversion}.
#' @return an \code{efficient.frontier} object with weights and other metrics along the efficient frontier
#' @author Ross Bennett
#' @export
extractEfficientFrontier <- function(object, match.col="ES", n.portfolios=25, risk_aversion=NULL){
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
      frontier <- meanvar.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios, risk_aversion=risk_aversion)
    }
  } # end optimize.portfolio.ROI
  
  # use extract.efficient.frontier for otpimize.portfolio output objects with global solvers
  if(inherits(object, c("optimize.portfolio.random", "optimize.portfolio.DEoptim", "optimize.portfolio.pso"))){
    frontier <- extract.efficient.frontier(object=object, match.col=match.col, n.portfolios=n.portfolios)
  }
  return(structure(list(call=call,
                        frontier=frontier,
                        R=R,
                        portfolio=portf), class="efficient.frontier"))
}

