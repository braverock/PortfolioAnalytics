

# I am going to start with two levels. Once that is working, I will generalize
# to any arbitrary number of levels.

#' Multple Layer Portfolio Specification
#' 
#' Create and specify a multiple layer portfolio
#' 
#' The \code{sub.portfolios} slot is a list where each element contains the 
#' portfolio object and rebalancing parameters for the optimization of the 
#' sub portfolio.
#' This allows, for example, each sub portfolio to have different rebalancing 
#' frequencies (i.e. monthly or quarterly), optimization methods, etc.
#' 
#' Each sub portfolio is optimized with \code{optimize.portfolio.rebalancing} 
#' to create a time series of proxy returns. 
#' 
#' The "top level" portfolio is used to specify the constraints and objectives 
#' to control the optimization given the proxy returns of each sub portfolio. 
#' 
#' @param portfolio the "top level" portfolio
#' @param levels number of levels of sub-portfolios
#' @param \dots any additional parameters
#' @return a \code{mult.portfolio.spec} object with the top level portfolio 
#' and sub portfolios with optimization parameters for each sub portfolio
#' @author Ross Bennett
#' @export
mult.portfolio.spec <- function(portfolio, levels=2, ...){
  structure(c(list(top.portfolio = portfolio,
                   sub.portfolios = list()),
              list(...)),
            class="mult.portfolio.spec")
}

# constructor for sub.portfolio object
sub.portfolio <- function(portfolio, 
                          optimize_method = c("DEoptim","random","ROI","pso","GenSA"), 
                          search_size = 20000,
                          rp = NULL, 
                          rebalance_on = NULL, 
                          training_period = NULL, 
                          trailing_periods = NULL, 
                          ...){
  # Check to make sure that the portfolio passed in is a portfolio object
  if (!is.portfolio(portfolio)) stop("portfolio passed in is not of class 'portfolio'")
  
  # structure and return
  return(structure( c(list(portfolio = portfolio,
                           optimize_method = optimize_method[1],
                           search_size = search_size,
                           rp = rp,
                           rebalance_on = rebalance_on,
                           training_period = training_period,
                           trailing_periods = trailing_periods),
                      list(...)),
                    class="sub.portfolio"
  ) # end structure
  )
}

#' Add sub-portfolio
#' 
#' Add a sub-portfolio to a multiple layer portfolio specification object
#' 
#' @param mult.portfolio a \code{mult.portfolio.spec} object
#' @param portfolio a \code{portfolio} object to add as a sub portfolio.
#' @param optimize_method optimization method for the sub portfolio
#' @param search_size integer, how many portfolios to test, default 20,000
#' @param rp matrix of random portfolio weights, default NULL, mostly for automated use by rebalancing optimization or repeated tests on same portfolios
#' @param rebalance_on haracter string of period to rebalance on. See 
#' \code{\link[xts]{endpoints}} for valid names.
#' @param training_period an integer of the number of periods to use as 
#' a training data in the front of the returns data
#' @param trailing_periods an integer with the number of periods to roll over
#' (i.e. width of the moving or rolling window), the default is NULL will 
#' run using the returns data from inception 
#' @param \dots additonal passthrough parameters to \code{\link{optimize.portfolio.rebalancing}}
#' @param indexnum the index number of the sub portfolio. If \code{indexnum=NULL}
#' (the default), then the sub portfolio object is appended to the list of 
#' sub portfolios in the \code{mult.portfolio} object. If \code{indexnum} is 
#' specified, the portfolio in that index number is overwritten.
#' @seealso \code{\link{mult.portfolio.spec}} \code{\link{portfolio.spec}} \code{\link{optimize.portfolio}} \code{\link{optimize.portfolio.rebalancing}}
#' @author Ross Bennett
#' @export
add.sub.portfolio <- function(mult.portfolio, 
                              portfolio, 
                              optimize_method = c("DEoptim","random","ROI","pso","GenSA"), 
                              search_size = 20000,
                              rp = NULL, 
                              rebalance_on = NULL, 
                              training_period = NULL, 
                              trailing_periods = NULL,
                              ..., 
                              indexnum = NULL){
  # Check to make sure that the portfolio passed in is a portfolio mult.portfolio
  if(!inherits(mult.portfolio, "mult.portfolio.spec")) stop("mult.portfolio must be of class 'mult.portfolio.spec'")
  
  # construct a sub portfolio object
  tmp_portfolio <- sub.portfolio(portfolio=portfolio, 
                                 optimize_method=optimize_method[1], 
                                 search_size=search_size, 
                                 rp=rp, 
                                 rebalance_on=rebalance_on, 
                                 training_period=training_period, 
                                 trailing_periods=trailing_periods, 
                                 ...=...)
  
  if(inherits(tmp_portfolio, "sub.portfolio")){
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))){
      indexnum <- length(mult.portfolio$sub.portfolios)+1
    }
    mult.portfolio$sub.portfolios[[indexnum]] <- tmp_portfolio
  }
  return(mult.portfolio)
}

# This function calls optimize.portfolio.rebalancing on each sub portfolio
# according to the given optimization parameters and returns an xts object
# representing the proxy returns of each sub portfolio
proxy.mult.portfolio <- function(R, mult.portfolio, ...){
  # Check to make sure that the mult.portfolio passed in is a 
  # mult.portfolio.spec object
  if(!inherits(mult.portfolio, "mult.portfolio.spec")){
    stop("mult.portfolio must be of class 'mult.portfolio.spec'")
  }
  
  n.sub.portfolios <- length(mult.portfolio$sub.portfolios)
  if(n.sub.portfolios <= 1) stop("Must have more than 1 sub portfolio")
  
  # Initialize list to store the returns for each sub portfolio
  ret <- vector("list", n.sub.portfolios)
  
  # Loop through the sub portfolios and call optimize.portfolio.rebalancing
  # on each sub portfolio and its optimization parameters
  for(i in 1:n.sub.portfolios){
    #print(paste("sub portfolio", i))
    tmp <- mult.portfolio$sub.portfolios[[i]]
    
    # We need to subset the R object based on the names of portfolio$assets in 
    # the sub portfolio
    # This requires that asset names match colnames(R)
    R.tmp <- R[,names(tmp$portfolio$assets)]
    if(ncol(R.tmp) != length(tmp$portfolio$assets)){
      stop("R object of returns not subset correctly. Make sure the names of 
           the assets in the sub portfolio match the column names of the R object") 
    }
    # This needs to support anything in ... that could be passed to optimize.portfolio
    .formals <- formals(optimize.portfolio.rebalancing)
    .formals <- modify.args(formals=.formals, arglist=NULL, R=R, dots=TRUE)
    .formals <- modify.args(formals=.formals, arglist=tmp, dots=TRUE)
    .formals$... <- NULL
    #print(.formals)
    opt <- try(do.call(optimize.portfolio.rebalancing, .formals), silent=TRUE)
    if(!inherits(opt, "try-error")) {
      w <- extractWeights(opt)
      # geometric chaining TRUE/FALSE, should be FALSE if any weights are negative
      g <- ifelse(any(w < 0), FALSE, TRUE)
      ret.tmp <- Return.portfolio(R.tmp, weights = w, geometric = g)
      colnames(ret.tmp) <- paste("proxy", i, sep=".")
      ret[[i]] <- ret.tmp
      #print(ret[[i]])
    } else {
      stop(paste("optimize.portfolio.rebalancing for sub portfolio", i, "generated an error or warning:", opt))
    }
  }
  proxy.ret <- na.omit(do.call(cbind, ret))
  return(proxy.ret)
}


