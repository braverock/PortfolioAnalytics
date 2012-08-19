#' provides multi-level sector-based geometric attribution
#' 
#' Provides multi-level sector-based geometric attribution. The Brinson model
#' attributes excess returns at one level. This function works with more 
#' complex decision processes. For instance, the 3-level decision process 
#' may have the following levels: type of asset - country - sector. The 
#' levels should be specified in the vector with elements in the particular 
#' order: from the highest level to the lowest. Returns and weighs for 
#' portfolio and benchmark should be at the lowest level (e.g. individual 
#' instruments). Benchmark should have the same number of columns as portfolio.
#' That is there should be a benchmark for each instrument in the portfolio 
#' (possibly 0). The contribution to the allocation in the \eqn{i^{th}}
#' category for the \eqn{d^{th}} level is: 
#' \deqn{\left(^{d}w_{pi}-^{d}w_{bi}\right)\times
#' \left(\frac{1+^{d}R_{bi}}{1+^{d-1}R_{bi}}-1\right)
#' \times\frac{1+^{d-1}R_{bi}}{1+bs^{d-1}}}
#' The total attribution for each asset allocation step in the decision process
#' is: \deqn{\frac{1+^{d}bs}{1+^{d-1}bs}-1}
#' The final step, stock selection, is measured by:
#' \deqn{^{d}w_{pi}\times\left(\frac{1+R_{pi}}{1+^{d}R_{bi}}-1\right)
#' \times\frac{1+^{d}R_{bi}}{1+^{d}bs}}
#' 
#' @aliases Attribution.levels
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param h data.frame with the hierarchy obtained from the buildHierarchy 
#' function or defined manually in the same style as buildHierarchy's
#' output
#' @param \dots any other passthrough parameters
#' @return returns the list with geometric excess returns including annualized
#' geometric excess returns, total attribution effects (allocation, selection 
#' and total) including total multi-period attribution effects, attribution 
#' effects at each level and security selection
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.geometric}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 215-220
#' @keywords multi-level attribution, geometric attribution
#' @examples
#' 
#' data(attrib)
#' Attribution.levels(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], h = attrib.hierarchy, c("type", "MarketCap", "Sector"))
#' Attribution.levels(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], h = attrib.hierarchy, c("type", "Sector"))
#' 
#' @export
Attribution.levels <-
function(Rp, wp, Rb, wb, h, ...)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to perform the geometric attribution analysis.
    
    # Inputs:
    # Rp       xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # h        data.frame with the hierarchy
  
    # Outputs: 
    # This function returns the list with total attribution effects 
    # (allocation, selection and total) including total multi-period 
    # attribution effects, attribution effects at each level and security
    # selection
  
    # FUNCTION:
    Rp = checkData(Rp)
    Rb = checkData(Rb)
    colnames(Rb) = colnames(Rp)
    WP = wp   # Save original weights in order to avoid double conversion later
    WB = wb
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
      Rp = Rp[2:nrow(Rp)]
      Rb = Rb[2:nrow(Rb)]
    }
    if (ncol(Rb) == 1){
      Rb = matrix(rep(coredata(Rb), ncol(Rp)), nrow(Rp), ncol(Rp))
    }
    if (ncol(Rb) != ncol(Rp)){
      stop("Please use benchmark xts that has columns with benchmarks for each
            asset or one common benchmark for all assets")
    }

    levels <- unlist(list(...))
    if (!is.null(levels)) stopifnot(is.character(levels))
    if (length(levels) == 1){
      stop("Use Attribution function for the single level. This function is for
           the multi-level attribution")
    }

    # Get portfolio and benchmark returns
    if (is.vector(WP)  & is.vector(WB)){
      rp = Return.portfolio(Rp, WP)
      rb = Return.portfolio(Rb, WB)
    } else{
      rp = Return.rebalancing(Rp, WP)
      rb = Return.rebalancing(Rb, WB)
    }
    names(rp) = "Total"
    names(rb) = "Total"
    
    # Geometric excess returns + annualized geometric excess returns
    excess.returns = (1 + rp) / (1 + coredata(rb)) - 1
    if (nrow(rp) > 1){
      er = Return.annualized.excess(rp, rb)
      excess.returns = rbind(as.matrix(excess.returns), er)
    }
    colnames(excess.returns) = "Geometric"
    
    # Transform the hierarchy to the correct form
    for (i in 2:length(levels)){
      if (is.numeric(h[[levels[i]]])){
        h = HierarchyQuintiles(h, levels[i])
      }
      h[[levels[i]]] = paste(h[[levels[i - 1]]],  h[[levels[i]]], sep = "-")
    }
    
    # Get returns and weights at all levels
    returns.p = list()
    weights.p = list()
    returns.b = list()
    weights.b = list()
    bs = list()
    for(i in 1:length(levels)){
      returns.p[[i]] = Return.level(Rp, WP, h, level = levels[i])
      weights.p[[i]] = Weight.level(WP, Rp, h, level = levels[i])
      returns.b[[i]] = Return.level(Rb, WB, h, level = levels[i])
      weights.b[[i]] = Weight.level(WB, Rp, h, level = levels[i])
      # semi-notional funds returns
      bs[[i]] = reclass(rowSums(returns.b[[i]] * weights.p[[i]]), rp)  
    }
    names(returns.p) = levels
    names(weights.p) = levels
    names(returns.b) = levels
    names(weights.b) = levels

    # Total attribution effects
    allocation = matrix(rep(NA, nrow(Rp) * length(levels)), nrow(Rp), 
                        length(levels))
    allocation[, 1] = (1 + bs[[1]]) / coredata(1 + rb) - 1 # Allocation 1
    for (i in 2:length(levels)){
      allocation[, i] = (1 + bs[[i]]) / (1 + bs[[i-1]]) - 1
    }
    selection = (1 + rp) / (1 + last(bs)[[1]]) - 1
    total = (1 + rp) / coredata(1 + rb) - 1 # Total excess return
    
    # Transform portfolio, benchmark returns and semi-notional funds returns to
    # conformable matrices for multi-level attribution
    b = as.xts(matrix(rep(rb, ncol(returns.b[[1]])), nrow(rb), 
                      ncol(returns.b[[1]])), index(rb))
    r = as.xts(matrix(rep(rp, ncol(last(returns.b)[[1]])), nrow(rp), 
                      ncol(last(returns.b)[[1]])), index(rp))
    
    returns.b2 = list()
    for (j in 1:(length(levels) - 1)){ 
      # make benchmark returns conformable at different levels
      r_l = Return.level(Rb, WB, h, level = levels[j])
      r_h = Return.level(Rb, WB, h, level = levels[j + 1])
      hierarchy = split(h[levels[j]], h[levels[j + 1]])
      for (i in 1:ncol(r_h)){
        r_h[, i] = r_l[, hierarchy[[i]][1, 1]]
      }
      returns.b2[[j]] = r_h
    }

    for (i in 1:(length(bs) - 1)){
      bs[[i]] = as.xts(matrix(rep(bs[[i]], ncol(returns.b2[[i]])), nrow(r), 
                              ncol(returns.b2[[i]])), index(r))
    }
    bs[length(bs)] = bs[length(bs) - 1]

    # Attribution at each level
    level = list()
    level[[1]] = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) 
                                                      / (1 + b) - 1)
    for (i in 2:length(levels)){ 
      level[[i]] = (weights.p[[i]] - weights.b[[i]]) * 
                   ((1 + returns.b[[i]]) / (1 + returns.b2[[i-1]]) - 1) * 
                   ((1 + returns.b2[[i-1]]) / (1 + bs[[i-1]]))
    }

    # Security/Asset selection
    select = reclass(weights.p[[length(weights.p)]], rp) * 
            ((1 + r) / (1 + returns.b[[length(returns.b)]]) - 1) * 
            ((1 + returns.b[[length(returns.b)]]) / (1 + bs[[length(bs)]]))
    # Get the multi-period summary
    general = cbind(allocation, selection)
    general = rbind(as.data.frame(general), (apply(1 + general, 2, prod) - 1))
    for (i in 1:length(level)){
      level[[i]] = rbind(as.data.frame(level[[i]]), 
                         (apply(1 + level[[i]], 2, prod) - 1))
      rownames(level[[i]])[nrow(level[[i]])] = "Total"
    }
    select = rbind(as.data.frame(select), (apply(1 + select, 2, prod) - 1))
    rownames(general)[nrow(general)] = "Total"
    rownames(select)[nrow(select)] = "Total"
        
    # Label the output
    result = list()
    labels = paste(rep("Level", length(levels)), 1:length(levels))
    names(level) = labels
    colnames(general) = c(paste(labels, (rep("Allocation", 
                                             length(levels)))), "Selection")
    
    result[[1]] = excess.returns
    result[[2]] = general
    result[[3]] = level
    result[[4]] = select
    names(result) = c("Excess returns", "Multi-level attribution", 
                      "Attribution at each level", "Security selection")
    return(result)
}