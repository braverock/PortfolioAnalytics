#' performs sector-based single-level attribution
#' 
#' Performs sector-based single-level attribution analysis. Portfolio 
#' performance measured relative to a benchmark gives an indication of the 
#' value-added by the portfolio. Equipped with weights and returns of portfolio
#' segments, we can dissect the value-added into useful components. This 
#' function is based on the sector-based approach to the attribution. The 
#' workhorse is the Brinson model that explains the arithmetic difference 
#' between portfolio and benchmark returns. That is it breaks down the 
#' arithmetic excess returns at one level. If returns and weights are available
#' at the lowest level (e.g. for individual instruments), the aggregation up to
#' the chosen level from the hierarchy can be done using 
#' \code{\link{Return.level}} function. The attribution effects can be computed
#' for several periods. The multi-period summary is obtained using one of 
#' linking methods: Carino, Menchero, GRAP, Frongello or Davies Laker. It also
#' allows to break down the geometric excess returns, which link naturally over
#' time. Finally, it annualizes arithmetic and geometric excess returns 
#' similarly to the portfolio and/or benchmark returns annualization. 
#' 
#' The arithmetic excess returns are decomposed into the sum of allocation, 
#' selection and interaction effects across \eqn{n} sectors:
#' \deqn{R_{p}-R_{b}=\sum^{n}_{i=1}\left(A_{i}+S_{i}+I_{i}\right)}
#' The arithmetic attribution effects for the category i are computed
#' as suggested in the Brinson, Hood and Beebower (1986):
#' Allocation effect
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times R_{bi}}{Ai = (wpi - wbi) * Rbi}
#' Selection effect
#' \deqn{S_{i}=w_{pi}\times(R_{pi}-R_{bi})}{Si = wpi * (Rpi - Rbi)}
#' Interaction effect
#' \deqn{I_{i}=(w_{pi}-w_{bi})
#' \times(R_{pi}-R_{bi})}{Ii = (wpi - wbi) * Rpi - Rbi}
#' \eqn{R_{p}}{Rp} - total portfolio returns,
#' \eqn{R_{b}}{Rb} - total benchmark returns, 
#' \eqn{w_{pi}}{wpi} - weights of the category \eqn{i} in the portfolio, 
#' \eqn{w_{bi}}{wbi} - weights of the category \eqn{i} in the benchmark, 
#' \eqn{R_{pi}}{Rpi} - returns of the portfolio category \eqn{i}, 
#' \eqn{R_{bi}}{Rbi} - returns of the  benchmark category \eqn{i}.
#' If Brinson and Fachler (1985) is selected the allocation effect differs:
#' \deqn{A_{i}=(w_{pi}-w_{bi})
#' \times (R_{bi} - R_{b})}{Ai = (wpi - wbi) * (Rbi - Rb)}
#' Depending on goals we can give priority to the allocation or to 
#' the selection effects. If the priority is given to the sector allocation
#' the interaction term will be combined with the security selection effect
#' (top-down approach). If the priority is given to the security selection,
#' the interaction term will be combined with the asset-allocation effect
#' (bottom-up approach).
#' Usually we have more than one period. In that case individual arithmetic 
#' attribution effects should be adjusted using linking methods. Adjusted
#' arithmetic attribution effects can be summed up over time to provide the
#' multi-period summary: 
#' \deqn{R_{p}-R_{b}=\sum^{T}_{t=1}\left(A_{t}'+S_{t}'+I_{t}'\right)}
#' where \eqn{T} is the number of periods and prime stands for the adjustment.
#' The geometric attribution effects do not suffer from the linking problem.
#' Moreover we don't have the interaction term. For more details about the 
#' geometric attribution see the documentation to 
#' \code{\link{Attribution.geometric}}. Finally, arithmetic annualized excess 
#' returns are computed as the arithmetic difference between annualised 
#' portfolio and benchmark returns:
#' \deqn{AAER=r_{a}-b_{a}}{AAER = ra - ba} the geometric annualized excess
#' returns are computed as the geometric difference between annualized 
#' portfolio and benchmark returns: 
#' \deqn{GAER=\frac{1+r_{a}}{1+b_{a}}-1}{GAER = (1 + ra) / (1 + ba) - 1}
#' In the case of multi-currency portfolio, the currency return, currency
#' surprise and forward premium should be specified. The multi-currency
#' arithmetic attribution is handled following Ankrim and Hensel (1992).
#' Currency returns are decomposed into the sum of the currency surprise and
#' the forward premium: \deqn{R_{ci} = R_{cei} + R_{fpi}}{Rci = Rcei + Rfpi}
#' where 
#' \deqn{R_{cei} = \frac{S_{i}^{t+1} - F_{i}^{t+1}}{S_{i}^{t}}}
#' \deqn{R_{fpi} = \frac{F_{i}^{t+1}}{S_{i}^{t}} - 1}
#' \eqn{S_{i}^{t}}{Sit} - spot rate for asset \eqn{i} at time \eqn{t}
#' \eqn{F_{i}^{t}}{Fit} - forward rate for asset \eqn{i} at time \eqn{t}. 
#' Excess returns are decomposed into the sum of allocation, selection and 
#' interaction effects as in the standard Brinson model: 
#' \deqn{R_{p}-R_{b}=\sum^{n}_{i=1}\left(A_{i}+S_{i}+I_{i}\right)}
#' However the allocation effect is computed taking into account currency
#' effects:
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times (R_{bi} - R_{ci} - R_{l})}{Ai = 
#' (wpi - wbi) * (Rbi - Rci - Rl)}
#' Benchmark returns adjusted to the currency:
#' \deqn{R_{l} = \sum^{n}_{i=1}w_{bi}\times(R_{bi}-R_{ci})}
#' The contribution from the currency is analogous to asset allocation:
#' \deqn{C_{i} = (w_{pi} - w_{bi}) \times (R_{cei} - e) + (w_{pfi} - w_{bfi}) 
#' \times (R_{fi} - e)}
#' where \deqn{e = \sum^{n}_{i=1}w_{bi}\times R_{cei}}
#' The final term, forward premium, is also analogous to the asset allocation:
#' \deqn{R_{fi} = (w_{pi} - w_{bi}) \times (R_{fpi} - d)}{Rfi = (wpi - wbi) * 
#' (Rfpi - d)}
#' where \deqn{d = \sum^{n}_{i=1}w_{bi}\times R_{fpi}}
#' and \eqn{R_{fpi}} - forward premium
#' In general if the intent is to estimate statistical parameters, the 
#' arithmetic excess return is preferred. However, due to the linking 
#' challenges, it may be preferable to use geometric excess return if the 
#' intent is to link and annualize excess returns.
#' 
#' @aliases Attribution
#' @param Rp T x n xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param Rb T x n xts, data frame or matrix of benchmark returns
#' @param wb vector, xts, data frame or matrix of benchmark weights
#' @param method Used to select the priority between allocation and selection 
#' effects in arithmetic attribution. May be any of: \itemize{ \item none - 
#' present allocation, selection and interaction effects independently, 
#' \item top.down - the priority is given to the sector allocation. Interaction
#' term is combined with the security selection effect, \item bottom.up - the 
#' priority is given to the security selection. Interaction term is combined 
#' with the sector allocation effect} 
#' By default "none" is selected
#' @param wpf vector, xts, data frame or matrix with portfolio weights of 
#' currency forward contracts
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @param S (T+1) x n xts, data frame or matrix with spot rates. The first date
#' should coincide with the first date of portfolio returns
#' @param F (T+1) x n xts, data frame or matrix with forward rates. The first
#' date should coincide with the first date of portfolio returns
#' @param Rpl xts, data frame or matrix of portfolio returns in local currency
#' @param Rbl xts, data frame or matrix of benchmark returns in local currency
#' @param Rbh xts, data frame or matrix of benchmark returns hedged into the
#' base currency
#' @param bf TRUE for Brinson and Fachler and FALSE for Brinson, Hood and 
#' Beebower arithmetic attribution. By default Brinson, Hood and Beebower 
#' attribution is selected
#' @param linking Used to select the linking method to present the multi-period
#' summary of arithmetic attribution effects. May be any of: 
#' \itemize{\item carino - logarithmic linking coefficient method
#' \item menchero - Menchero's smoothing algorithm
#' \item grap - linking approach developed by GRAP
#' \item frongello - Frongello's linking method
#' \item davies.laker - Davies and Laker's linking method}
#' By default Carino linking is selected
#' @param geometric TRUE/FALSE, whether to use geometric or arithmetic excess
#' returns for the attribution analysis. By default arithmetic is selected
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period. By default unadjusted attribution effects are 
#' returned
#' @return returns a list with the following components: excess returns with
#' annualized excess returns over all periods, attribution effects (allocation, 
#' selection and interaction)
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.levels}}, 
#' \code{\link{Attribution.geometric}}
#' @references Ankrim, E. and Hensel, C. \emph{Multi-currency performance
#' attribution}. Russell Research Commentary. November 2002 \cr Bacon, C. 
#' \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley.
#' 2004. Chapter 5, 6, 8 \cr Christopherson, Jon A., Carino, David R., Ferson, 
#' Wayne E. \emph{Portfolio Performance Measurement and Benchmarking}. 
#' McGraw-Hill. 2009. Chapter 18-19 \cr Brinson, G. and Fachler, N. (1985)
#' \emph{Measuring non-US equity portfolio performance}. Journal of Portfolio
#' Management. Spring. p. 73 -76. \cr Gary P. Brinson, L. Randolph Hood, and 
#' Gilbert L. Beebower, \emph{Determinants of Portfolio Performance}. Financial
#' Analysts Journal. vol. 42, no. 4, July/August 1986, p. 39-44 \cr 
#' Karnosky, D. and Singer, B. \emph{Global asset management and performance 
#' attribution. The Research Foundation of the Institute of Chartered Financial
#' Analysts}. February 1994. \cr
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Attribution(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], Rb = attrib.returns[, 11:20], 
#' wb = attrib.weights[2, ], method = "top.down", linking = "carino")
#' 
#' @export
Attribution <- 
function (Rp, wp, Rb, wb, 
          wpf = NA, wbf = NA, S = NA, F = NA, Rpl = NA, Rbl = NA, Rbh = NA,
          bf = FALSE,
          method = c("none", "top.down", "bottom.up"), 
          linking = c("carino", 
                      "menchero", 
                      "grap", 
                      "frongello", 
                      "davies.laker"),
          geometric = FALSE, adjusted = FALSE)
{   # @author Andrii Babii

    # DESCRIPTION:
    # Function to perform the attribution analysis.

    # Inputs:
    # Rp       T x n xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       T x n xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
    # wpf      vector, xts, data frame or matrix with portfolio weights of
    #          currency forward contracts
    # wbf      vector, xts, data frame or matrix with benchmark weights of 
    #          currency forward contracts
    # S        (T+1) x n xts, data frame or matrix with spot rates
    # F        (T+1) x n xts, data frame or matrix with forward rates
    # Rpl      xts, data frame or matrix of portfolio returns in local currency
    # Rbl      xts, data frame or matrix of benchmark returns in local currency
    # Rbh      xts, data frame or matrix of benchmark returns hedged into the
    #          base currency
  
    # Outputs: 
    # This function returns the attribution effects with multi-period summary
    # and annualized excess returns
  
    # FUNCTION:
    # Transform data to the xts objects
    Rb = checkData(Rb)
    Rp = checkData(Rp)
    WP = wp # Save original weights in order to avoid double conversion later
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
    method = method[1]
    linking = linking[1]
    
    currency = !(is.null(dim(wpf)) & is.null(dim(wbf)) & 
                   is.null(dim(S)) & is.null(dim(F)) & 
                   is.null(dim(Rpl)) & is.null(dim(Rpl)) & 
                   is.null(dim(Rpl)))
    
    if (geometric == FALSE & linking != "davies.laker"){ 
      # The function makes all computations for the arithmetic attribution
      # case (except for Davies and Laker linking)
    
      # Compute attribution effects (Brinson, Hood and Beebower model)
      # If portfolio is single-currency
      if (!currency){
        Rc = 0
        L = 0
      } else{         # If multi-currency portfolio
        S = checkData(S)
        F = checkData(F)
        wpf = Weight.transform(wpf, Rp)
        wbf = Weight.transform(wbf, Rb)
        
        Rc = lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Rd = lag(F, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Re = Rc - coredata(Rd)
        Rl = Rb - coredata(Rc)
        Rk = Rp - coredata(Rc)
        Rfp = Re / (1 + Rd)
        E = reclass(matrix(rep(rowSums(Re * coredata(wb)), ncol(Rb)), nrow(Rb),
                           ncol(Rb)), Rp)
        L = reclass(matrix(rep(rowSums(Rl * coredata(wb)), ncol(Rb)), nrow(Rb), 
                           ncol(Rb)), Rp)
        D = reclass(matrix(rep(rowSums(Rd * coredata(wb)), ncol(Rb)), nrow(Rb), 
                           ncol(Rb)), Rp)
        # Contribution to currency
        Cc = (wp - wb) * (Re - E) + (wpf - wbf) * (Rfp - E) 
        # Forward premium
        Df = (wp - wb) * (Rd - D) 
        Cc = cbind(Cc, rowSums(Cc))
        Df = cbind(Df, rowSums(Df))
        colnames(Cc) = c(colnames(S), "Total")
        colnames(Df) = colnames(Cc)
      }
      
      # Get total portfolio returns
      if (is.vector(WP)  & is.vector(WB)){
        rp = Return.portfolio(Rp, WP, geometric = FALSE)
        rb = Return.portfolio(Rb, WB, geometric = FALSE)
      } else{
        rp = Return.rebalancing(Rp, WP, geometric = FALSE)
        rb = Return.rebalancing(Rb, WB, geometric = FALSE)
      }
      names(rp) = "Total"
      names(rb) = "Total"
      
      # Get individual attribution effects
      if (bf == TRUE){ # Brinson and Fachler (1985) allocation effect
        allocation = coredata(wp - wb) * (Rb - coredata(Rc) - coredata(L) - 
          rep(rb, ncol(Rb)))
      } else{          # Brinson, Hood and Beebower (1986) allocation effect
        allocation = coredata(wp - wb) * (Rb - coredata(Rc) - coredata(L))
      }

      selection = (Rp  - coredata(Rb)) * wb
      interaction = (wp - wb) * (Rp - coredata(Rb))
      
      # Get total attribution effects 
      n = ncol(allocation)               # number of segments
      allocation = cbind(allocation, rowSums(allocation))
      names(allocation)[n + 1] = "Total"  
      selection = cbind(selection, rowSums(selection))
      names(selection)[n + 1] = "Total"   
      interaction = cbind(interaction, rowSums(interaction))
      names(interaction)[n + 1] = "Total"
      
      # Adjust attribution effects using one of linking methods if there are
      # mutliple periods
      if (nrow(allocation) > 1){
        if (linking == "carino"){
          allocation = Carino(rp, rb, allocation, adjusted)
          selection = Carino(rp, rb, selection, adjusted)
          interaction = Carino(rp, rb, interaction, adjusted)
        }
      
        if (linking == "menchero"){
          allocation = Menchero(rp, rb, allocation, adjusted)
          selection = Menchero(rp, rb, selection, adjusted)
          interaction = Menchero(rp, rb, interaction, adjusted)
        }    
      
        if (linking == "grap"){
          allocation = Grap(rp, rb, allocation, adjusted)
          selection = Grap(rp, rb, selection, adjusted)
          interaction = Grap(rp, rb, interaction, adjusted)
        }
      
        if (linking == "frongello"){
          allocation = Frongello(rp, rb, allocation, adjusted)
          selection = Frongello(rp, rb, selection, adjusted)
          interaction = Frongello(rp, rb, interaction, adjusted)
        }    
      }      
      # Arithmetic excess returns + annualized arithmetic excess returns
      excess.returns = rp - coredata(rb)
      if (nrow(rp) > 1){
        er = Return.annualized.excess(rp, rb, geometric = FALSE)
        excess.returns = rbind(as.matrix(excess.returns), er)
      }
      colnames(excess.returns) = "Arithmetic"
      
      # Select the appropriate result corresponding to the chosen method
      result = list()
      result[[1]] = excess.returns
      result[[2]] = allocation
      result[[3]] = selection
      if (method == "top.down"){     # Top-down attribution
        result[[3]] = result[[3]] + interaction
      }
      if (method == "bottom.up"){    # Bottom-up attribution
        result[[2]] = result[[2]] + interaction
      }
      if (method == "none"){
        result[[4]] = interaction
      }
    } else{ # The function takes output of the corresponding function 
            # (Attribution.geometric or DaviesLaker)
      if (geometric == TRUE){
        attrib = Attribution.geometric(Rp, WP, Rb, WB)
      }
      
      if (linking == "davies.laker"){
        attrib = DaviesLaker(Rp, WP, Rb, WB)
      }
      result = attrib
    }
    
    # Label the output
    if ((method == "none" & geometric == FALSE) | linking == "davies.laker"){
      names(result) = c("Excess returns", "Allocation", "Selection", 
                        "Interaction")
    } else{
      names(result) = c("Excess returns", "Allocation", "Selection")
    }
    
    # If multi-currency portfolio
    if (currency){
      result[[length(result) + 1]] = Cc
      result[[length(result) + 1]] = Df
      names(result)[(length(result)-1):length(result)] = 
        c("Currency management", "Forward Premium")
    }
    return(result)
}
