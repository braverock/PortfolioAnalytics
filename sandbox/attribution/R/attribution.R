#' performs arithmetic attribution
#' 
#' Performance attribution analysis. Portfolio performance measured 
#' relative to a benchmark gives an indication of the value-added by the 
#' portfolio. Equipped with weights and returns of portfolio segments, we 
#' can dissect the value-added into useful components. This function is based
#' on the sector-based approach to the attribution. The workhorse is the
#' Brinson model that explains the arithmetic difference between portfolio and
#' benchmark returns. That is it breaks down the arithmetic excess returns at 
#' one level. If returns and weigths are available at the lowest level (e.g. 
#' for individual instruments), the aggregation up to the chosen level from the
#' hierarchy can be done using Return.level function. The attribution effects 
#' can be computed for several periods. The multi-period summary is obtained 
#' using one of linking methods: Carino, Menchero, GRAP, Frongello. It also 
#' allows to break down the geometric excess returns, which link naturally 
#' over time. Finally, it annualizes arithmetic and geometric excess returns 
#' similarly to the portfolio and/or benchmark returns annualization. 
#' 
#' The arithmetic exess returns are decomposed into the sum of allocation, 
#' selection and interaction effects across \deqn{n} sectors:
#' \deqn{r-b=\overset{n}{\underset{i=1}{\sum}}\left(A_{i}+S_{i}+I_{i}\right)}
#' The arithmetic attribtion effects for the category \deqn{i} are computed
#' as suggested in the Brinson, Hood and Beebower (1986):
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times R_{bi}} - allocation effect
#' \deqn{S_{i}=w_{pi}\times(R_{pi}-R_{bi})} - selection effect
#' \deqn{I_{i}=(w_{pi}-w_{bi})\times(r_{i}-b_{i})} - interaction effect
#' \deqn{r} - total portfolio returns
#' \deqn{b} - total benchmark returns
#' \deqn{w_{pi}} - weights of the category \deqn{i} in the portfolio
#' \deqn{w_{bi}} - weigths of the category \deqn{i} in the benchmark
#' \deqn{R_{pi}} - returns of the portfolio category \deqn{i}
#' \deqn{R_{bi}} - returns of the benchmark category \deqn{i}
#' Depending on goals we can give priority to the allocation or to 
#' the selection effects. If the priority is given to the sector allocation
#' the interaction term will be combined with the security selection effect
#' (top-down approach). If the priority is given to the security selection,
#' the interaction term will be combined with the asset-allocation effect
#' (bottom-up approach).
#' Usually we have more then one period. In that case individual arithmetic 
#' attribution effects should be adjusted using linking methods. Adjusted
#' arithmetic attribution effects can be summed up over time to provide the
#' multi-period summary: 
#' \deqn{r-b=\overset{T}{\underset{t=1}{\sum}}\left(A_{t}'+S_{t}'+I_{t}'\right)}
#' , where \deqn{T} - number of periods; prime stands for the adjustment.
#' The geometric attribution effects do not suffer from the linking problem.
#' Moreover we don't have the interaction term. For more details about the 
#' geometric attribution see the documentation to 
#' \code{link{Attribution.geometric}}
#' Finally, arithmetic annualized excess returns are computed as the 
#' arithmetic difference between annualised portfolio and benchmark returns:
#' \deqn{AAER=r_{a}-b_{a}}; the geometric annualized excess returns are
#' computed as the geometric difference between annualized portfolio
#' and benchmark returns: \deqn{GAER=\frac{1+r_{a}}{1+b_{a}}-1}
#' In the case of multi-currency portfolio, the currency return, currency
#' surprise and forward premium should be specified. The multi-currency
#' arithmetic attribution is handled following Ankrim and Hensel (1992).
#' Currency returns are decomposed into the sum of the currency surprise and
#' the forward premium: \deqn{R_{ci} = R_{cei} + R_{fpi}}, where 
#' \deqn{R_{cei} = \frac{S_{i}^{t+1} - F_{i}^{t+1}}{S_{i}^{t}}
#' \deqn{R_{fpi} = \frac{F_{i}^{t+1}}{S_{i}^{t}} - 1}
#' \deqn{S_{i}^{t}} - stop rate for asset i at time t
#' \deqn{F_{i}^{t}} - forward rate for asset i at time t
#' Excess returns are decomposed into the sum of allocation, selection and 
#' interaction effects as in the standard Brinson model: 
#' \deqn{r-b=\overset{n}{\underset{i=1}{\sum}}\left(A_{i}+S_{i}+I_{i}\right)}
#' However the allocation effect is computed taking into account currency
#' effects:
#' \deqn{A_{i}=(w_{pi}-w_{bi})\times (R_{bi} - R_{ci} - R_{l})} - allocation
#' \deqn{R_{l} = \overset{n}{\underset{i=1}{\sum}}w_{bi}\times(R_{bi}-R_{ci})} - 
#' benchmark return adjusted for currecy.
#' The contribution from currency is analogous to asset allocation:
#' \deqn{C_{i} = (w_{pi} - w_{bi}) \times (R_{cei} - e) + (w_{pfi} - w_{bfi}) \times (R_{fi} - e)}
#' where \deqn{e = \overset{n}{\underset{i=1}{\sum}}w_{bi}\times R_{cei}}
#' The final term, forward premium, is also analogous to the asset allocation:
#' \deqn{R_{fi} = (w_{pi} - w_{bi}) \times (R_{fpi} - d)}
#' where \deqn{d = \overset{n}{\underset{i=1}{\sum}}w_{bi}\times R_{fpi}}
#' \deqn{R_{fpi}} - forward premium
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
#' priority is given to the security selection. Interection term is combined 
#' with the sector allocation effect}
#' @param wpf vector, xts, data frame or matrix with portfolio weights of 
#' currency forward contracts
#' @param wbf vector, xts, data frame or matrix with benchmark weights of 
#' currency forward contracts
#' @param S (T+1) x n xts, data.frame or matrix with spot rates. The first date
#' should coincide with the first date of portfolio returns
#' @param F (T+1) x n xts, data.frame or matrix with forward rates. The first
#' date should coincide with the first date of portfolio returns
#' @param linking Used to select the linking method to present the multi-period 
#' summary of arithmetic attribution effects. It is also used to select the 
#' geometric attribution. May be any of: \itemize{ \item carino - logarithmic 
#' linking coefficient method, \item menchero - Menchero's smoothing algorithm, 
#' \item grap - linking approach developed by GRAP, \item frongello - 
#' Frongello's linking method
#' @param geometric TRUE/FALSE, whether to use geometric or arithmetic excess
#' returns for the attribution analysis
#' @param adjusted TRUE/FALSE, whether to show original or smoothed attribution
#' effects for each period
#' @return returns a list with the following components: excess returns with
#' annualized excess returns over all periods, attribution effects (allocation, 
#' selection and interaction)
#' @author Andrii Babii
#' @seealso \code{\link{Attribution.levels}}
#' @references Ankrim, E. and Hensel, C. \emph{Multi-currency performance
#' attribution}.Russell Research Commentary.November 2002
#' 
#' Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. Chapter 5, 8
#' 
#' Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 2009. 
#' Chapter 18-19
#' 
#' Gary P. Brinson, L. Randolph Hood, and Gilbert L. Beebower, \emph{Determinants of 
#' Portfolio Performance}, Financial Analysts Journal, vol. 42, no. 4, July/August 
#' 1986, pp. 39–44.
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
#' 
#' @TODO fix bug with annualized excess returns, Brinson-Fachler, check if we can compute
#' total effects for individual segments in Davies-Laker and Geometric
#' @export
Attribution <- 
function (Rp, wp, Rb, wb, wpf = "none", wbf = "none", S = "none", F = "none", 
          method = c("none", "top.down", "bottom.up"), 
          linking = c("carino", "menchero", "grap", "frongello", "davies.laker"),
          geometric = FALSE, adjusted = FALSE)
{   # @author Andrii Babii

    # DESCRIPTION:
    # Function to perform the attribution analysis.

    # Inputs:
    # Rp       T x n xts, data frame or matrix of portfolio returns
    # wp       vector, xts, data frame or matrix of portfolio weights
    # Rb       T x n xts, data frame or matrix of benchmark returns
    # wb       vector, xts, data frame or matrix of benchmark weights
  
    # Outputs: 
    # This function returns the attribution effects with multi-period summary
    # and annualized excess returns
  
    # FUNCTION:
    # Transform data to the xts objects
    Rb = checkData(Rb)
    Rp = checkData(Rp)
    wp = Weight.transform(wp, Rp)
    wb = Weight.transform(wb, Rb)
    if (nrow(wp) < nrow(Rp)){ # Rebalancing occurs next day
      Rp = Rp[2:nrow(Rp)]
      Rb = Rb[2:nrow(Rb)]
    }
    
    # Compute attribution effects (Brinson, Hood and Beebower model)
    if (wpf == "none" & wbf == "none" & S == "none" & F =="none"){ 
             # If portfolio is single-currency
        Rc = 0
        L = 0
    } else{  # If multi-currency portfolio
        S = checkData(S)
        F = checkData(F)
        wpf = Weight.transform(wpf, Rp)
        wbf = Weight.transform(wbf, Rb)
        
        Rc = lag(S, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Rd = lag(F, -1)[1:nrow(Rp), ] / S[1:nrow(Rp), ] - 1
        Re = Rc - Rd
        Rl = Rb - Rc
        Rk = Rp - Rc
        Rfp = Re / (1 + Rd)
        E = reclass(matrix(rep(rowSums(wb * Re), ncol(Rb)), nrow(Rb), ncol(Rb)), Rp)
        L = reclass(matrix(rep(rowSums(wb * Rl), ncol(Rb)), nrow(Rb), ncol(Rb)), Rp)
        D = reclass(matrix(rep(rowSums(wb * Rd), ncol(Rb)), nrow(Rb), ncol(Rb)), Rp)
        Cc = (wp - wb) * (Re - E) + (wpf - wbf) * (Rfp - E) # Contribution to currency
        Df = (wp - wb) * (Rd - D) # Forward premium
        Cc = cbind(Cc, rowSums(Cc))
        Df = cbind(Df, rowSums(Df))
        colnames(Cc) = c(colnames(S), "Total")
        colnames(Df) = colnames(Cc)
    }
    allocation = (wp - wb) * (Rb - Rc - L)
    selection = wb * (Rp  - Rb)
    interaction = (wp - wb) * (Rp - Rb)

    # Get total attribution effects 
    n = ncol(allocation)               # number of segments
    allocation = cbind(allocation, rowSums(allocation))
    names(allocation)[n + 1] = "Total"  
    selection = cbind(selection, rowSums(selection))
    names(selection)[n + 1] = "Total"   
    interaction = cbind(interaction, rowSums(interaction))
    names(interaction)[n + 1] = "Total"
                                         
    # Get total portfolio returns          
    rp = reclass(rowSums(Rp * wp), Rp)  
    rb = reclass(rowSums(Rb * wb), Rb)
    names(rp) = "Total"                    
    names(rb) = "Total"                 
       
    # Adjust attribution effects using one of linking methods
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
    
    if (geometric == TRUE){
        attrib = Attribution.geometric(Rp, wp, Rb, wb)
    }
    
    if (linking == "davies.laker"){
        attrib = DaviesLaker(Rp, wp, Rb, wb)
    }
    
    # Total arithmetic excess returns
    rp.c = prod(1 + rp) - 1              
    rb.c = prod(1 + rb) - 1
    if (geometric == FALSE | linking != "davies.laker"){
        excess.returns = rp - rb
        aer = as.matrix(rp.c - rb.c)
        rownames(aer) = "Total arithmetic"
        excess.returns = rbind(as.matrix(excess.returns), aer)
    }

    # Select the appropriate result corresponding to the chosen method
    if (geometric == FALSE & linking != "davies.laker"){
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
    } else{
      result = attrib
    }
    
    # Label the output
    if ((linking == "none" & geometric == FALSE) | linking == "davies.laker"){
        names(result) = c("Excess returns", "Allocation", "Selection", "Interaction")
    } else{
        names(result) = c("Excess returns", "Allocation", "Selection")
    }
    
    # If multi-currency portfolio
    if (!(wpf == "none" & wbf == "none" & S == "none" & F =="none")){
        result[[length(result) + 1]] = Cc
        result[[length(result) + 1]] = Df
        names(result)[(length(result)-1):length(result)] = c("Contribution from currency", "Forward Premium")
    }
    return(result)
}