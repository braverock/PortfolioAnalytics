#' convert information about options, warrants or convertible bonds to the
#' equivalent of returns
#' 
#' The performance of option contracts is measured in esactly the same way as
#' any other asset but they also generate economic exposures. The key to 
#' attribution analysis with options is to inclue the appropriate economic 
#' exposure for which we can use the option's  delta. The option itself will
#' provide part of the valuation. The remaindeer of the option's economic value
#' must be achieved again by using notional assets: \emph{option economic 
#' exposure = Delta x number of options x strike price = option valuation + 
#' notional exposure}
#' @aliases Conv.option
#' @param option \emph{nx8} matrix containing option ID (as rownames), and columns
#' corresponding to (in particular order): strike price, number of options, 
#' current option price, end option price, option's delta, returns on the 
#' underlying
#' @return This function returns the equivalent of returns for options, 
#' warrants or convertible bonds
#' @author Andrii Babii
#' @seealso \code{\link{Attribution}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 237-241
#' @keywords derivatives attribution, option attribution
#' @examples
#' 
#' option = matrix(c(1000, 1000, 1000, 300, 400, 10, 20, 30, 40, 50, 10, 11, 
#' 12, 13, 14, 12, 13, 14, 15, 16, 0.1, 0.2, 0.3, 0.4, 0.5, 0.1, 0.1, 0.2, 
#' 0.2, 0.3), 5, 6)
#' colnames(option) = c("Strike", "Number", "Current option", "End option", 
#' "delta", "returns")
#' rownames(option) = c("CVX", "XOM", "GE", "WMT", "FB")
#' Conv.option(option)
#' 
#' @export
Conv.option <- 
function (option)
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to compute option returns
  
    # Inputs:
    # option   n x 6 matrix containing option IDs (as rownames), and columns
    # corresponding to (in the particular order): strike price, number of 
    # options, current option price, end option price, option's delta, returns
    # on the underlying
  
    # Outputs: 
    # This function returns the equivalent of returns for options, warrants or
    # convertible bonds
  
    # FUNCTION:

    ecvalue = option[, 1] * option[, 2] * option[, 5]
    notionalexp = ecvalue - option[, 2] * option[, 3]
    returns = (notionalexp * option[, 6] + option[, 2] * option[, 4]) / ecvalue
    return(returns)
}