# Optimizer Functions

# Copyright 2006 Brian G. Peterson , Aaron van Meerten, Peter Carl


################################################################################
# FUNCTIONS:
#
#
#
#
#
#
#
#
#
#
#
#
#
#
################################################################################

# Check to see if the required libraries are loaded
if(!require("fBasics", quietly=TRUE)) {
    stop("package", sQuote("fBasics"), "is needed.  Stopping")
}
if(!require("fPortfolio", quietly=TRUE)) {
    stop("package", sQuote("fPortfolio"), "is needed.  Stopping")
}

# ------------------------------------------------------------------------------
weight.grid =
function (columnnames, seqstart=.05, seqend=.25, seqstep=.05)
{   # @author Aaron van Meerten

    # Description:

    # Function loosely based on expand.grid
    #
    # Generate and returns a grid of weighting vectors
    # containing all unique combinations
    # of seq parameters where rowsum = 1
    #
    # columnnames = vector of names for the grid columns
    # seqstart = start/from value for seq()
    # seqend = end/to value for seq()
    # seqstep = step/by value for seq()
    #
    # we will use seq(seqstart, seqend, seqstep)
    #

    # FUNCTION:

    result = NA

    # Return Value:
    result
}

# ------------------------------------------------------------------------------
maxdrawdown
function (x)
{ # adapted from package "tseries"
    if (NCOL(x) > 1)
        stop("x is not a vector or univariate time series")
    if (any(is.na(x)))
        stop("NAs in x")
    cmaxx <- cummax(x) - x
    mdd <- max(cmaxx)
    to <- which(mdd == cmaxx)
    from <- double(NROW(to))
    for (i in 1:NROW(to)) from[i] <- max(which(cmaxx[1:to[i]] ==
        0))
    return(list(maxdrawdown = mdd, from = from, to = to))
}

# ------------------------------------------------------------------------------
cut.returns =
function (R, cutrow, startrow=1)
{   # @author Brian G. Peterson

    # Description:

    # FUNCTION:
    result = R[startrow:cutrow,]

    # Return Value:
    result
}



# ------------------------------------------------------------------------------
weighttest =
function (weightgrid, test=1)
{
    rows=nrow(weightgrid)

    result=NA

    for(row in 1:rows) {
        r = as.vector(weightgrid[row,])
        #if (!is.numeric(r)) stop("The selected row is not numeric")

        if (sum(r) == test) {
            #if (result=NA) {
                #create data.frame
            #    result=data.frame(r=r)
            #} else {
                r=data.frame(r=r)
                result=cbind(result,r)
            #}
        }
    } #end rows loop

    # Return Value:
    result

}



# ------------------------------------------------------------------------------
#WeightedReturns =
# use pfolioReturn(returnarray,weightingvector) from fPortfolio
WeightedReturns =
function (R, weightgrid, from, to)
{ # @author Brian G. Peterson

    rows=nrow(weightgrid)

    result=NA

    # data type conditionals
    # cut the return series for from:to
    if (class(R) == "numeric") {
        R = as.matrix(R)
    }
    if (class(R) == "matrix") {
        R = R[from:to,]
    }
    if (class(R) == "data.frame") {
        R = R[from:to, ]
    }
    if (class(R) == "timeSeries") {
        R = R@Data[from:to,]
    }

    if (ncol(weightgrid) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")


    for(row in 1:rows) {

        w = as.vector(weightgrid[row,])
        # test each row in the weighting vectors against the right dates in the return collection

        # pfolioReturn
        returns=pfolioReturn(R,w)

        # annualizedReturn
        annualReturn=annualizedReturn(returns)

        # modifiedVaR(95%) from pfolioReturn
        mVaR = modifiedVaR(returns, p=0.95)

        # maxDrawdown
        mddlist= maxdrawdown(returns)

        # Expected Shortfall
        ES = CVaRplus(returns, weights = NULL, alpha = 0.05)[1]

        # construct a data structure that holds each result for this row
        if (row==1) {
                #create data.frame
                result=data.frame()
                resultrow=data.frame()
        }
        # first cbind
        resultrow = cbind( annualReturn, mVaR, mddlist$maxdrawdown, ES )

        rownames(resultrow) = row

        # then rbind
        result    = rbind(result,resultrow)

    } #end rows loop
    colnames(result)=c("Annualized Return","modifiedVaR(95%)","Max Drawdown","Expected Shortfall")

    # Return Value:
    result

}

# ------------------------------------------------------------------------------
# GeometricReturn
# use annualizedReturn from Peter Carl in performance-analytics.R


