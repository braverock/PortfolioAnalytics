# periodApplyEZ.r
#
# purpose: Modified version of period.apply() to work with logLinking.zoo()
#
## authors: Eric Zivot
## created: August 3, 2011
## modified: August 3, 2011


#' Apply Function Over Specified Interval
#' 
#' Modified version of period.apply() from package xts to work with
#' logLinking.zoo
#' 
#' Similar to the rest of the apply family, calculate a specified functions
#' value given a shifting set of data values. The primary difference is that it
#' is that \code{period.apply.EZ} applies a function to non-overlapping
#' intervals along a vector. This is a modified of the function
#' \code{period.apply} in package xts. The modification allows the output to be
#' either a vector with length of \code{INDEX} minus 1 or a matrix with number
#' of rows length of \code{INDEX} minus 1. Useful for applying arbitrary
#' functions over an entire data object by an aribirtary index, as when
#' \code{INDEX} is the result of a call to \code{endpoints}.
#' 
#' 
#' @param x data to apply \code{FUN} to
#' @param INDEX numeric vector specifying indexing
#' @param FUN an argument of type \code{function}
#' @param \dots additional arguments for \code{FUN}
#' @return A vector with length of INDEX minus 1 or a matrix with number of
#' rows length of INDEX minus 1.
#' @author Original code by Jeff Ryan. Modified by Eric Zivot.
period.apply.EZ <- function (x, INDEX, FUN, ...)
{
# TODO Brian should merge/move this into xts/quantmod.
# Agruments
# x       data to apply FUN to
# INDEX   numeric vector specifying indexing
# FUN     an argument of type function
# ...     additional arguments for FUN
# Details
# Similar to the rest of the apply family, calculate a specified functions value
# given a shifting set of data values. The primary difference is that it is that
# period.apply applies a function to non-overlapping intervals along a vector.
# This is a modified of the function period.apply in package xts. The modification
# allows the output to be either a vector with length of INDEX minus 1 or a matrix
# with number of rows length of INDEX minus 1.
#
# Useful for applying arbitrary functions over an entire data object by an
# aribirtary index, as when INDEX is the result of a call to endpoints.
#
# Value
# A vector with length of INDEX minus 1 or a matrix with number of rows length
# of INDEX minus 1.
    require(xts)
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
        FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    ## ghetto rigging to work with logLinking.zoo(). Allows output to be either
    ## a vector or a matrix. period.apply() only gives vector result
    if (is.matrix(xx)) {
      reclass(t(xx), x[INDEX])
    } else {
      reclass(xx, x[INDEX])
    }
}
