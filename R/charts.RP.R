chart.Weights.RP <- function(RP, neighbors = NA, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    columnnames = names(RP$weights)
    numassets = length(columnnames)

    if(!is.na(neighbors)){
        xtract=extractStats.rp(RP)
        orderx = order(xtract[,"out.mean"])
        subsetx = head(xtract[orderx,2:(numassets+1)], n=neighbors)
    }
    par( mar=c(5,4,4,2)) # set margin to accomodate vertical names
    plot(RP$random_portfolios[1,], type="b", col="orange", axes=FALSE, xlab="", ylim=c(0,max(RP$constraints$max)), ...)
    if(!is.na(neighbors)){
        for(i in 1:neighbors) points(subsetx[i,], type="b", col="lightblue")
        points(RP$random_portfolios[1,], type="b", col="orange", pch=16) # to overprint neighbors
    }

    points(RP$weights, type="b", col="red", pch=16)
    points(RP$constraints$min, type="b", col="darkgray", lty="dashed", pch=24)
    points(RP$constraints$max, type="b", col="darkgray", lty="dashed", pch=25)
    box()
    axis(2)
    axis(1, labels=names(RP$weights), at=1:numassets, las=3)
}

chart.Scatter.RP <- function(RP, neighbors = NA, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    xtract = extractStats.rp(RP)
    columnnames = colnames(xtract)
    return.column = grep("objective_measures.mean",columnnames)
    ## @TODO: Generalize this to find column containing the "risk" metric
    risk.column = grep("objective_measures.MES",columnnames)

    plot(xtract[,risk.column],xtract[,return.column], xlab=colnames(xtract)[risk.column], ylab=columnnames[return.column], col="lightgray", ...)

    if(!is.na(neighbors)){ # overplot nearby portfolios
        orderx = order(xtract[,"out.mean"])
        subsetx = head(xtract[orderx,], n=neighbors)
        points(subsetx[,risk.column], subsetx[,return.column], col="lightblue", pch=1)
    }

    points(xtract[1,risk.column],xtract[1,return.column], col="orange", pch=16) # overplot the equal weighted (or seed)
    ## @TODO: Generalize this to find column containing the "risk" metric
    points(RP$constrained_objective$MES, RP$constrained_objective$mean, col="red", pch=16) # optimal
}

charts.RP <- function(RP, neighbors = NA, ...){
# Specific to the output of the random portfolio code with constraints
    # @TODO: check that RP is of the correct class
    layout(matrix(c(1,2)),height=c(2,1),width=1)
    chart.Scatter.RP(RP, neighbors, ...)
    chart.Weights.RP(RP, neighbors, ...)
}
