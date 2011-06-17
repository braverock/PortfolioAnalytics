chart.StackedBar2 =
function (w, colorset = NULL, space = 0.2, cex.legend = 0.8, 
    cex.names = 1, cex.axis = 1, las = 3, legend.loc = "under", 
    element.color = "black", unstacked = TRUE, xlab = NULL, 
    ylim = NULL, axisnames = TRUE , l=1.9, r=1, u = 4, ...) 
{
    w = checkData(w, method = "matrix")
    w.columns = ncol(w)
    w.rows = nrow(w)
    if (is.null(colorset)) 
        colorset = 1:w.columns
    if (is.null(xlab)) 
        minmargin = 3
    else minmargin = 5
    if (unstacked & dim(w)[1] == 1) {
        if (las > 1) {
            bottommargin = max(c(minmargin, (strwidth(colnames(w), 
                units = "in"))/par("cin")[1])) * cex.names
            par(mar = c(bottommargin, l, u, r) + 0.1)
        }
        barplot(w, col = colorset[1], las = las, horiz = FALSE, 
            space = space, xlab = xlab, cex.names = cex.names, 
            axes = FALSE, ylim = ylim, ...)
        axis(2, col = element.color, las = las)
    }
    else {
        op <- par(no.readonly = TRUE)
        if (!is.null(legend.loc)) {
            if (legend.loc == "under") {
                layout(rbind(1, 2), height = c(6, 1), width = 1)
            }
            else par(mar = c(5, l, u, r) + 0.1)
        }
        if (las > 1) {
            bottommargin = max(c(minmargin, (strwidth(rownames(w), 
                units = "in"))/par("cin")[1])) * cex.names
            par(mar = c(bottommargin, l, 4, r) + 0.1)
        }
        else {
            if (is.null(xlab)) 
                bottommargin = 3
            else bottommargin = 5
            par(mar = c(bottommargin, l, u, r) + 0.1)
        }
        positives = w
        for (column in 1:ncol(w)) {
            for (row in 1:nrow(w)) {
                positives[row, column] = max(0, w[row, column])
            }
        }
        negatives = w
        for (column in 1:ncol(w)) {
            for (row in 1:nrow(w)) {
                negatives[row, column] = min(0, w[row, column])
            }
        }
        if (is.null(ylim)) {
            ymax = max(0, apply(positives, FUN = sum, MARGIN = 1))
            ymin = min(0, apply(negatives, FUN = sum, MARGIN = 1))
            ylim = c(ymin, ymax)
            ylim =  c( 0.2*floor(10*(ylim[1]+0.1)/2) , 0.2*ceiling(10*(ylim[2]-0.1)/2) )
        }
        # extend ylim to have extend labels on y-axis

        barplot(t(positives), col = colorset, space = space, 
            axisnames = axisnames, axes = FALSE, ylim = ylim, ...)
        barplot(t(negatives), add = TRUE, col = colorset, space = space, 
            las = las, xlab = xlab, cex.names = cex.names, axes = FALSE, 
            ylim = ylim, axisnames = FALSE, ...)
        axis(2, col = element.color, las = las, cex.axis = cex.axis, 
               at=seq( ylim[1], ylim[2] , 0.2 ) , labels = seq( ylim[1], ylim[2] , 0.2 ) )
        
        if (!is.null(legend.loc)) {
            if (legend.loc == "under") {
                par(mar = c(0, 2, 0, 1) + 0.1)
                plot.new()
                if (w.columns < 4) 
                  ncol = w.columns
                else ncol = 4
                legend("center", legend = colnames(w), cex = cex.legend, 
                  fill = colorset, ncol = 3, box.col = element.color, 
                  border.col = element.color)
            }
        }
        #par(op)
    }
}