
library(PerformanceAnalytics)
z = rnorm(50)
z = xts( z , order.by = as.Date(c(1:50)))
table.Drawdowns(z)


findDrawdowns = 
function (R, geometric = TRUE, ...) 
{
    x = checkData(R[, 1, drop = FALSE], method = "matrix")
    drawdowns = Drawdowns(x, geometric = geometric)
    draw = c()
    begin = c()
    end = c()
    length = c(0)
    trough = c(0)
    index = 1
    if (drawdowns[1] >= 0) 
        priorSign = 1
    else priorSign = 0
    from = 1
    sofar = drawdowns[1]
    to = 1
    dmin = 1
    for (i in 1:length(drawdowns)) {
        thisSign <- ifelse(drawdowns[i] < 0, 0, 1)
        if (thisSign == as.numeric(priorSign)) {
            #if (drawdowns[i] < sofar) {
            if (drawdowns[i] < as.numeric(sofar)) {
                sofar = drawdowns[i]
                dmin = i
            }
            to = i + 1
        }
        else {
            draw[index] = sofar
            begin[index] = from
            trough[index] = dmin
            end[index] = to
            from = i
            sofar = drawdowns[i]
            to = i + 1
            dmin = i
            index = index + 1
            priorSign = thisSign
        }
    }
    draw[index] = sofar
    begin[index] = from
    trough[index] = dmin
    end[index] = to
    list(return = draw, from = begin, trough = trough, to = end, 
        length = (end - begin + 1), peaktotrough = (trough - 
            begin + 1), recovery = (end - trough))
}