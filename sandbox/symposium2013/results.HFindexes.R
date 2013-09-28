# Presentation of results from optimization scripts run prior to this script

op <- par(no.readonly=TRUE)

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ONLY Equal Weight portfolio in StdDev space
# --------------------------------------------------------------------
# Done
png(filename="RP-EqWgt-MeanSD-ExAnte.png", units="in", height=5.5, width=9, res=96) 
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
# Calculate chart bounds to unify with the charts below
xlim.StdDev=c(min(c(xtract[,"StdDev"], buoys.portfmeas[,"StdDev"])), max(c(xtract[,"StdDev"], buoys.portfmeas[,"StdDev"])))
ylim.mean=c(min(c(xtract[,"mean"], buoys.portfmeas[,"Mean"])), max(c(xtract[,"mean"], buoys.portfmeas[,"Mean"])))

plot(xtract[,"StdDev"],xtract[,"mean"], xlab="Ex Ante Std Dev", ylab="Ex Ante Mean", col="darkgray", axes=FALSE, main="", cex=.7, xlim=xlim.StdDev, ylim=ylim.mean)
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
# Overplot the equal weight portfolio
points(buoys.portfmeas[8,"StdDev"],buoys.portfmeas[8,"Mean"], col=tol8qualitative[8], pch=16, cex=1.5) # watch the order in portfmeas
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright",legend=results.names[8], col=tol8qualitative[8], pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and ASSET portfolios in StdDev space
# --------------------------------------------------------------------
# @TODO: add the assets to this chart
png(filename="RP-Assets-MeanSD-ExAnte.png", units="in", height=5.5, width=9, res=96) 
xlim.StdDev.assets =c(min(c(xtract[,"StdDev"], assets.portfmeas[,"StdDev"], 0)), max(c(xtract[,"StdDev"], assets.portfmeas[,"StdDev"],0.03)))
ylim.mean.assets =c(min(c(xtract[,"mean"], assets.portfmeas[,"Mean"], 0)), max(c(xtract[,"mean"], assets.portfmeas[,"Mean"])))
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
# Revise the chart bounds to include the asssets
plot(xtract[,"StdDev"],xtract[,"mean"], xlab="Ex Ante mETL", ylab="Ex Ante Mean", col="darkgray", axes=FALSE, main="", cex=.7, xlim=xlim.StdDev.assets, ylim=ylim.mean.assets)
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
abline(v = 0, col = "darkgray")
# Overplot the equal weight portfolio
points(buoys.portfmeas[8,"StdDev"],buoys.portfmeas[8,"Mean"], col=tol8qualitative[8], pch=16, cex=1.5) # watch the order in portfmeas
text(x=buoys.portfmeas[8,"StdDev"], y=buoys.portfmeas[8,"Mean"], labels=rownames(buoys.portfmeas)[8], pos=4, cex=1)
points(assets.portfmeas[,"StdDev"],assets.portfmeas[,"Mean"], col=rich8equal, pch=18, cex=1.5) # watch the order in portfmeas
text(x=assets.portfmeas[,"StdDev"], y=assets.portfmeas[,"Mean"], labels=rownames(assets.portfmeas), pos=4, cex=1)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
#legend("right",legend=rownames(assets.portfmeas), col=rich8equal, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and BUOY portfolios in StdDev space
# --------------------------------------------------------------------
# Done
png(filename="RP-BUOY-MeanSD-ExAnte.png", units="in", height=5.5, width=9, res=96) 
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
plot(xtract[,"StdDev"],xtract[,"mean"], xlab="Ex Ante Std Dev", ylab="Ex Ante Mean", col="darkgray", axes=FALSE, main="", cex=.7, xlim=xlim.StdDev, ylim=ylim.mean)
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
# Overplot the buoy portfolios
points(buoys.portfmeas[,"StdDev"],buoys.portfmeas[,"Mean"], col=tol8qualitative, pch=16, cex=1.5) # watch the order in portfmeas
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright",legend=results.names, col=tol8qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot Ex Ante scatter of RP and BUOY portfolios in mETL space
# --------------------------------------------------------------------
# Done
png(filename="RP-BUOYS-mETL-ExAnte.png", units="in", height=5.5, width=9, res=96) 
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
xlim.ES=c(min(c(xtract[,"ES"], buoys.portfmeas[,"mETL"])), max(c(xtract[,"ES"], buoys.portfmeas[,"mETL"])))
plot(xtract[,"ES"],xtract[,"mean"], xlab="Ex Ante mETL", ylab="Ex Ante Mean", col="darkgray", axes=FALSE, main="", cex=.7, xlim=xlim.ES, ylim=ylim.mean)
grid(col = "darkgray")
# Overplot the buoy portfolios
points(buoys.portfmeas[,"mETL"],buoys.portfmeas[,"Mean"], col=tol8qualitative, pch=16, cex=1.5) # watch the order in portfmeas
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("bottomright",legend=results.names, col=tol8qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=0.8, inset=.02)
par(op)
dev.off()

# --------------------------------------------------------------------
# Plot weights of Buoy portfolios
# --------------------------------------------------------------------
# Done
source('./R/chart.UnStackedBar.R')
# Wgts = extractWeights(buoys)
png(filename=paste(resultsdir, "Weights-Buoys.png", sep=""), units="in", height=5.5, width=9, res=96)
chart.UnStackedBar(t(Wgts), colorset=tol8qualitative, equal.line=TRUE)
dev.off()

# --------------------------------------------------------------------
# Plot contribution to risk of Buoy portfolios
# --------------------------------------------------------------------
# @TODO: revise for this result set
# @TODO: add contribution to risk to portfmeas
source('./R/chart.UnStackedBar.R')
png(filename=paste(resultsdir, "mETL-Perc-Contrib-Buoys.png", sep=""), units="in", height=5.5, width=9, res=96)
chart.UnStackedBar(t(buoys.perc.es), colorset=tol8qualitative, equal.line=TRUE)
dev.off()
# Alternatively, use table function for ES

# --------------------------------------------------------------------
# Plot cumulative contribution to risk of Buoy portfolios
# --------------------------------------------------------------------
cumRisk=NULL
for(i in 1:NROW(buoys.contrib.es)) {
  y = cumsum(buoys.contrib.es[i,order(buoys.contrib.es[i,], decreasing=TRUE)])
  cumRisk=rbind(cumRisk,y)
}
colnames(cumRisk)=c("Most",2:6,"Least")
rownames(cumRisk)= results.names

png(filename=paste(resultsdir, "mETL-CumulPerc-Contrib-Buoys.png", sep=""), units="in", height=5.5, width=9, res=96)
par(mar=c(5, 4, 1, 4) + 0.1) #c(bottom, left, top, right)
plot(cumRisk[8,], ylim=c(0,max(cumRisk)), col=tol8qualitative[8], type="l", lwd=2, axes=FALSE, main="", xlab="Rank of Contribution to Risk", ylab="Portfolio Risk")
grid(col = "darkgray")
abline(h = 0, col = "darkgray")
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
for(i in 1:8) {
  lines(cumRisk[i,], col=tol8qualitative[i], lwd=3)
  # put the values of the rightmost dot on the plot; that's the portfolio risk 
  points(7, cumRisk[i,7], col = tol8qualitative[i], pch=20, cex=1)
	mtext(paste(round(100*cumRisk[i,7],2),"%", sep=""), line=.5, side = 4, at=cumRisk[i,7], adj=0, las=2, cex = 0.9, col = tol8qualitative[i])
}
# Add legend
legend("bottomright",legend=results.names, col=tol8qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, cex=.9, lwd=3, inset=.02)
par(op)
dev.off()


# --------------------------------------------------------------------
# Plot contribution of risk in EqWgt portfolio
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Plot efficient frontier of mean-sd?
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Plot efficient frontier of mean-mETL?
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Plot efficient frontier of Equal Risk
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Plot Ex Post scatter of buoy portfolios?
# --------------------------------------------------------------------
# @TODO: revise for this result set

# Calculate ex post results
xpost.ret=Return.cumulative(BHportfs["2008-07::2008-09"])
xpost.sd=StdDev(BHportfs["2008-07::2008-09"])*sqrt(3)
xante.ret=xtract[,"pamean.pamean"]/3
xante.sd=xtract[,"pasd.pasd"]/sqrt(3)

xpost.obj=NA
for(i in 1:NROW(RND.weights)){
  x = Return.portfolio(R=edhec.R["2008-07::2008-09"], weights=RND.weights[i,])
  y=c(Return.cumulative(x), StdDev(x)*sqrt(3))
  if(is.na(xpost.obj))
    xpost.obj=y
  else
    xpost.obj=rbind(xpost.obj,y)
}
rownames(xpost.obj)=rownames(RND.weights)
colnames(xpost.obj)=c("Realized Returns","Realized SD")
xmin=min(c(xpost.sd,xante.sd))
xmax=max(c(xpost.sd,xante.sd))
ymin=min(c(xpost.ret,xante.ret))
ymax=max(c(xpost.ret,xante.ret))

png(filename="Scatter-ExPost-2008-06-30.png", units="in", height=5.5, width=9, res=96)
par(mar=c(5, 4, 1, 2) + 0.1) #c(bottom, left, top, right)
plot(xpost.sd,xpost.ret, xlab="StdDev", ylab="Mean", col="darkgray", axes=FALSE, main="", cex=.7,  xlim=c(xmin,xmax), ylim=c(ymin,ymax))
grid(col = "darkgray")
points(xpost.obj[,2],xpost.obj[,1], col=tol7qualitative, pch=16, cex=1.5)
points(xante.sd,xante.ret, col="lightgray", cex=.7)
points(unlist(RND.objectives[,2])/sqrt(3),unlist(RND.objectives[,1])/3, col=tol7qualitative, pch=16, cex=1.5)
abline(h = 0, col = "darkgray")
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")
legend("topright",legend=rownames(RND.weights), col=tol7qualitative, pch=16, ncol=1,  border.col="darkgray", y.intersp=1.2, inset=.02)
dev.off()



# --------------------------------------------------------------------
# Ex Post Results Through Time?
# --------------------------------------------------------------------
# @TODO: revise for this result set
buoys.R=cbind(EqWgt,MeanSD, MeanmETL,MinSD,MinmETL,EqSD,EqmETL)
png(filename="Buoy-Cumulative-Returns.png", units="in", height=5.5, width=9, res=96) 
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) # c(bottom, left, top, right)
chart.CumReturns(buoys.R["2000::",], main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= tol7qualitative, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(buoys.R["2000::",], main = "", ylab = "Drawdown", colorset = tol7qualitative, cex.axis=.6, cex.lab=.7)
par(op)
dev.off()


### APPENDIX SLIDES:

# --------------------------------------------------------------------
# Show turnover of the RP portfolios relative to the EqWgt portfolio
# --------------------------------------------------------------------
turnover = function(w1,w2) {sum(abs(w1-w2))/length(w1)}
# Calculate the turnover matrix for the random portfolio set:
to.matrix<-matrix(nrow=NROW(rp),ncol=NROW(rp))
for(x in 1:NROW(rp)){
  for(y in 1:NROW(rp)) {
    to.matrix[x,y]<-turnover(rp[x,],rp[y,])
  }
}

png(filename="Turnover-2008-06-30.png", units="in", height=5.5, width=9, res=96)
# postscript(file="TurnoverOf20101231.eps", height=6, width=5, paper="special", horizontal=FALSE, onefile=FALSE)
op <- par(no.readonly=TRUE)
layout(matrix(c(1,2)),height=c(4,1.25),width=1)
par(mar=c(4,4,1,2)+.1, cex=1) # c(bottom, left, top, right)
seq.col = heat.colors(11)
## Draw the Scatter chart of combined results
### Get the random portfolios from one of the result sets
x=apply(rp, MARGIN=1,FUN=turnover,w2=rp[1,])
plot(xtract[,"pasd.pasd"],xtract[,"pamean.pamean"], xlab="Predicted StdDev", ylab="Predicted Mean", col=seq.col[ceiling(x*100)], axes=FALSE, main="", cex=.7, pch=16)
grid(col = "darkgray")
points(RND.objectives[1,2],RND.objectives[1,1], col="blue", pch=19, cex=1.5)
axis(1, cex.axis = 0.8, col = "darkgray")
axis(2, cex.axis = 0.8, col = "darkgray")
box(col = "darkgray")

# Add legend to bottom panel
par(mar=c(5,5.5,1,3)+.1, cex=0.7)
## Create a histogramed legend for sequential colorsets
## this next bit of code is based on heatmap.2 in gplots package
x=ceiling(x*100)
scale01 <- function(x, low = min(x), high = max(x)) {
  return((x - low)/(high - low))
}
breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = length(seq.col)+1)
min.raw <- min(x, na.rm = TRUE)
max.raw <- max(x, na.rm = TRUE)
z <- seq(min.raw, max.raw, length = length(seq.col))
image(z = matrix(z, ncol = 1), col = seq.col, breaks = breaks, xaxt = "n", yaxt = "n")
par(usr = c(0, 1, 0, 1)) # needed to draw the histogram correctly
lv <- pretty(breaks)
xv <- scale01(as.numeric(lv), min.raw, max.raw)
axis(1, at = xv, labels=sprintf("%s%%", pretty(lv)))
h <- hist(x, plot = FALSE, breaks=breaks)
hx <- scale01(breaks, min(x), max(x))
hy <- c(h$counts, h$counts[length(h$counts)])
lines(hx, hy/max(hy)*.95, lwd = 2, type = "s", col = "blue")
axis(2, at = pretty(hy)/max(hy)*.95, pretty(hy))
title(ylab="Count")
title(xlab="Degree of Turnover from Equal Weight Portfolio")
par(op)
dev.off()

# --------------------------------------------------------------------
# Show CONCENTRATION of the RP portfolios?
# --------------------------------------------------------------------
# Basically the same chart as above but use HHI instead of turnover calc