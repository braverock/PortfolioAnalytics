### For Presentation at FactSet's 2013 US Investment Process Symposium
# November 10 - 12 , 2013
# Peter Carl

### Description
# This script will generate a series of plots and csv data in the resultsdir
# for possible inclusion in slides.

### Make needed changes to workspace here ###
#
datadir = "./data/"
resultsdir = "./results/"
functionsdir = "./R/"

### Load the necessary packages
require(vcd) # for color palates
require(corrplot) # for correlation charts

# This may be useful for PCA analysis of index data
# require(FactorAnalytics) # development version > build 

### Set up color palates
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
    axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# Qualitative color scheme by Paul Tol
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")

# Constants
p=1-(1/12) # 
Rf=.03/12 # Monthly risk free rate
colorset = rich8equal
dataname="EDHEC"

########################################################################
# Load data
########################################################################
# Load the data objects from downloaded and parsed files
## See 'parse.EDHEC.R' and 'download.SP500TR.R'

# Load data from cache
load("./cache/edhec.RData")
load("./cache/SP500TR.RData")

# Drop some indexes and reorder
R = edhec[,c("Convertible Arbitrage", "Equity Market Neutral","Fixed Income Arbitrage", "Event Driven", "CTA Global", "Global Macro", "Long/Short Equity")]


########################################################################
# Returns-based performance analysis
########################################################################
# --------------------------------------------------------------------
# Returns through time
# --------------------------------------------------------------------
png(filename=paste(resultsdir, dataname, "-Cumulative-Returns.png", sep=""), units="in", height=5.5, width=9, res=96) 
par(cex.lab=.8) # should set these parameters once at the top
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) #c(bottom, left, top, right)
chart.CumReturns(R, main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= rainbow8equal, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(edhec.R, main = "", ylab = "Drawdown", colorset = rainbow8equal, cex.axis=.6, cex.lab=.7)
par(op)
dev.off()

# --------------------------------------------------------------------
# Monthly Returns and Risk
# --------------------------------------------------------------------
# @TODO: Too small: break this into two graphics? Directional, non-directional?
png(filename=paste(resultsdir, dataname, "-BarVaR.png", sep=""), units="in", height=5.5, width=9, res=96) 
# Generate charts of returns with ETL and VaR through time
par(mar=c(3, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.BarVaR(R, p=p, gap=36, main="", show.greenredbars=TRUE, 
              methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, 
              colorset=rep("Black",7), ylim=c(-.1,.15))
par(op)
dev.off()

# --------------------------------------------------------------------
# Rolling Performance
# --------------------------------------------------------------------
png(filename=paste(resultsdir, dataname, "-RollPerf.png", sep=""), units="in", height=5.5, width=9, res=96) 
# Generate charts of EDHEC index returns with ETL and VaR through time
par(mar=c(5, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.RollingPerformance(R, width=36, main="", colorset=rainbow8equal, legend.loc="topleft")
par(op)
dev.off()

# --------------------------------------------------------------------
# Returns and Risk Scatter
# --------------------------------------------------------------------
png(filename=paste(resultsdir, dataname, "-Scatter36m.png", sep=""), units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(last(edhec.R,36), main="EDHEC Index Trailing 36-Month Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()
png(filename=paste(resultsdir, dataname, "-ScatterSinceIncept.png", sep=""), units="in", height=5.5, width=4.5, res=96) 
chart.RiskReturnScatter(edhec.R, main="EDHEC Index Since Inception Performance", colorset=rainbow8equal, ylim=c(0,.2), xlim=c(0,.12))
dev.off()

# --------------------------------------------------------------------
# Table of Return and Risk Statistics
# --------------------------------------------------------------------
# @TODO: Too small, break into two panels?
require(Hmisc)
source(paste(functionsdir,'table.RiskStats.R', sep="")
incept.stats = t(table.RiskStats(R=R, p=p, Rf=Rf))
write.csv(incept.stats, file=paste(resultsdir, dataname, "-inception-stats.csv", sep=""))
png(filename=paste(resultsdir, dataname, "-InceptionStats.png", sep=""), units="in", height=5.5, width=9, res=96) 
textplot(format.df(incept.stats, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1,3,1,3,3,1,3,3,1,1,3,3,1,0), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, mar = c(0,0,4,0)+0.1))
dev.off()

# --------------------------------------------------------------------
# Compare Distributions
# --------------------------------------------------------------------
# @TODO: too small?
png(filename=paste(resultsdir, dataname, "-Distributions.png", sep=""), units="in", height=5.5, width=9, res=96) 
source(paste(functionsdir, "/page.Distributions", sep=""))
page.Distributions(R)
dev.off()

# --------------------------------------------------------------------
# Correlation Panels
# --------------------------------------------------------------------
# col3 <- colorRampPalette(c("darkgreen", "white", "darkred"))
library(gplots)
# Generate some color choices for the scale
skewedWB20 = c(colorpanel(16, "#008566","#E1E56D"), colorpanel(5, "#E1E56D", "#742414")[-1])
skewedGnYeRd10 = c(colorpanel(8, "darkgreen", "yellow"),colorpanel(3, "yellow", "darkred")[-1])
skewedGnYeRd20 = c(colorpanel(16, "darkgreen", "yellow"),colorpanel(5, "yellow", "darkred")[-1])
       
M <- cor(R)
colnames(M) = rownames(M) 
order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="complete")
M.hc2 <- M[order.hc2,order.hc2]
png(filename=paste(resultsdir, dataname, "-cor-inception.png", sep=""), units="in", height=5.5, width=4.5, res=96) 
corrplot(M.hc2, tl.col="black", tl.cex=0.8, method="shade", col=skewedWB20, cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25, shade.lwd=0, cl.length=11)
corrRect.hclust(M.hc2, k=3, method="complete", col="blue")
dev.off()

M36 <- cor(last(R,36))
colnames(M36) = rownames(M36) = row.names
order36.hc2 <- corrMatOrder(M36, order="hclust", hclust.method="complete")
M36.hc2 <- M36[order36.hc2,order36.hc2]
png(filename=paste(resultsdir, dataname, "-cor-tr36m.png", sep=""), units="in", height=5.5, width=4.5, res=96) 
corrplot(M36.hc2, tl.col="black", tl.cex=0.8, method="shade", col=skewedWB20, cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25, shade.lwd=0, cl.length=11)
corrRect.hclust(M36.hc2, k=3, method="complete", col="blue")
dev.off()

# @TODO: Add 12M rolling correlation to S&P500

# --------------------------------------------------------------------
# Rolling Correlation to S&P500 TR
# --------------------------------------------------------------------

png(filename=paste(resultsdir, dataname, "-RollCorr.png", sep=""), units="in", height=5.5, width=9, res=96) 
chart.RollingCorrelation(R,SP500.TR, width=24, legend.loc="bottomleft", colorset=rainbow8equal, main="Rolling 24-Month Correlations")
dev.off()
       
# --------------------------------------------------------------------
## Autocorrelation
# --------------------------------------------------------------------

# require(Hmisc)
AC.stats = t(table.Autocorrelation(R=R))
write.csv(AC.stats, file=paste(resultsdir, dataname, "-AC-stats.csv", sep=""))
png(filename=paste(resultsdir, dataname, "-ACStats.png", sep=""), units="in", height=5.5, width=9, res=96) 
# sort by p-value
AC.order = order(AC.stats[,7], decreasing=FALSE)
textplot(format.df(AC.stats[AC.order,], na.blank=TRUE, numeric.dollar=FALSE, rdec=c(rep(4,dim(AC.stats)[1])), col.just=rep("nc",dim(AC.stats)[2])), rmar = 0.7, cmar = 0.9, max.cex=1, halign = "center", valign = "center", row.valign="center", wrap.rownames=50, wrap.colnames=10)
dev.off()

png(filename=paste(resultsdir, dataname, "-ACStackedBars.png", sep=""), units="in", height=5.5, width=9, res=96) 
rownames(AC.stats)= sapply(colnames(R), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)
chart.StackedBar(as.matrix(AC.stats[,1:6]), colorset=bluemono, main="Observed Autocorrelation")
dev.off()