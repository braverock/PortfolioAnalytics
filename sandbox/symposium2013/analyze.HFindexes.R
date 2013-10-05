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
require(PerformanceAnalytics)
require(vcd) # for color palates
require(corrplot) # for correlation charts
require(gplots)
require(extrafont)
require(Cairo)
require(Hmisc)

### Graphics
# To insert in Powerpoint 2007 (gah!), graphics should be rendered as pdf objects using the Cario package for better font handling:
# > CairoPDF(file="ConcPercESContrib-mETL-wHull.pdf", height=5.5, width=9, family="Cambria")
# then, do post-processing with ImageMagick on Linux:
# $ convert -density 300 ConcPercESContrib-mETL-wHull.pdf -quality 100 -sharpen 0x1.0 ConcPercESContrib-mETL-wHull.png
# That appears to give a sharp, dense picture that shows well on slides.  Use the pdf directly if possible, of course.

# Corporate colors and fonts
wb13color = c(
  rgb(0,65,101, max=255), # Blair Blue
  rgb(129,147,219, max=255), # Light Purple
  rgb(0,133,102, max=255), # Dark Green
  rgb(0,0,0, max=255), # Black
  rgb(154,155,156, max=255), # Light Grey
  rgb(0,122,201, max=255), # Dark Cyan
  rgb(240,171,0, max=255), # Bright Orange
  rgb(72,72,74, max=255), # Dark Grey
  rgb(122,184,0, max=255), # Bright Green
  rgb(87,6,140, max=255), # Dark Purple
  rgb(220,80,52, max=255), # Dark Orange
  rgb(243,211,17, max=255), # Yellow
  rgb(61,183,228, max=255)  # Bright Cyan
  )

# Skewed-scale GrYlRd in WmB colors for correlation charts
skewedWB20 = c(colorpanel(16, "#008566","#E1E56D"), colorpanel(5, "#E1E56D", "#742414")[-1])

CairoFonts(
  regular="Cambria:style=Regular",
  bold="Cambria:style=Bold",
  italic="Cambria:style=Italic",
  bolditalic="Cambria:style=Bold Italic,BoldItalic",
  symbol="Symbol"
)

par(las=1) # axis labels all horizontal
par(cex.lab=.8) # shrink axis labels
op <- par(no.readonly = TRUE)

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
colorset = wb13color
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
R.names = colnames(R)
R.foldednames = sapply(colnames(R), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)

########################################################################
# Returns-based performance analysis
########################################################################
# --------------------------------------------------------------------
# Returns through time
# --------------------------------------------------------------------
CairoPDF(file=paste(resultsdir, dataname, "-Cumulative-Returns.pdf", sep=""), height=5.5, width=9) 
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) #c(bottom, left, top, right)
chart.CumReturns(R, main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= wb13color, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7, las=1, pch="")
par(mar = c(4, 4, 0, 2))
chart.Drawdown(edhec.R, main = "", ylab = "Drawdown", colorset = wb13color, cex.axis=.6, cex.lab=.7, las=1)
par(op)
dev.off()

# --------------------------------------------------------------------
# Monthly Returns and Risk
# --------------------------------------------------------------------
# Done
CairoPDF(file=paste(resultsdir, dataname, "-BarVaR.pdf", sep=""), height=5.5, width=9) 
# Generate charts of returns with ETL and VaR through time
par(mar=c(3, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.BarVaR(R[,1:4], p=p, gap=36, main="", show.greenredbars=TRUE, 
              methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, 
              colorset=rep("Black",7), ylim=c(-.1,.15), las=1, clean="boudt")
par(op)
dev.off()
CairoPDF(file=paste(resultsdir, dataname, "-BarVaR2.pdf", sep=""), height=5.5, width=9) 
# Generate charts of returns with ETL and VaR through time
par(mar=c(3, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.BarVaR(R[,5:7], p=p, gap=36, main="", show.greenredbars=TRUE, 
              methods=c("ModifiedES", "ModifiedVaR"), show.endvalue=TRUE, 
              colorset=rep("Black",7), ylim=c(-.1,.15), las=1, clean="boudt")
par(op)
dev.off()

# --------------------------------------------------------------------
# Rolling Performance
# --------------------------------------------------------------------
CairoPDF(file=paste(resultsdir, dataname, "-RollPerf.pdf", sep=""), height=5.5, width=9) 
# Generate charts of EDHEC index returns with ETL and VaR through time
par(mar=c(5, 4, 0, 2) + 0.1) #c(bottom, left, top, right)
charts.RollingPerformance(R, width=36, main="", pch="", colorset=wb13color, legend.loc="topleft", las=1)
par(op)
dev.off()

# --------------------------------------------------------------------
# Returns and Risk Scatter
# --------------------------------------------------------------------
CairoPDF(file=paste(resultsdir, dataname, "-Scatter36m.pdf", sep=""), height=5.5, width=4.5) 
chart.RiskReturnScatter(last(edhec.R,36), main="EDHEC Index Trailing 36-Month Performance", colorset=wb13color, ylim=c(0,.2), xlim=c(0,.12), las=1)
dev.off()
CairoPDF(file=paste(resultsdir, dataname, "-ScatterSinceIncept.pdf", sep=""), height=5.5, width=4.5) 
chart.RiskReturnScatter(edhec.R, main="EDHEC Index Since Inception Performance", colorset=wb13color, ylim=c(0,.2), xlim=c(0,.12), las=1)
dev.off()

# --------------------------------------------------------------------
# Table of Return and Risk Statistics
# --------------------------------------------------------------------
# @TODO: Too small, break into two panels?
source(paste(functionsdir,'table.RiskStats.R', sep=""))
incept.stats = t(table.RiskStats(R=R, p=p, Rf=Rf))
write.csv(incept.stats, file=paste(resultsdir, dataname, "-inception-stats.csv", sep=""))
CairoPDF(file=paste(resultsdir, dataname, "-InceptionStats.pdf", sep=""), height=5.5, width=4.5) 
textplot(format.df(incept.stats, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1,3,1,3,3,1,3,3,1,1,3,3,1,0), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, mar = c(0,0,4,0)+0.1))
dev.off()

# --------------------------------------------------------------------
# Compare Distributions
# --------------------------------------------------------------------
# 
CairoPDF(file=paste(resultsdir, dataname, "-Distributions.pdf", sep=""), height=5.5, width=9) 
#source(paste(functionsdir, "/page.Distributions", sep=""))
page.Distributions(R[,1:4], colorset=wb13color, las=1)
dev.off()

CairoPDF(file=paste(resultsdir, dataname, "-Distributions2.pdf", sep=""), height=5.5, width=9) 
#source(paste(functionsdir, "/page.Distributions", sep=""))
page.Distributions(R[,5:7], colorset=wb13color, las=1)
dev.off()

# --------------------------------------------------------------------
# Correlation Panels
# --------------------------------------------------------------------
# col3 <- colorRampPalette(c("darkgreen", "white", "darkred"))       
M <- cor(R)
colnames(M) = rownames(M) 
order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="complete")
M.hc2 <- M[order.hc2,order.hc2]
CairoPDF(file=paste(resultsdir, dataname, "-cor-inception.pdf", sep=""), height=5.5, width=4.5) 
corrplot(M.hc2, tl.col="black", tl.cex=0.8, method="shade", col=skewedWB20, cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25, shade.lwd=0, cl.length=11)
corrRect.hclust(M.hc2, k=3, method="complete", col="blue")
dev.off()

M36 <- cor(last(R,36))
colnames(M36) = rownames(M36) = rownames(M)
order36.hc2 <- corrMatOrder(M36, order="hclust", hclust.method="complete")
M36.hc2 <- M36[order36.hc2,order36.hc2]
CairoPDF(file=paste(resultsdir, dataname, "-cor-tr36m.pdf", sep=""), height=5.5, width=4.5) 
corrplot(M36.hc2, tl.col="black", tl.cex=0.8, method="shade", col=skewedWB20, cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25, shade.lwd=0, cl.length=11)
corrRect.hclust(M36.hc2, k=3, method="complete", col="blue")
dev.off()

# --------------------------------------------------------------------
# Table of Return correlations
# --------------------------------------------------------------------

write.csv(M, file=paste(resultsdir, dataname, "-inception-cor.csv", sep=""))
  colnames(M)=R.foldednames
# write(print(xtable(M, digits=1, align=rep("c",8)), type="html", html.table.attributes = "border = '0', align = 'center'"), file=paste(resultsdir, dataname, "-inception-cor.html", sep=""))
# write(print(xtable(M, digits=1, align=rep("c",8))), file=paste(resultsdir, dataname, "-inception-cor.latex", sep=""))
# write(pandoc.table.return(M, digits=1, split.tables=Inf), file=paste(resultsdir, dataname, "-inception-cor.md", sep=""))
  
# --------------------------------------------------------------------
# Rolling Correlation to S&P500 TR
# --------------------------------------------------------------------

CairoPDF(file=paste(resultsdir, dataname, "-RollCorr.pdf", sep=""), height=5.5, width=9) 
chart.RollingCorrelation(R,SP500.TR, width=24, legend.loc=NULL, colorset=wb13color, main="Rolling 24-Month Correlation to S&P500 TR", las=1)
legend("bottomleft", legend=colnames(R), inset = 0.02, border.col="darkgrey", lwd=3, col=wb13color, cex=0.7, y.intersp=1.1)
dev.off()
       
# --------------------------------------------------------------------
# Autocorrelation
# --------------------------------------------------------------------

# require(Hmisc)
AC.stats = t(table.Autocorrelation(R=R))
write.csv(AC.stats, file=paste(resultsdir, dataname, "-AC-stats.csv", sep=""))
CairoPDF(file=paste(resultsdir, dataname, "-ACStats.pdf", sep=""), height=5.5, width=4.5) 
# sort by p-value
AC.order = order(AC.stats[,7], decreasing=FALSE)
textplot(format.df(AC.stats[AC.order,], na.blank=TRUE, numeric.dollar=FALSE, rdec=c(rep(4,dim(AC.stats)[1])), col.just=rep("nc",dim(AC.stats)[2])), rmar = 0.7, cmar = 0.9, max.cex=1, halign = "center", valign = "center", row.valign="center", wrap.rownames=50, wrap.colnames=10)
dev.off()

CairoPDF(file=paste(resultsdir, dataname, "-ACStackedBars.pdf", sep=""), height=5.5, width=9) 
rownames(AC.stats)= sapply(colnames(R), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)
chart.StackedBar(as.matrix(AC.stats[,1:6]), colorset=bluemono, main="Observed Autocorrelation", las=1)
dev.off()
       
# --------------------------------------------------------------------
# ETL parameterization charts
# --------------------------------------------------------------------
# Requires a recent modification to the chart in PerformanceAnalytics to make the y-axes match; in  revision 3191
source('./R/chart.VaRSensitivity.R')
CairoPDF(file=paste(resultsdir, dataname, "-ETL-sensitivity.pdf", sep=""), height=5.5, width=9)
layout(matrix(c(1:8), nrow=2))
par(mar = c(4, 4, 5, 2)+0.1) #c(bottom, left, top, right)
for(i in 1:NCOL(R)){
  chart.VaRSensitivity(R[,i], methods=c("ModifiedES","HistoricalES", "GaussianES"), legend.loc=NULL, clean="boudt", colorset=wb13color, lty=c(2,1,2), lwd=3, main=R.names[i], ylim=c(-0.09,0), ylab="Expected Tail Loss", las=1) #c("orange", "black", "darkgray")
  abline(v = 1-1/12, col = wb13color[11], lty = 2, lwd=1)
}
  plot.new()
  legend("center", legend=c("Modified \nETL","Historical \nETL", "Gaussian \nETL"), lty=c(2,1,2), lwd=3, col=wb13color, cex=1.2, y.intersp=2, box.col="darkgrey")
par(op)
dev.off()
  
  