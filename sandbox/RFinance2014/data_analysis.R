library(PerformanceAnalytics)

source("data_prep.R")

##### Equity Data for Example 1 and Example 2 #####
colors <- c(rep("blue", 15), rep("green", 15), rep("red", 5))
sd.order <- order(StdDev(equity.data))

# boxplot to compare return distributions
# mar(bottom, left, top, right)
# default: par(mar=c(5, 4, 4, 2) + 0.1)
png("data_figures/equity_box.png")
boxplot(coredata(equity.data[,sd.order]),
        cex.axis=0.8, las=3, ylab="Returns", pch=18,
        col=colors[sd.order],
        main="Return Distribution\n(sorted by StdDev)")
legend("topleft", legend=c("Large Cap", "Mid Cap", "Small Cap"), 
       fill=c("blue", "green", "red"), bty="n", cex=0.8)
dev.off()

##### edhec Data for Example 3 and Example 4 #####
p <- 0.95

png("data_figures/relative_barvar.png")
charts.BarVaR(R[,1:3], width=60, methods=c("ModifiedES", "ModifiedVaR"), 
              main="Relative Value")
dev.off()

png("data_figures/directional_barvar.png")
charts.BarVaR(R[,4:6], width=60, methods=c("ModifiedES", "ModifiedVaR"), 
              main="Directional")
dev.off()


colors <- c(rep("lightblue", 3), rep("red", 3))
ES.order <- order(ES(R, p=p, invert=FALSE))

png("data_figures/edhec_box.png")
boxplot(coredata(R[,ES.order]),
        cex.axis=0.8, las=3, ylab="Returns", pch=18,
        col=colors[ES.order],
        main="Return Distribution\n(sorted by Modified ES (95%))")
legend("topleft", legend=c("Relative Value", "Directional"), 
       fill=c("lightblue", "red"), bty="n", cex=0.8)
dev.off()
