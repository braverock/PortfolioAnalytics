# script for data analysis

library(PerformanceAnalytics)
library(lattice)
library(corrplot)

load("data/edhec.rda")

head(edhec)
R <- edhec[,1:4]
p <- 0.95

first(R)
last(R)

# plot the timeseries of returns
# plot(as.zoo(edhec))
xyplot(R, scales=list(y="same"))
charts.BarVaR(R, width=36, methods=c("ModifiedES", "ModifiedVaR"))
dev.off()

# boxplot to compare return distributions
# mar(bottom, left, top, right)
# default: par(mar=c(5, 4, 4, 2) + 0.1)
par(mar=c(10, 4, 4, 2) + 0.1)
boxplot(coredata(R[,order(ES(R, p=p, invert=FALSE))]),
        cex.axis=0.8, las=3, ylab="Returns", pch=18,
        main="Return Distribution\n(sorted by Modified ES (95%))")
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

# head(R[,order(ES(R, invert=FALSE))])
# head(R[,order(StdDev(R))])
# chart.Boxplot(R[,order(ES(R, invert=FALSE))])
# chart.Boxplot(R[,order(StdDev(R))])
# boxplot(coredata(R), col=c(2:5), cex.names=0.8, las=3)

# chart the distribution of returns
for(i in 1:ncol(R)){
  chart.Histogram(R[,i], methods=c("add.density", "add.normal"), 
                  colorset=c("lightgray", "black", "blue"))
  legend("topleft", legend=c("kernel density estimate", "normal"), 
         lty=c(1,1), col=c("black", "blue"), bty="n")
  Sys.sleep(1)
}


# chart the correlation and covariance
# from http://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res <- cor.mtest(R)

corrplot(cor(R), p.mat=res[[1]], main="Correlation", 
         sig.level=0.05, tl.cex=0.8)

# corrplot(M, method="number", bg="gray", tl.cex=0.8)
# corrplot.mixed(M, bg="gray", tl.cex=0.8)

# If I compare sample min variance portfolio to a ledoit-shrinkage or robust, 
# I should use plotcov to compare covaiance matrices



