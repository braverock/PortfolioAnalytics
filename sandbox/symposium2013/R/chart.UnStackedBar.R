chart.UnStackedBar <- function(w, colorset=1:NROW(w), rotate=c("vertical", "horizontal"), yaxis=TRUE, equal.line=FALSE, las=par(las), ...)
  {
  # Weights should come in as:
  #              Convertible Arbitrage CTA Global Distressed Securities 
  #   2000-01-01            0.02500000 0.14601749             0.0250000 
  #   2001-01-01            0.15785710 0.19577551             0.0250000 
  #   2002-01-01            0.24431295 0.02500000             0.0250000 
  #   2003-01-01            0.21955470 0.06590151             0.0250000
  
  # if (wrap) 
  #   row.names = sapply(rownames(object), function(x) paste(strwrap(x, wrap.rownames), collapse = "\n"), USE.NAMES = FALSE)
  rotate = rotate[1]
  if(is(w, "xts"))
    row.names=index(w)
  else
    row.names = sapply(rownames(w), function(x) paste(strwrap(x,10), collapse = "\n"), USE.NAMES=FALSE)
  if(rotate=="vertical"){
    par(oma = c(4,8,2,1), mar=c(0,1,0,1)) # c(bottom, left, top, right)
    layout(matrix(c(1:NCOL(w)), nr = 1, byrow = TRUE))
    for(i in 1:NCOL(w)){
      if(i==1){
        barplot(w[,i], col=colorset[i], horiz=TRUE, xlim=c(0,max(w)), axes=FALSE, names.arg=row.names, las=las,...)
        abline(v=0, col="darkgray")
        if(equal.line)
          abline(v=1/NROW(w), col="darkgray", lty=2)
        axis(1, cex.axis = 1, col = "darkgray", las=las)
        mtext(colnames(w)[i], side= 3, cex=0.8, adj=0.5)
      } 
      else{
        barplot(w[,i], col=colorset[i], horiz=TRUE, xlim=c(0,max(w)), axes=FALSE, names.arg="", ylab=colnames(w)[i], ...)
        abline(v=0, col="darkgray")
        if(equal.line)
          abline(v=1/NROW(w), col="darkgray", lty=2)
        if(yaxis)
          axis(1, cex.axis = 1, col = "darkgray", las=las)
        mtext(colnames(w)[i], side= 3, cex=0.8, adj=0.5)
      }
    }
  }
  else { # rotation is horizontal (zero line is horizontal)
    par(oma = c(8,4,2,1), mar=c(1,0,1,1)) # c(bottom, left, top, right)
    layout(matrix(c(1:NCOL(w)), nr = NCOL(w), byrow = FALSE))
    for(i in 1:NCOL(w)){
      if(i==NCOL(w)){
        barplot(w[,i], col=colorset[i], horiz=FALSE, ylim=c(0,max(w)), axes=FALSE, names.arg=row.names, las=las, cex.names=1.5, ...)
        abline(h=0, col="darkgray")
        if(equal.line)
          abline(h=1/NROW(w), col="darkgray", lty=2)
        axis(2, cex.axis = 1, col = "darkgray", las=las)
        mtext(colnames(w)[i], side= 3, cex=1, adj=0)
      } 
      else{
        barplot(w[,i], col=colorset[i], horiz=FALSE, ylim=c(0,max(w)), axes=FALSE, names.arg=rep("",length(w[,i])), ylab=colnames(w)[i], ...)
        abline(h=0, col="darkgray")
        if(equal.line)
          abline(h=1/NROW(w), col="darkgray", lty=2)
        if(yaxis)
          axis(2, cex.axis = 1, col = "darkgray", las=las)
        mtext(colnames(w)[i], side= 3, cex=1, adj=0)
      }
    }
  }
  par(op)
}

# Another way, this without layout
# http://timotheepoisot.fr/2013/02/17/stacked-barcharts/