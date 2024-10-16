## ----setup, include=FALSE-------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 6, fig.height = 4, fig.align = "center")


## ---- warning = FALSE, message = FALSE------------------
library(PCRA)
library(PortfolioAnalytics)
library(CVXR)
library(xts)


## -------------------------------------------------------
stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2006-01-01", "2012-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP")
returnsAll <- selectCRSPandSPGMI("weekly",
                                   dateRange = dateRange,
                                   stockItems = stockItems, 
                                   factorItems = NULL, 
                                   subsetType = "CapGroupLast",
                                   subsetValues = "SmallCap", 
                                   outputType = "xts")
returns <- returnsAll[,1:30]
MARKET <- returnsAll[, 107]
returns10 <- returnsAll[140:300, 21:30]
range(index(returns))
range(index(returns10))


## -------------------------------------------------------
tsPlotMP(MARKET)


## -------------------------------------------------------
tsPlotMP(returns[, 21:30])


## -------------------------------------------------------
tsPlotMP(returns10)


## -------------------------------------------------------
funds <- colnames(returns10)
pspec <- portfolio.spec(funds)
pspec <- add.constraint(pspec, type="full_investment")
pspec <- add.constraint(pspec, type="long_only")
pspec <- add.objective(pspec, type="risk", name="var")


## -------------------------------------------------------
opt <- optimize.portfolio(returns10, pspec, optimize_method = "CVXR")


## -------------------------------------------------------
opt


## -------------------------------------------------------
outCovClassic <- opt.outputMvo(opt, returns10, digits = 3, frequency = "weekly")
(WtsCovClassic <- outCovClassic$Wgts)


## ---- eval = FALSE--------------------------------------
## custom.covRob.MM <- function(R, ...){
##   out <- list()
##   if(hasArg(tol)) tol = match.call(expand.dots = TRUE)$tol else tol = 1e-4
##   if(hasArg(maxit)) maxit = match.call(expand.dots = TRUE)$maxit else maxit = 50
##   robustCov <- RobStatTM::covRobMM(X=R, tol=tol, maxit=maxit)
##   out$sigma <- robustCov$cov
##   out$mu <- robustCov$center
##   return(out)
## }


## -------------------------------------------------------
opt <- optimize.portfolio(returns10, pspec, 
                   optimize_method = "CVXR", 
                   momentFUN = "custom.covRob.MM",
                   maxit = 100, tol = 1e-5)
outCovRobMM <- opt.outputMvo(opt, returns10, digits = 3, frequency = "weekly")
(WtsCovRobMM <- outCovRobMM$Wgts)


## ---- eval = FALSE--------------------------------------
## custom.covRob.Rocke <- function(R, ...){
##   out <- list()
##   if(hasArg(tol)) tol = match.call(expand.dots = TRUE)$tol else tol = 1e-4
##   if(hasArg(maxit)) maxit = match.call(expand.dots = TRUE)$maxit else maxit = 50
##   if(hasArg(initial)) initial = match.call(expand.dots = TRUE)$initial else initial = 'K'
##   if(hasArg(maxsteps)) maxsteps = match.call(expand.dots = TRUE)$maxsteps else maxsteps = 5
##   if(hasArg(propmin)) propmin = match.call(expand.dots = TRUE)$propmin else propmin = 2
##   if(hasArg(qs)) qs = match.call(expand.dots = TRUE)$qs else qs = 50
## 
##   robustCov <- RobStatTM::covRobRocke(X = R, initial = initial, maxsteps = maxsteps, propmin = propmin,
##                                       qs = qs, tol = tol, maxit = maxit)
## 
##   out$sigma <- robustCov$cov
##   out$mu <- robustCov$center
##   return(out)
## }


## -------------------------------------------------------
opt <- optimize.portfolio(returns10, pspec, 
                   optimize_method = "CVXR", 
                   momentFUN = "custom.covRob.Rocke",
                   tol = 1e-5, maxit =100, maxsteps = 7)
outCovRobRocke <- opt.outputMvo(opt, returns10, digits = 3, frequency = "weekly")
(WtsCovRobRocke <- outCovRobRocke$Wgts)


## ---- eval = FALSE--------------------------------------
## custom.covRob.Mcd <- function(R, ...){
## 
##   if(hasArg(control)) control = match.call(expand.dots = TRUE)$control else control = MycovRobMcd()
##   if(hasArg(alpha)) alpha = match.call(expand.dots = TRUE)$alpha else alpha = control$alpha
##   if(hasArg(nsamp)) nsamp = match.call(expand.dots = TRUE)$nsamp else nsamp = control$nsamp
##   if(hasArg(nmini)) nmini = match.call(expand.dots = TRUE)$nmini else nmini = control$nmini
##   if(hasArg(kmini)) kmini = match.call(expand.dots = TRUE)$kmini else kmini = control$kmini
##   if(hasArg(scalefn)) scalefn = match.call(expand.dots = TRUE)$scalefn else scalefn = control$scalefn
##   if(hasArg(maxcsteps)) maxcsteps = match.call(expand.dots = TRUE)$maxcsteps
##   else maxcsteps = control$maxcsteps
## 
##   if(hasArg(initHsets)) initHsets = match.call(expand.dots = TRUE)$initHsets
##   else initHsets = control$initHsets
## 
##   if(hasArg(seed)) seed = match.call(expand.dots = TRUE)$seed else seed = control$seed
##   if(hasArg(tolSolve)) tolSolve = match.call(expand.dots = TRUE)$tolSolve else tolSolve = control$tolSolve
##   if(hasArg(wgtFUN)) wgtFUN = match.call(expand.dots = TRUE)$wgtFUN else wgtFUN = control$wgtFUN
## 
##   if(hasArg(use.correction)) use.correction = match.call(expand.dots = TRUE)$use.correction
##   else use.correction = control$use.correction
## 
##   robustMCD <- robustbase::covMcd(x = R, alpha = alpha,
##                                   nsamp = nsamp, nmini = nmini,
##                                   kmini = kmini, seed = seed,
##                                   tolSolve = tolSolve, scalefn = scalefn,
##                                   maxcsteps = maxcsteps,
##                                   initHsets = initHsets,
##                                   wgtFUN = wgtFUN, use.correction = use.correction)
## 
##   return(list(mu = robustMCD$center, sigma = robustMCD$cov))
## }


## -------------------------------------------------------
opt <- optimize.portfolio(returns10, pspec, 
                   optimize_method = "CVXR", 
                   momentFUN = "custom.covRob.Mcd", 
                   alpha = 0.75, nsamp = 600)
outCovRobMcd <- opt.outputMvo(opt, returns10, digits = 3, frequency = "weekly")
(WtsCovRobMcd <- outCovRobMcd$Wgts)


## ---- eval = FALSE--------------------------------------
## MycovRobMcd <- function(alpha = 1/2,
##            nsamp = 500, nmini = 300, kmini = 5,
##            scalefn = "hrv2012", maxcsteps = 200,
##            seed = NULL, tolSolve = 1e-14,
##            wgtFUN = "01.original", beta,
##            use.correction = TRUE
##   ){
##     if(missing(beta) || !is.numeric(beta))
##       beta <- 0.975
## 
##     return(list(alpha = alpha, nsamp = nsamp,
##                 nmini = as.integer(nmini), kmini = as.integer(kmini),
##                 seed = as.integer(seed),
##                 tolSolve = tolSolve, scalefn = scalefn,
##                 maxcsteps = as.integer(maxcsteps),
##                 wgtFUN = wgtFUN, beta = beta,
##                 use.correction = use.correction))
##   }


## ---- eval = FALSE--------------------------------------
## covMcd.params <- MycovRobMcd(alpha = 0.75, nsamp = 600)
## opt <- optimize.portfolio(returns, pspec,
##                    optimize_method = "CVXR",
##                    momentFUN = "custom.covRob.Mcd",
##                    control = covMcd.params)


## ---- eval = FALSE--------------------------------------
## custom.covRob.TSGS <- function(R, ...){
##  if(hasArg(control)) control = match.call(expand.dots = TRUE)$control else control = MycovRobTSGS()
##  if(hasArg(filter)) filter = match.call(expand.dots = TRUE)$filter else filter = control$filter
## 
##  if(hasArg(partial.impute)) partial.impute = match.call(expand.dots = TRUE)$partial.impute
##  else partial.impute = control$partial.impute
## 
##  if(hasArg(tol)) tol = match.call(expand.dots=TRUE)$tol else tol=control$tol
##  if(hasArg(maxiter)) maxiter = match.call(expand.dots = TRUE)$maxiter else maxiter = control$maxiter
##  if(hasArg(loss)) loss = match.call(expand.dots = TRUE)$loss else loss = control$loss
##  if(hasArg(init)) init = match.call(expand.dots = TRUE)$init else init = control$init
## 
##   tsgsRob <- GSE::TSGS(x = R, filter = filter,
##                        partial.impute = partial.impute, tol = tol,
##                        maxiter = maxiter, method = loss,
##                        init = init)
## 
##   return(list(mu = tsgsRob@mu, sigma = tsgsRob@S))
## 
## }


## -------------------------------------------------------
opt <- optimize.portfolio(returns10, pspec, 
                   optimize_method = "CVXR", 
                   momentFUN = "custom.covRob.TSGS")
outCovRobTSGS <- opt.outputMvo(opt, returns10, digits = 3, frequency = "weekly")
(WtsCovRobTSGS <- outCovRobTSGS$Wgts)


## ---- eval = F------------------------------------------
## MycovRobTSGS <- function(filter = c("UBF-DDC","UBF","DDC","UF"),
##                 partial.impute = FALSE, tol = 1e-4, maxiter = 150,
##                 loss = c("bisquare","rocke"),
##                 init = c("emve", "qc", "huber", "imputed", "emve_c")){
## 
##   filter <- match.arg(filter)
##   loss <- match.arg(loss)
##   init <- match.arg(init)
## 
##   return(list(filter = filter, partial.impute = partial.impute,
##               tol = tol, maxiter = as.integer(maxiter),
##               loss = loss,init))
## }


## -------------------------------------------------------
dat <- data.frame(rbind(WtsCovClassic, WtsCovRobMM, WtsCovRobRocke,
                 WtsCovRobMcd, WtsCovRobTSGS))
print.data.frame(dat)


## -------------------------------------------------------
dat.mat <- rbind(outCovClassic[2:4], outCovRobMM[2:4],  outCovRobRocke[2:4],
                 outCovRobMcd[2:4], outCovRobTSGS[2:4])
dat <- as.data.frame(dat.mat)
row.names(dat) <- c("GmvLOcovClassic", "GmvLOcovRobMM", "GmvLOcovRobRocke",
                    "GmvLOcovRobMcd", "GmvLOcovRobTSGS")
print.data.frame(dat)


## -------------------------------------------------------
# Plot function
robPlot <- function(GMV, MARKET, plot=1){
  # Optimize Portfolio at Monthly Rebalancing and 5-Year Training
  if(plot == 1){
    momentEstFun = 'custom.covRob.Rocke'
    name = "GmvLOCovRobRocke"
  }else if(plot == 2){
    momentEstFun = 'custom.covRob.Mcd'
    name = "GmvLOCovMcd"
  }else if(plot == 3){
    momentEstFun = 'custom.covRob.TSGS'
    name = "GmvLOTSGS"
  }else{
    print("plot should be 1, 2 or 3")
    return()
  }
  
  bt.gmv.rob <- optimize.portfolio.rebalancing(returns, pspec,
                                            optimize_method = "CVXR",
                                            rebalance_on = "weeks",
                                            training_period = 100, 
                                            momentFUN = momentEstFun)
  
  # Extract time series of portfolio weights
  wts.gmv.rob <- extractWeights(bt.gmv.rob)
  # Compute cumulative returns of portfolio
  GMV.rob <- Return.rebalancing(returns, wts.gmv.rob)
  
  # Combine GMV.LO and MARKET cumulative returns
  ret.comb <- na.omit(merge(GMV.rob, GMV, MARKET, all=F))
  names(ret.comb) <- c(name, "GmvLO", "MARKET")
  
  plot <- backtest.plot(ret.comb, colorSet = c("darkgreen", "black", 
                        "red"), ltySet = c(1,2,3))
  
  return(list(ret = ret.comb, plot = plot))
}



## -------------------------------------------------------
bt.gmv <- optimize.portfolio.rebalancing(returns, pspec,
                                         optimize_method = "CVXR",
                                         rebalance_on="weeks",
                                         training_period = 100)
# Extract time series of portfolio weights
wts.gmv <- extractWeights(bt.gmv)
# Compute cumulative returns of portfolio
GMV <- Return.rebalancing(returns, wts.gmv)
  
res.covRob <- robPlot(GMV=GMV, MARKET=MARKET, plot=1)
res.covRob$plot


## -------------------------------------------------------
table.Drawdowns(res.covRob$ret$GmvLOCovRobRocke, top=1)


## -------------------------------------------------------
table.Drawdowns(res.covRob$ret$GmvLO, top=1)


## -------------------------------------------------------
table.Drawdowns(res.covRob$ret$MARKET, top=1)


## -------------------------------------------------------
set.seed(1234)
res.covMcd = robPlot(GMV=GMV, MARKET=MARKET, plot=2)
res.covMcd$plot


## -------------------------------------------------------
# longest drawdown for robust based portfolio
table.Drawdowns(res.covMcd$ret$GmvLOCovMcd, top=1) 


## -------------------------------------------------------
res.TSGS = robPlot(GMV=GMV, MARKET=MARKET, plot=3)
res.TSGS$plot


## -------------------------------------------------------
# longest drawdown for robust based portfolio
table.Drawdowns(res.TSGS$ret$GmvLOTSGS, top=1)


## ---- eval = FALSE--------------------------------------
## user.covRob.Rocke <- function(R){
##   out <- list()
##   robustCov <- RobStatTM::covRobRocke(X = R, maxit = 200, maxsteps = 10)
##   out$sigma <- robustCov$cov
##   out$mu <- robustCov$center
##   return(out)
## }


## ---- eval = FALSE--------------------------------------
## opt <- optimize.portfolio(returns, pspec,
##                    optimize_method = "CVXR",
##                    momentFUN = "user.covRob.Rocke")


## ---- eval = FALSE--------------------------------------
## user.covRob.OGK <- function(R){
##   robustCov <- robustbase::covOGK(x = R)
##   return(list(mu = robustCov$center, sigma = robustCov$cov))
## }


## ---- eval = FALSE--------------------------------------
## opt <- optimize.portfolio(returns, pspec,
##                    optimize_method = "CVXR",
##                    momentFUN = "user.covMcd.OGK")

## opt$moment_values
