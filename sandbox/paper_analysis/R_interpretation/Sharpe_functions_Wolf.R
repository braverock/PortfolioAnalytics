

diffSharpeRatio <-
function(pr1 , pr2  , rebalweights1 , rebalweights2 , C = 0.0001 ,  initialV = 1000 ){

  vwealth1 = vwealth2 = initialV*(1-C)
  for( t in 1:length(pr1) ){
     # should be a different C in function of the change in weights and thus not at the portfoli but asset level
     vwealth1 = c( vwealth1 , vwealth1[t]*(1+pr1[t])*(  1-C*sum(abs(rebalweights1[t]))  )  ); 
     vwealth2 = c( vwealth2 , vwealth2[t]*(1+pr2[t])*(  1-C*sum(abs(rebalweights2[t]))   ) ); 
  }
  vret1 = (vwealth1[-1]-vwealth1[1:(length(vwealth1)-1)])/vwealth1[1:(length(vwealth1)-1)]
  vret2 = (vwealth2[-1]-vwealth2[1:(length(vwealth1)-1)])/vwealth2[1:(length(vwealth2)-1)]
  #out = boot.time.inference( ret = cbind( vret1_mon , vret2_mon ) , b = 4 , M = 1000 )
  out = hac.inference(ret = cbind(vret1,vret2) )

  return( c(out$Difference,out$p.Values[1] ) )
}


boot.time.inference <-
function(ret, b, M, Delta.null = 0, digits = 4){
    T = length(ret[,1])
    l = floor(T / b)
    Delta.hat = sharpe.ratio.diff(ret)
    d = abs(Delta.hat - Delta.null) / compute.se.Parzen.pw(ret)
    p.value = 1
    for (m in (1:M)) {
      ret.star = ret[cbb.sequence(T, b),]
      Delta.hat.star = sharpe.ratio.diff(ret.star)
      ret1.star = ret.star[,1]
      ret2.star = ret.star[,2]
      mu1.hat.star = mean(ret1.star)
      mu2.hat.star = mean(ret2.star)
      gamma1.hat.star = mean(ret1.star^2)
      gamma2.hat.star = mean(ret2.star^2)
      gradient = rep(0, 4)
      gradient[1] = gamma1.hat.star / (gamma1.hat.star -mu1.hat.star^2)^1.5
      gradient[2] = - gamma2.hat.star / (gamma2.hat.star - mu2.hat.star^2)^1.5
      gradient[3] = -0.5 * mu1.hat.star / (gamma1.hat.star - mu1.hat.star^2)^1.5
      gradient[4] = 0.5 * mu2.hat.star / (gamma2.hat.star - mu2.hat.star^2)^1.5
      y.star = data.frame(ret1.star - mu1.hat.star, ret2.star -  mu2.hat.star,
        ret1.star^2 - gamma1.hat.star, ret2.star^2 - gamma2.hat.star)
      Psi.hat.star = matrix(0, 4, 4)
      for (j in (1:l)) {
        zeta.star = b^0.5 * mean(y.star[((j-1)*b+1):(j*b),])
        Psi.hat.star = Psi.hat.star + zeta.star %*% t(zeta.star)
      }
      Psi.hat.star = Psi.hat.star / l
      se.star = as.numeric(sqrt(t(gradient) %*% Psi.hat.star %*% gradient / T))
      d.star = abs(Delta.hat.star - Delta.hat) / se.star
      if (d.star >= d) {
        p.value = p.value + 1
      }
    }
    p.value = p.value / (M + 1)
    list(Difference = round(Delta.hat, digits), p.Value = round(p.value, digits))
  }

 sharpe.ratio.diff <- function(ret){
    ret1 = ret[,1]
    ret2 = ret[,2]
    mu1.hat = mean(ret1)
    mu2.hat = mean(ret2)
    sig1.hat = var(ret1)^.5
    sig2.hat = var(ret2)^.5
    SR1.hat = mu1.hat / sig1.hat
    SR2.hat = mu2.hat / sig2.hat
    diff = SR1.hat - SR2.hat
    diff
  }

hac.inference <-
function(ret, digits = 3) {
    ret1 = ret[,1]
    ret2 = ret[,2]
    mu1.hat = mean(ret1)
    mu2.hat = mean(ret2)
    sig1.hat = var(ret1)^.5
    sig2.hat = var(ret2)^.5
    SR1.hat = mu1.hat / sig1.hat
    SR2.hat = mu2.hat / sig2.hat
    SRs = round(c(SR1.hat, SR2.hat), digits)
    diff = SR1.hat - SR2.hat
    names(SRs) = c("SR1.hat", "SR2.hat")
    Delta.hat = SR1.hat - SR2.hat
    se = compute.se.Parzen(ret)
    se.pw = compute.se.Parzen.pw(ret)
    SEs = round(c(se, se.pw), digits)
    names(SEs) = c("HAC", "HAC.pw")
    PV = 2 * pnorm(-abs(diff) / se)
    PV.pw = 2 * pnorm(-abs(diff)/ se.pw)
    PVs = round(c(PV, PV.pw), digits)
    names(PVs) = c("HAC", "HAC.pw")
    list(Sharpe.Ratios = SRs, Difference = round(diff, digits),
         Standard.Errors = SEs, p.Values = PVs)
    
    
  }

compute.se.Parzen <-
function(ret){
    # implements the Parzen kernel estimator with automatic choice of bandwith
    ret1 = ret[,1]
    ret2 = ret[,2]
    T = length(ret1)
    mu1.hat = mean(ret1)
    mu2.hat = mean(ret2)
    gamma1.hat = mean(ret1^2)
    gamma2.hat = mean(ret2^2)
    gradient = rep(0, 4)
    gradient[1] = gamma1.hat / (gamma1.hat - mu1.hat^2)^1.5
    gradient[2] = - gamma2.hat / (gamma2.hat - mu2.hat^2)^1.5
    gradient[3] = -0.5 * mu1.hat / (gamma1.hat - mu1.hat^2)^1.5
    gradient[4] = 0.5 * mu2.hat / (gamma2.hat - mu2.hat^2)^1.5
    V.hat = compute.V.hat(ret)
    Psi.hat = compute.Psi.hat(V.hat)
    se = as.numeric(sqrt(t(gradient) %*% Psi.hat %*% gradient / T))
    se
  }


block.size.calibrate <-
function(ret, b.vec= c(1, 3, 6, 10), alpha = 0.05, M = 199, K = 1000, b.av = 5, T.start = 50){
    b.len = length(b.vec)
    emp.reject.probs = rep(0, b.len)
    Delta.hat = sharpe.ratio.diff(ret)
    ret1 = ret[,1]
    ret2 = ret[,2]
    T = length(ret1)
    Var.data = matrix(0, T.start + T, 2)
    #Var.data[1, ] = ret[1, ]
    Var.data[1, ] = as.numeric(ret[1, ])
    Delta.hat = sharpe.ratio.diff(ret)
    fit1 = lm(ret1[2:T] ~ ret1[1:(T-1)] + ret2[1:(T-1)])
    fit2 = lm(ret2[2:T] ~ ret1[1:(T-1)] + ret2[1:(T-1)])
    coef1 = as.numeric(fit1$coef)
    coef2 = as.numeric(fit2$coef)
    resid.mat = cbind(as.numeric(fit1$resid), as.numeric(fit2$resid))
    for (k in (1:K)) {
      resid.mat.star = rbind(c(0,0), resid.mat[sb.sequence(T-1, b.av,T.start+T-1),])
      print(resid.mat.star)
      for (t in (2:(T.start+T))) {
        Var.data[t, 1] = coef1[1] + coef1[2]*Var.data[t-1,1] +
          coef1[3]*Var.data[t-1,2] + resid.mat.star[t,1]
        Var.data[t, 2] = coef2[1] + coef2[2]*Var.data[t-1,1] +
          coef2[3]*Var.data[t-1,2] + resid.mat.star[t,2]
      }
      # print(Var.data)
      Var.data.trunc = Var.data[(T.start+1):(T.start+T), ]
      for (j in (1:b.len)) {
        p.Value = boot.time.inference(Var.data.trunc, b.vec[j], M, Delta.hat)$p.Value
        if (p.Value <= alpha) {
          emp.reject.probs[j] = emp.reject.probs[j] + 1
        }
      }
    }
    emp.reject.probs = emp.reject.probs / K
    b.order = order(abs(emp.reject.probs - alpha))
    b.opt = b.vec[b.order[1]]
    b.vec.with.probs = rbind(b.vec, emp.reject.probs)
    colnames(b.vec.with.probs) = rep("", length(b.vec))
    list(Empirical.Rejection.Probs = b.vec.with.probs, b.optimal = b.opt)
  }

compute.V.hat<-
function(ret){
    # what Andrews (1991) calls V.hat = V(theta.hat) in our context
    ret1 = ret[,1]
    ret2 = ret[,2]
    V.hat = cbind(ret1 - mean(ret1), ret2 - mean(ret2),
      ret1^2 - mean(ret1^2), ret2^2 - mean(ret2^2))
    V.hat
  }

compute.Psi.hat<-
function(V.hat) {
    # see first part of (2.5) of Andrews (1991)
    # except we call Psi.hat what he calls J.hat there
    T = length(V.hat[,1])
    alpha.hat = compute.alpha.hat(V.hat)
    S.star = 2.6614 * (alpha.hat * T)^.2
    Psi.hat = compute.Gamma.hat(V.hat, 0)
    j = 1
    while (j < S.star) {
      Gamma.hat = compute.Gamma.hat(V.hat, j)
      Psi.hat = Psi.hat + kernel.Parzen(j / S.star) * (Gamma.hat + t(Gamma.hat))
      j = j + 1
    }
    Psi.hat = (T / (T - 4)) * Psi.hat
    Psi.hat
  }

compute.alpha.hat<-
function(V.hat){
    # see (6.4) of Andrews (1991)
    dimensions = dim(V.hat)
    T = dimensions[1]
    p = dimensions[2]
    numerator = 0
    denominator = 0
    for (i in (1:p)) {
      fit = ar(V.hat[, i], 0, 1, method = "ols")
      rho.hat = as.numeric(fit[2])
      sig.hat = sqrt(as.numeric(fit[3]))
      numerator = numerator + 4 * rho.hat^2 * sig.hat^4 / (1 - rho.hat)^8
      denominator = denominator + sig.hat^4 / (1 - rho.hat)^4
    }
    numerator / denominator
  }

 compute.Gamma.hat<-
function(V.hat, j){
    # see second part of (2.5) of Andrews (1991)
    dimensions = dim(V.hat)
    T = dimensions[1]
    p = dimensions[2]
    Gamma.hat = matrix(0, p, p)
    if (j >= T)
      stop("j must be smaller than the row dimension!")
    for (i in ((j+1):T)) 
      Gamma.hat = Gamma.hat + V.hat[i,] %*% t(V.hat[i - j,])
    Gamma.hat = Gamma.hat / T
    Gamma.hat
  }
kernel.Parzen<-
function(x)
{
  if (abs(x) <= 0.5)
    result = 1 - 6 * x^2 + 6 * abs(x)^3
  else if (abs(x) <= 1)
    result = 2 * (1 - abs(x))^3
  else
    result = 0
  result
}
compute.se.Parzen.pw<-
function(ret){
    # implements the prewhitened Parzen kernel estimator of A&M (1992)
    ret1 = ret[,1]
    ret2 = ret[,2]
    mu1.hat = mean(ret1)
    mu2.hat = mean(ret2)
    gamma1.hat = mean(ret1^2)
    gamma2.hat = mean(ret2^2)
    gradient = rep(0, 4)
    gradient[1] = gamma1.hat / (gamma1.hat - mu1.hat^2)^1.5
    gradient[2] = - gamma2.hat / (gamma2.hat - mu2.hat^2)^1.5
    gradient[3] = -0.5 * mu1.hat / (gamma1.hat - mu1.hat^2)^1.5
    gradient[4] = 0.5 * mu2.hat / (gamma2.hat - mu2.hat^2)^1.5    
    T = length(ret1)
    V.hat = compute.V.hat(ret)
    A.ls = matrix(0, 4, 4)
    V.star = matrix(0, T - 1, 4)
    reg1 = V.hat[1:T-1,1]
    reg2 = V.hat[1:T-1,2]
    reg3 = V.hat[1:T-1,3]
    reg4 = V.hat[1:T-1,4]
    for (j in (1:4)) {
      fit = lm(V.hat[2:T,j] ~ -1 + reg1 + reg2 + reg3 + reg4)
      A.ls[j,] = as.numeric(fit$coef)
      V.star[,j] = as.numeric(fit$resid)
    }
    # SVD adjustment of A&M (1992, page 957)
    svd.A = svd(A.ls)
    d = svd.A$d
    d.adj = d
    for (i in (1:4)) {
      if (d[i] > 0.97)
        d.adj[i] = 0.97
      else if (d[i] < -0.97)
        d.adj[i] = -0.97
    }
    A.hat = svd.A$u %*% diag(d.adj) %*% t(svd.A$v)
    D = solve(diag(4) - A.hat)
    reg.mat = rbind(reg1, reg2, reg3, reg4)
    for (j in (1:4)) {
      V.star[,j] = V.hat[2:T,j] - A.hat[j,] %*% reg.mat
    }
    Psi.hat = compute.Psi.hat(V.star)
    Psi.hat = D %*% Psi.hat %*% t(D)
    se = as.numeric(sqrt(t(gradient) %*% Psi.hat %*% gradient / T))
    se
  }
