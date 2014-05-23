lwShrink <- function(x, shrink=NULL){
  # port of matlab code from http://www.econ.uzh.ch/faculty/wolf/publications.html#9
  # Ledoit, O. and Wolf, M. (2004).
  # Honey, I shrunk the sample covariance matrix.
  # Journal of Portfolio Management 30, Volume 4, 110-119.
  
  # De-mean returns
  n <- nrow(x)
  p <- ncol(x)
  meanx <- colMeans(x)
  x <- x - matrix(rep(meanx, n), ncol=p, byrow=TRUE)
  
  # Compute sample covariance matrix using the de-meaned returns
  sample <- (1 / n) * (t(x) %*% x)
  
  # Compute prior
  var <- matrix(diag(sample), ncol=1)
  sqrtvar <- sqrt(var)
  tmpMat <- matrix(rep(sqrtvar, p), nrow=p)
  rBar <- (sum(sum(sample / (tmpMat * t(tmpMat)))) - p) / (p * (p - 1))
  prior <- rBar * tmpMat * t(tmpMat)
  diag(prior) <- var
  
  if(is.null(shrink)){
    # What is called pi-hat
    y <- x^2
    phiMat <- t(y) %*% y / n - 2 * (t(x) %*% x) * sample / n + sample^2
    phi <- sum(phiMat)
    
    # What is called rho-hat
    term1 <- (t(x^3) %*% x) / n
    help <- t(x) %*% x / n
    helpDiag <- matrix(diag(help), ncol=1)
    term2 <- matrix(rep(helpDiag, p), ncol=p, byrow=FALSE) * sample
    term3 <- help * matrix(rep(var, p), ncol=p, byrow=FALSE)
    term4 <- matrix(rep(var, p), ncol=p, byrow=FALSE) * sample
    thetaMat <- term1 - term2 - term3 + term4
    diag(thetaMat) <- 0
    rho <- sum(diag(phiMat)) + rBar * sum(sum(((1 / sqrtvar) %*% t(sqrtvar)) * thetaMat))
    
    # What is called gamma-hat
    gamma <- norm(sample - prior, "F")^2
    
    # Compute shrinkage constant
    kappa <- (phi - rho) / gamma
    shrinkage <- max(0, min(1, kappa / n))
  } else {
    shrinkage <- shrink
  }
  # Compute the estimator
  sigma <- shrinkage * prior + (1 - shrinkage) * sample
  out <- list(cov=sigma, prior=prior, shrinkage=shrinkage)
  return(out)
}
