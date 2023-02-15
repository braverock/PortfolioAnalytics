#' extract the risk value when knowing the weights
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param portfolio an object of type "portfolio" specifying the constraints and objectives for the optimization
#' @param w the weight of the portfolio
#' @export extract_risk
extract_risk <- function(R, w, ES_alpha = 0.05, EQS_alpha = 0.05){
  res = list()
  res$mean = mean(R %*% w)
  res$StdDev = sqrt(t(w) %*% cov(R) %*% w)
  
  if(ES_alpha > 0.5) ES_alpha <- (1 - ES_alpha)
  if(EQS_alpha > 0.5) EQS_alpha <- (1 - EQS_alpha)
  
  # ES/EQS by CVXR
  T <- dim(R)[1]
  X <- as.matrix(R)
  zeta <- CVXR::Variable(1)
  z <- CVXR::Variable(T)
  
  ## ES
  obj_es <- zeta + (1/(T*ES_alpha)) * sum(z)
  con_es <- list(z >= 0, z >= -X %*% w - zeta)
  p_es <- CVXR::Problem(CVXR::Minimize(obj_es), constraints = con_es)
  res_es = CVXR::solve(p_es)
  res$ES = res_es$value
  
  ## EQS
  obj_eqs <- zeta + (1/EQS_alpha) * CVXR::p_norm(z, p=2)
  con_eqs = list(z >= 0, z >= -X %*% w - zeta)
  p_eqs <- CVXR::Problem(CVXR::Minimize(obj_eqs), constraints = con_eqs)
  res_eqs = CVXR::solve(p_eqs)
  res$EQS = res_eqs$value
  
  res
}

