###############################################################################
# R (https://r-project.org/) Numeric Methods for Optimization of Portfolios
#
# Copyright (c) 2022-2032 Xinran Zhao
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


#' extract the risk value when knowing the weights
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param w the weight of the portfolio
#' @param ES_alpha the default value is 0.05, but could be specified as any value between 0 and 1
#' @param CSM_alpha the default value is 0.05, but could be specified as any value between 0 and 1
#' @param moment_setting the default is NULL, should provide moment_setting=list(mu=, sigma=) if customize momentFUN
#' @export extract_risk
extract_risk <- function(R, w, ES_alpha = 0.05, CSM_alpha = 0.05, moment_setting = NULL){
  res = list()
  if(is.null(moment_setting$mu)) res$mean = mean(R %*% w) else res$mean = moment_setting$mu %*% w
  if(is.null(moment_setting$sigma)) res$StdDev = sqrt(t(w) %*% cov(R) %*% w) else res$StdDev = sqrt(t(w) %*% moment_setting$sigma %*% w)
  
  if(ES_alpha > 0.5) ES_alpha <- (1 - ES_alpha)
  if(CSM_alpha > 0.5) CSM_alpha <- (1 - CSM_alpha)
  
  # ES/CSM by CVXR
  T <- dim(R)[1]
  X <- as.matrix(R)
  zeta <- CVXR::Variable(1)
  z <- CVXR::Variable(T)
  
  ## ES
  obj_es <- zeta + (1/(T*ES_alpha)) * sum(z)
  con_es <- list(z >= 0, z >= -X %*% w - zeta)
  p_es <- CVXR::Problem(CVXR::Minimize(obj_es), constraints = con_es)
  res_es = CVXR::solve(p_es, solver = "ECOS")
  res$ES = res_es$value
  
  ## CSM
  obj_CSM <- zeta + (1/CSM_alpha) * CVXR::p_norm(z, p=2)
  con_CSM = list(z >= 0, z >= -X %*% w - zeta)
  p_CSM <- CVXR::Problem(CVXR::Minimize(obj_CSM), constraints = con_CSM)
  res_CSM = CVXR::solve(p_CSM, solver = "ECOS")
  res$CSM = res_CSM$value
  
  res
}

