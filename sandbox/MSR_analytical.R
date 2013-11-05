MSR = function (sigma,return_estimate,long_only) {
  
  #'Maximum Sharpe Ratio
  #'Calculates the portfolio weights in accordance with a maximum sharpe ratio strategy.
  #'From Gautam, K. & Lodh, A. 2013 "Scientific Beta Efficient Maximum Sharpe Ratio Indices " EDHEC-Risk Institute Scientific Beta(2013)
  #'@Title Efficient Maximum Sharpe Ratio
    #'@author Corporate Knights Inc.: Michael Fong /email{mfong@corporateknights.com}, Kyle Balkissoon /email{kyle@corporateknights.com}
    #'@param sigma = Covariance matrix of returns
    #'@param return_estimate = vector of expected returns (or) expected returns - risk free rate 
    #'
    #' 
  step1 = sigma
  unit_vector = c(rep(1,ncol(step1)))
  #Calculate EMS portfolio weight matrix   
  step2 = (step1)^(-1)%*%return_estimate 
  step3 = as.numeric((unit_vector)%*%(step1)^(-1)%*%return_estimate)
  step4 = step2/step3
  # Long-Only Adjustment - Set negative weights to zero    
  if(long_only=='TRUE'){step5 = ifelse(step4<0,0,step4)
}else{step5 = step4}
  # Normalize Weights  
  step6 = step5/sum(step5)
  return(t(step6))
}