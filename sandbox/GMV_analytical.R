GMV = function (sigma) {
 
  #'Global Minimum Variance
  #'Calculates the portfolio weights in accordance with a minimum variance strategy.
  #'From Goltz, F. & Lodh, A. 2013 "Scientific Beta Efficient Minimum Volatility Indices " EDHEC-Risk Institute Scientific Beta(2013)
  #'@Title Global Minimum Variance 
    #'@author Corporate Knights Inc.: Michael Fong /email{mfong@corporateknights.com}, Kyle Balkissoon /email{kyle@corporateknights.com}
    #'@param sigma = Covariance matrix of returns
    #'
    #'  
    step1 = sigma
    unit_vector = c(rep(1,ncol(step1)))
    # Calculate GMV portfolio weight matrix    
    step2 = (step1)^(-1)%*%unit_vector
    step3 = as.numeric(t(unit_vector)%*%(step1)^(-1)%*%unit_vector)
    step4 = step2/step3
    # Long-Only Adjustment - Set negative weights to zero  
    step5 = ifelse(step4<0,0,step4) 
    # Normalize Weights  
    step6 = step5/sum(step5)
    return(step6)
}