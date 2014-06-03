library(PortfolioAnalytics)

# Use edhec data
data(edhec)

R <- edhec[,1:10]

# Dimensions of data
m <- nrow(R)
N <- ncol(R)

# Number of factors to use
k <- 3

##### Step 1 #####
fit <- statistical.factor.model(R, k)
names(fit)
beta <- fit$factor_loadings
f <- fit$factor_realizations
res <- fit$residuals

##### Step 2 #####
# Compute the moments of the factors and idiosyncratic risk factors
# Note: The idiosyncratic factors are the residuals in the model (i.e. the
# unexplained asset return variation)

# Check for equality with functions from Kris Boudt and functions I have 
# included in the package

# residual moments
denom <- m - k - 1
stockM2 <- colSums(res^2) / denom
stockM3 <- colSums(res^3) / denom
stockM4 <- colSums(res^4) / denom

# Compute the centered factors
# f.centered <- center(f)

# factor moments
# (k x k)
factorM2 <- cov(f)

# (k x k^2)
factorM3 <- PerformanceAnalytics:::M3.MM(f)

# (k x k^3)
factorM4 <- PerformanceAnalytics:::M4.MM(f)

##### Step 3 #####
# Compute the covariance, coskewness, and cokurtosis estimates from the statistical 
# factor model.

# covariance matrix
all.equal(
  PortfolioAnalytics:::covarianceMF(beta, stockM2, factorM2),
  extractCovariance(fit)
)

# coskewness matrix
all.equal(
  PortfolioAnalytics:::coskewnessMF(beta, stockM3, factorM3),
  extractCoskewness(fit)
)

# cokurtosis matrix
all.equal(
  PortfolioAnalytics:::cokurtosisMF(beta, stockM2,stockM4, factorM2, factorM4),
  extractCokurtosis(fit)
)
