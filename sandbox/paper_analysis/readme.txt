#################################################################################
# Code and data for replication of analysis of
# Boudt K, Carl P and Peterson B. 2009. Portfolio optimization with risk budgets. 
# Disclaimer: This is free software and comes with ABSOLUTELY NO WARRANTY
# These programs are compatible with R version 2.13.0
#################################################################################

# Set your working directory to the folder "***\risk budget programs" 

# This folder contains the subfolders R_allocation, R_interpretation, data, weights, etc

# Folder insample: is the insample analysis for the bondequity portfolio and for the efficient frontier

# Folder R_Allocation has the R code needed to do solve the portfolio allocation problem:

- main_riskbudgets.R: user interface through which the data, optimization criteria and constraints are identified and the allocation functions are called.

- risk_budget_functions.R: has the functions for the portfolio allocation

- estimators.R: has the portfolio risk estimators. 

# Folder R_interpretation has the R code needed for the analysis of the optimized portfolios:

- stackedweightriskcontributionplot_oct.R: produces a 2x2 plot with the weights and risk allocation of the investment size and risk budget constrained portfolios.
  uses functions in chart.StackedBar.R
- performanceanalysis_oct.R: studies the out-of-sample returns (uses functions in pfolioreturn.R to obtain returns that account for compounding)


# Folder data has the data 

# Folder weights has the weights for the optimized portfolios 

# Folder riskcont has the percentage risk contributions for the optimized portfolios 