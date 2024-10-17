README
================

This V2.1 version of PortfolioAnalytics is an update to the substantial
V2.0 version that was released on 2024-07-03. We first describe the V2.0
features, then discuss the R demo capability, and finally we describe
the additional V2.1 features.

# 2.0 Features

A major feature of 2.0 is the integration of the CVXR solver R package
for convex optimization. CVXR supports eleven solver packages, each of
which supports solvers for one or more of the following optimization
problems: LP, QP, SOCP, SDP, EXP, MIP. See the Table near the beginning
of the document “Convex Optimization in R” at <https://cvxr.rbind.io/>.
Thus, with PortfolioAnalytics 2.0, users are able to use any one of a
variety of solvers available in CVXR for their portfolio optimization
problems.

A particular use of CVXR in PortfolioAnalytics 2.0 is for computing
Minimum Coherent Second Moment (MinCSM) portfolios, which are
second-order cone programming (SOCP) optimization problems. This is
quite a new capability that is not available in other portfolio
optimization software products. Details are provided in the Vignette
“cvxrPortfolioAnalytics”.

Another important feature of PortfolioAnalytics 2.0, is that it contains
functionality for computing outliers-robust minimum variance (MV)
optimal portfolios based on any one of several robust covariance matrix
estimators that are not much influenced by outliers Details are provided
in the Vignette “robustCovMatForPA”.

New PortfolioAnalytics Functions:

1.  meancsm.efficient.frontier (create Mean-CSM efficient frontier)
    utility function
2.  meanrisk.efficient.frontier (generate multiple efficient frontiers
    for portfolios with the same constraint object.
3.  extract_risk (extract the risk value, e.g., StdDev or ES or CSM,
    based on the weights of a portfolio)
4.  chart.EfficientFrontierCompare (Overlay the efficient frontiers of
    different minRisk portfolio objects on a single plot)
5.  backtest.plot (based on Peter Carl’s code, generate plots of the
    cumulative returns and/or drawdown for back-testing)
6.  opt.outputMvo (converts output of `optimize.portfolio` to a list of
    the portfolio weights, mean, volatility and Sharpe Ratio)
7.  plotFrontiers (plot frontiers based on the result of
    `meanvar.efficient.frontier`, `meanetl.efficient.frontier` or
    `meancsm.efficient.frontier`)

Enhanced PortfolioAnalytics Functions:

1.  optimize.portfolio (enhanced with CVXR solvers, CSM objective,
    customizable arg `momentFUN=` and output `~$moment_values`)
2.  optimize.portfolio.rebalancing (enhanced with CVXR solvers, CSM
    objective and customizable arg `momentFUN=`)
3.  create.EfficientFrontier (enhanced with type `mean-CSM` and
    `mean-risk`, and customizable arg `momentFUN=`)

Support of S3 Methods for CVXR:
1.  print.optimize.portfolio.CVXR
2.  extractStats.optimize.portfolio.CVXR

Custom Moment Functions for Robust Covariance Matrices:
1.  custom.covRob.MM
2.  custom.covRob.Rocke
3.  custom.covRob.Mcd
4.  custom.covRob.TSGS
5.  MycovRobMcd
6.  MycovRobTSGS

Two Vignette pdf files downloadable from PortfolioAnalytics at CRAN:
<https://cran.r-project.org/web/packages/PortfolioAnalytics/index.html>:
1.  cvxrPortfolioAnalytics, with CRAN title “CVXR for PortfolioAnalytics”.
2.  robustCovMatForPA, with CRAN title “Robust Covariance Matrices for
    PortfolioAnalytics”

# PortfolioAnalytics Demo Scripts
PortfolioAnalytics has contained a substantial number of demo R scripts in the *demo* folder for a long time.  Assuming that an R package is installed, but not necessarily loaded, you can view a list of the names of al the demo folder R scripts with the following R command

demo(package = “packageName”)

which you can easily verify for the case of PortfolioAnalytics.
Among the long list of demo scripts, you will see the two demo scripts:

1.  demo_cvxrPortfolioAnalytics.R
2.  demo_robustCovMatForPA.R

You can view the documentation (man pages) for any demo R script with the command “??” (but not with “help” command).  For example, in RStudio, use of the command

?? demo_cvxrPortfolioAnalytics

results in a Help tab display with the followng two links:

*PortfolioAnalytics::demo_cvxrPortfolioAnalytics* and (*Run demo*). We recommend to only use the first link, which results in a display of the entire R script in the Help tab.  You should copy/paste the script into your own new R file, and run it in chunks that are of interest to you. We recommend doing this for the demo_cvxrPortfolioAnalytics.R and demo_robustCovMatForPA.R scripts.

NOTE: We do not recommend running the entire scripts above because they take a fairly long time, and this is the case for many of the demo scripts in PortfoloAnalytics. For this reason we do not recommend general use of the (*Run demo*) script, as well as for the further reason that some scripts fails to execute properly when run this way. On the other hand the (*Run demo*) link is handy for R demo scripts that run quickly.

# New 2.1 Features

This version contains the following new demo scripts:

1. demo_JPM2024MinDownsideRisk.R
2. demo_JPM2024MinDownsideRiskCVXR.R

The first script above replicates all the Exhibits (Figures and Tables) in the Journal of Portfolio Management paper “Minimum Downside Risk Portfolios, published in October 2024.  This first script uses CVXR optimization methods “under the hood” in PortfolioAnalytics.  The second script replicates just the back-test in Exhibits 6, 8, 10, 12, 14, 16, 18, in the above paper, and reveals the CVXR code directly.  


# Bug Reportin

Please contribute with bug fixes, comments, and testing scripts!

Please take your data and disguise it when submitting, or use data sets
like “edhec” like we do in the demos or or like “stocksCRSP” and
“factorsSPGMI” in the PCRA package or with your constraints and other
objectives modified to demonstrate your problem on public data.

Please report any bugs or issues on the PortfolioAnalytics GitHub page
at <https://github.com/braverock/PortfolioAnalytics/issues>

# Acknowledgements

The bulk of the work in creating PortfolioAnalytics 2.0 was done by
Xinran Zhao, along with contributions from Yifu Kang, under the support
of a 2022 Google Summer of Code (GSOC 2022). Xinran and Yifu were
mentored in GSOC 2022 by Professor Doug Martin and Professor Steve
Murray in the Applied Mathematics Department at the University of
Washington.
