README
================

This V2.1 version of PortfolioAnalytics is an update to the substantial
V2.0 version that was released on 2024-07-03. We first describe the V2.0
features, then discuss the R demo capability, and finally we describe
the additional V2.1 features.

# 2.0 Features

A major feature of 2.0 was the integration of the CVXR solver R package
for convex optimization. CVXR supports eleven solver packages, each of
which supports solvers for one or more of the following optimization
problems: LP, QP, SOCP, SDP, EXP, MIP. See the Table near the beginning
of the document “Convex Optimization in R” at <https://cvxr.rbind.io/>.
Thus, with PortfolioAnalytics 2.0, users are able to use any one of a
large variety of solvers available in CVXR for their portfolio optimization
problems.

A particular use of CVXR in PortfolioAnalytics 2.0 is for computing
Minimum Coherent Second Moment (MCSM) portfolios, which are
second-order cone programming (SOCP) optimization problems. This is
a quite new capability that is not available in other portfolio
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

Two New Vignettes, the pdf files of which are downloadable from PortfolioAnalytics at
<https://cran.r-project.org/web/packages/PortfolioAnalytics/index.html>:
1.  cvxrPortfolioAnalytics, with CRAN title “CVXR for PortfolioAnalytics”.
2.  robustCovMatForPA, with CRAN title “Robust Covariance Matrices for
    PortfolioAnalytics”

# PortfolioAnalytics Demo Scripts
PortfolioAnalytics has contained a substantial number of demo R scripts in the *demo* folder for a long time.  Assuming that an R package is installed, but not necessarily loaded, you can view a list of the names of al the demo folder R scripts with the following R command

demo(package = “packageName”)

Use the above for the PortfolioAnalytics package, and you will see a list of over 30 demo scripts,
among which you will see the the following two demo scripts

1.  demo_cvxrPortfolioAnalytics.R
2.  demo_robustCovMatForPA.R

which runs the code for the corresponding two Vignettes listed above.

You can view the code for any demo R script with the command “??” (but not with “help” command).  For example, in RStudio, use of the command

?? demo_cvxrPortfolioAnalytics

results in a Help tab display with the followng two links:

* *PortfolioAnalytics::demo_cvxrPortfolioAnalytics*
* (*Run demo*).

Use the first link, which results in a display of the entire R script in the Help tab.  Then copy/paste the script into your own new R file, and run it in chunks that are of interest to you. Many of the chunks will run quite quickly, e.g., a few seconds, but a few of them may take 2-4 minutes. Doing so for the demo_cvxrPortfolioAnalytics.R will help you learn some PortfolioAnalytics basics, as well as learn how to use new capabilities in Versions 2.0 and 2.1. Running the demo_robustCovMatForPA.R scripts will show you how to compute *robust minimum variance* portfolios based on returns *robust covariance matrix estimators* that are not much influenced by returns outliers.

NOTE: We do not recommend general use of (*Run demo*). This is because it runs the entire demo script, which will often take much too long. Furthermore, some scripts may fail to execute properly when run this way. That said, the (*Run demo*) link can be handy for running R demo scripts that execute quickly.

# New 2.1 Features

The 2.1 version of PortfolioAnalytics contains the following new demo scripts:

1. demo_JPM2024MinDownsideRisk.R
2. demo_JPM2024MinDownsideRiskCVXR.R

The first script replicates all the Exhibits (Figures and Tables) in the Journal of Portfolio Management paper “Minimum Downside Risk Portfolios", published in October 2024. This first script uses CVXR package solvers “under the hood”, i.e., not directly visible, in PortfolioAnalytics.  The second script replicates just the back-test results in Exhibits 6, 8, 10, 12, 14, 16, 18, in the above paper, but it uses the CVXR code directly in the script, where on can easily see the CVXR code details.

The 2.1 release also contains:

1. Extended functionalities for graphical displays of multiple efficient frontiers, and robust covariance estimator settings
2. The term EQS for *expected quadratic shortfall* was replaced with CSM for *coherent second moment* risk.
3. Updates to the vignettes cvxrPortfolioAnalytics and robustCovMatForPA, and their demo scripts.

# Bug Reporting

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
