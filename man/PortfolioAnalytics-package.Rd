\name{PortfolioAnalytics-package}
\alias{PortfolioAnalytics-package}
\alias{PortfolioAnalytics}
\docType{package}
\title{
Numeric methods for optimization of portfolios
}

\description{
\kbd{PortfolioAnalytics} is an \R package to provide numerical solutions for portfolio problems with complex constraints and objective sets. The goal of the package is to aid practicioners and researchers in solving portfolio optimization problems with complex constraints and objectives that mirror real-world applications.

One of the goals of the packages is to provide a common interface to specify constraints and objectives that can be solved by any supported solver (i.e. optimization method). Currently supported optimization methods include
\itemize{
  \item random portfolios
  \item differential evolution
  \item particle swarm optimization
  \item generalized simulated annealing
  \item linear and quadratic programming routines
}

The solver can be specified with the \code{optimize_method} argument in \code{optimize.portfolio} and \code{optimize.portfolio.rebalancing}. The \code{optimize_method} argument must be one of "random", "DEoptim", "pso", "GenSA", "ROI", "quadprog", "glpk", or "symphony".

Additional information on random portfolios is provided below. The differential evolution algorithm is implemented via the \kbd{DEoptim} package, the particle swarm optimization algorithm via the \kbd{pso} package, the generalized simulated annealing via the \kbd{GenSA} package, and linear and quadratic programming are implemented via the \kbd{ROI} package which acts as an interface to the \kbd{Rglpk}, \kbd{Rsymphony}, and \kbd{quadprog} packages.

A key strength of \kbd{PortfolioAnalytics} is the generalization of constraints and objectives that can be solved.

If \code{optimize_method="ROI"} is specified, a default solver will be selected based on the optimization problem. The \code{glpk} solver is the default solver for LP and MILP optimization problems. The \code{quadprog} solver is the default solver for QP optimization problems. For example, \code{optimize_method = "quadprog"} can be specified and the optimization problem will be solved via ROI using the quadprog plugin package.

The extension to ROI solves a limited type of convex optimization problems:
\itemize{
\item{Maxmimize portfolio return subject leverage, box, group, position limit, target mean return, and/or factor exposure constraints on weights.}
\item{Minimize portfolio variance subject to leverage, box, group, turnover, and/or factor exposure constraints (otherwise known as global minimum variance portfolio).}
\item{Minimize portfolio variance subject to leverage, box, group, and/or factor exposure constraints and a desired portfolio return.}
\item{Maximize quadratic utility subject to leverage, box, group, target mean return, turnover, and/or factor exposure constraints and risk aversion parameter.
(The risk aversion parameter is passed into \code{optimize.portfolio} as an added argument to the \code{portfolio} object).}
\item{Maximize portfolio mean return per unit standard deviation (i.e. the Sharpe Ratio) can be done by specifying \code{maxSR=TRUE} in \code{optimize.portfolio}. 
If both mean and StdDev are specified as objective names, the default action is to maximize quadratic utility, therefore \code{maxSR=TRUE} must be specified to maximize Sharpe Ratio.}
\item{Minimize portfolio ES/ETL/CVaR optimization subject to leverage, box, group, position limit, target mean return, and/or factor exposure constraints and target portfolio return.}
\item{Maximize portfolio mean return per unit ES/ETL/CVaR (i.e. the STARR Ratio) can be done by specifying \code{maxSTARR=TRUE} in \code{optimize.portfolio}. 
If both mean and ES/ETL/CVaR are specified as objective names, the default action is to maximize mean return per unit ES/ETL/CVaR.}
}
These problems also support a weight_concentration objective where concentration of weights as measured by HHI is added as a penalty term to the quadratic objective.

Because these convex optimization problem are standardized, there is no need for a penalty term. The \code{multiplier} argument in \code{\link{add.objective}} passed into the complete constraint object are ingnored by the ROI solver.

Many real-world portfolio optimization problems are global optimization problems, and therefore are not suitable for linear or quadratic programming routines. \kbd{PortfolioAnalytics} provides a random portfolio optimization method and also utilizes the \R packages DEoptim, pso, and GenSA for solving non-convex global optimization problems.

\kbd{PortfolioAnalytics} supports three methods of generating random portfolios.
\itemize{
  \item The sample method to generate random portfolios is based on an idea by Pat Burns. This is the most flexible method, but also the slowest, and can generate portfolios to satisfy leverage, box, group, position limit, and leverage constraints.
  \item The simplex method to generate random portfolios is based on a paper by W. T. Shaw. The simplex method is useful to generate random portfolios with the full investment constraint (where the sum of the weights is equal to 1) and min box constraints. Values for min_sum and max_sum of the leverage constraint will be ignored, the sum of weights will equal 1. All other constraints such as the box constraint max, group and position limit constraints will be handled by elimination. If the constraints are very restrictive, this may result in very few feasible portfolios remaining. Another key point to note is that the solution may not be along the vertexes depending on the objective. For example, a risk budget objective will likely place the portfolio somewhere on the interior.
  \item The grid method to generate random portfolios is based on the \code{gridSearch} function in package \kbd{NMOF}. The grid search method only satisfies the min and max box constraints. The min_sum and max_sum leverage constraint will likely be violated and the weights in the random portfolios should be normalized. Normalization may cause the box constraints to be violated and will be penalized in \code{constrained_objective}.
}

\kbd{PortfolioAnalytics} leverages the \kbd{PerformanceAnalytics} package for many common objective functions. The objective types in \kbd{PortfolioAnalytics} are designed to be used with \kbd{PerformanceAnalytics} functions, but any user supplied valid \R function can be used as an objective.
}

\section{Optimization}{
This summary attempts to provide an overview of how to construct a portfolio object with constraints and objectives, run the optimization, and chart the results.

The portfolio object is initialized with the \code{\link{portfolio.spec}} function. The main argument to \code{\link{portfolio.spec}} is \code{assets}. The \code{assets} argument can be a scalar value for the number of assets, a character vector of fund names, or a named vector of initial weights.

Adding constraints to the portfolio object is done with \code{\link{add.constraint}}. The \code{\link{add.constraint}} function is the main interface for adding and/or updating constraints to the portfolio object. This function allows the user to specify the portfolio to add the constraints to, the type of constraints, arguments for the constraint, and whether or not to enable the constraint. If updating an existing constraint, the \code{indexnum} argument can be specified.

Objectives can be added to the portfolio object with \code{\link{add.objective}}. The \code{\link{add.objective}} function is the main function for adding and/or updating objectives to the portfolio object. This function allows the user to specify the portfolio to add the objectives to, the type, name of the objective function, arguments to the objective function, and whether or not to enable the objective. If updating an existing objective, the \code{indexnum} argument can be specified.

With the constraints and objectives specified in the portfolio object, the portfolio object can be passed to \code{\link{optimize.portfolio}} or \code{\link{optimize.portfolio.rebalancing}} to run the optimization. Arguments to \code{\link{optimize.portfolio}} include asset returns, the portfolio obect specifying constraints and objectives, optimization method, and other parameters specific to the solver. \code{\link{optimize.portfolio.rebalancing}} adds support for backtesting portfolio optimization through time with rebalancing or rolling periods.
}

\section{Advanced Optimization}{
In addition to the more standard optimizations described above, \kdb{PortfolioAnalytics} also supports multi-layer optimization and regime switching optimization. 

Support for multi-layer optimization allows one to construct a top level portfolio and several sub-portfolios with potentially different assets, constraints, and objectives. First, each sub-portfolio is optimized out-of-sample which creates a time series of returns. One can think of the out of sample returns for each sub-portfolio as the returns for a synthetic instrument. Finally, the out-of-sample returns of each sub-portfolio are then used as inputs for the top level optimization. The top level portfolio and sub-portfolios are created as normal using \code{portfolio.spec}, \code{add.constraint}, and \code{add.objective}. The multi-layer portfolio specification object is first initialized by passing the top level portfolio to \code{mult.portfolio.spec}. Sub-portfolios are then added with \code{add.sub.portfolio}. The multi-layer portfolio specification object can then be passed to \code{optimize.portfolio} and \code{optimize.portfolio.rebalancing}. See \code{demo(multi_layer_optimization)}.

Support for regime switching models allows one to change constraints and objectives depending on the current regime. Portfolios are created as normal with \code{portfolio.spec}, \code{add.constraint}, and \code{add.objective}. The portfolios are then combined with a regime object using \code{regime.portfolios} to create a regime portfolio specification which can then be passed to \code{optimize.portfolio} and \code{optimize.portfolio.rebalancing}. Regime switching optimization is implemented in such a way that any arbitrary regime model can be used. See \code{demo(regime_switching)}.
}

\section{Portfolio Moments}{
The \kdb{PortfolioAnalytics} framework to estimate solutions to constrained optimization problems is implemented in such a way that the moments of the returns are set once for use in lower level optimization functions. The \code{set.portfolio.moments} function computes the first, second, third, and fourth moments depending on the objective function(s) in the \code{portfolio} object. For example, if the third and fourth moments do not need to be calculated for a given objective, then \code{set.portfolio.moments} will try to detect this and not compute those moments. Currently, \code{set.portfolio.moments} implements methods to compute moments based on sample estimates, higher moments from fitting a statistical factor model based on the work of Kris Boudt, the Black Litterman model, and the Fully Flexible Framework based on the work of Attilio Meucci (NEED REFERENCE HERE). See the Custom Moment and Objective Functions vignette for a more detailed description and examples. 
}

\section{Charts and Graphs}{
Intuition into the optimization can be aided through visualization. The goal of creating the charts is to provide visualization tools for optimal portfolios regardless of the chosen optimization method.

\code{\link{chart.Weights}} plots the weights of the optimal portfolio. \code{\link{chart.RiskReward}} plots the optimal portfolio in risk-reward space. The random portfolios, DEoptim, and pso solvers will return trace portfolio information at each iteration when \code{\link{optimize.portfolio}} is run with \code{trace=TRUE}. If this is the case, \code{\link{chart.RiskReward}} will plot these portfolios so that the feasible space can be easily visualized. Although the GenSA and ROI solvers do not return trace portfolio information, random portfolios can be be generated with the argument \code{rp=TRUE} in \code{\link{chart.RiskReward}}. A \code{plot} function is provided that will plot the weights and risk-reward scatter chart. The component risk contribution can be charted for portfolio optimization problems with risk budget objectives with \code{\link{chart.RiskBudget}}. Neighbor portfolios can be plotted in \code{\link{chart.RiskBudget}}, \code{\link{chart.Weights}}, and \code{\link{chart.RiskReward}}.

Efficient frontiers can be extracted from \code{optimize.portfolio} objects or created from a \code{portfolio} object. The efficient frontier can be charted in risk-reward space with \code{\link{chart.EfficientFrontier}}. The weights along the efficient frontier can be charted with \code{\link{chart.Weights.EF}}.

Multiple objects created via \code{\link{optimize.portfolio}} can be combined with \code{\link{combine.optimizations}} for visual comparison. The weights of the optimal portfolios can be plotted with \code{\link{chart.Weights}}. The optimal portfolios can be compared in risk-reward space with \code{\link{chart.RiskReward}}. The portfolio component risk contributions of the multiple optimal portfolios can be plotted with \code{\link{chart.RiskBudget}}.
}

\section{Demos}{
\kdb{PortfolioAnalytics} contains a comprehensive collection of demos to demonstrate the functionality from very basic optimization problems such as estimating the solution to a minimum variance portfolio to more complex optimization problems with custom moment and objective functions.
}

\section{Vignettes}{
TODO
}

\section{Package Dependencies}{
Several of the functions in the \kbd{PortfolioAnalytics} package require time series data of returns and the \code{\link[xts]{xts}} package is used for working with time series data. 

The \kbd{PerformanceAnalytics} package is used for many common objective functions. The objective types in \kbd{PortfolioAnalytics} are designed to be used with \kbd{PerformanceAnalytics} functions such as \code{\link[PerformanceAnalytics]{StdDev}}, \code{\link[PerformanceAnalytics]{VaR}}, and \code{\link[PerformanceAnalytics]{ES}}.

The \kbd{foreach} and \kbd{iterators} packages are used extensively throughout the package to support parallel programming. The primary functions where \code{foreach} loops are used is \code{\link{optimize.portfolio}}, \code{\link{optimize.portfolio.rebalancing}}, and \code{\link{create.EfficientFrontier}}.

In addition to a random portfolios optimzation method, \kbd{PortfolioAnalytics} supports backend solvers by leveraging the following packages: \kbd{DEoptim}, \kbd{pso}, \kbd{GenSA}, \kbd{ROI} and associated ROI plugin packages.
}

\section{Further Work}{
Continued work to improved charts and graphs.

Continued work to improve features to combine and compare multiple optimal portfolio objects.

Support for more solvers.

Comments, suggestions, and/or code patches are welcome.
}

\author{
Ross Bennett \cr
Kris Boudt \cr
Peter Carl \cr
Brian G. Peterson \cr

Maintainer: Brian G. Peterson \email{brian@braverock.com}
}

\references{
Boudt, Kris and Lu, Wanbo and Peeters, Benedict, \emph{Higher Order Comoments of Multifactor Models and Asset Allocation} (June 16, 2014). Available at SSRN: http://ssrn.com/abstract=2409603 or http://dx.doi.org/10.2139/ssrn.2409603

Chriss, Neil A and Almgren, Robert, \emph{Portfolios from Sorts} (April 27, 2005). Available at SSRN: http://ssrn.com/abstract=720041 or http://dx.doi.org/10.2139/ssrn.720041 \cr

Meucci, Attilio, \emph{The Black-Litterman Approach: Original Model and Extensions} (August 1, 2008). Shorter version in, THE ENCYCLOPEDIA OF QUANTITATIVE FINANCE, Wiley, 2010. Available at SSRN: http://ssrn.com/abstract=1117574 or http://dx.doi.org/10.2139/ssrn.1117574 \cr

Meucci, Attilio, \emph{Fully Flexible Views: Theory and Practice} (August 8, 2008). Fully Flexible Views: Theory and Practice, Risk, Vol. 21, No. 10, pp. 97-102, October 2008. Available at SSRN: http://ssrn.com/abstract=1213325

Scherer, Bernd and Martin, Doug, \emph{Modern Portfolio Optimization}. Springer. 2005. \cr

Shaw, William Thornton, \emph{Portfolio Optimization for VAR, CVaR, Omega and Utility with General Return Distributions: A Monte Carlo Approach for Long-Only and Bounded Short Portfolios with Optional Robustness and a Simplified Approach to Covariance Matching} (June 1, 2011). Available at SSRN: http://ssrn.com/abstract=1856476 or http://dx.doi.org/10.2139/ssrn.1856476 \cr

}

\section{Acknowledgements}{
TODO
}

\seealso{
CRAN task view on Empirical Finance \cr \url{http://cran.r-project.org/src/contrib/Views/Econometrics.html}

CRAN task view on Optimization \cr \url{http://cran.r-project.org/web/views/Optimization.html}

Large-scale portfolio optimization with DEoptim \cr \url{http://cran.r-project.org/web/packages/DEoptim/vignettes/DEoptimPortfolioOptimization.pdf}
}