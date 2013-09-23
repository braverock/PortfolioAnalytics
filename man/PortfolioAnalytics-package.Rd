\name{PortfolioAnalytics-package}
\alias{PortfolioAnalytics-package}
\alias{PortfolioAnalytics}
\docType{package}
\title{
Numeric methods for optimization of portfolios
}

\description{
\kbd{PortfolioAnalytics} provides an\R packaged to provide numerical solutions for portfolio problems with complex constraints and objective sets. The goal of the package is to aid practicioners and researchers in solving portfolio optimization problems with complex constraints and objectives that mirror real-world applications.

One of the goals of the packages is to provide a common interface to specify constraints and objectives that can be solved by any supported solver (i.e. optimization method). Currently supported optimization methods include random portfolios, differential evolution, particle swarm optimization, generalized simulated annealing, and linear and quadratic programming routines. Additional information on random portfolios is provided below. The differential evolution algorithm is implemented via the \kbd{DEoptim} package, the particle swarm optimization algorithm via the \kbd{pso} package, the generalized simulated annealing via the \kbd{GenSA} package, and linear and quadratic programming are implemented via the \kbd{ROI} package which acts as an interface to the \kbd{Rglpk} and \kbd{quadprog} packages.

A key strength of \kbd{PortfolioAnalytics} is the generalization of constraints and objectives that can be solved by any available optimization methods. The quadratic and linear programming solvers can solve a limited type of convex optimization problems.
\itemize{
  \item Maxmimize portfolio return subject leverage, box, group, position limit, target mean return, and/or factor exposure constraints on weights.
  \item Minimize portfolio variance subject to leverage, box, group, turnover, and/or factor exposure constraints (otherwise known as global minimum variance portfolio).
  \item Minimize portfolio variance subject to leverage, box, group, and/or factor exposure constraints and a desired portfolio return.
  \item Maximize quadratic utility subject to leverage, box, group, target mean return, turnover, and/or factor exposure constraints and risk aversion parameter.
  \item Minimize ETL subject to leverage, box, group, position limit, target mean return, and/or factor exposure constraints and target portfolio return.
}

Many real-world portfolio optimization problems are 'global optimization' problems, and therefore are not suitable for linear or quadratic programming routines. \kbd{PortfolioAnalytics} provides a random portfolio optimization method, and also utilizes the \R packages DEoptim, pso, and GenSA for solving non-convex global optimization problems. \kbd{PortfolioAnalytics} supports three methods of generating random portfolios.

\itemize{
  \item The ’sample’ method to generate random portfolios is based on an idea by Pat Burns. This is the most flexible method, but also the slowest, and can generate portfolios to satisfy leverage, box, group, and position limit constraints.
  \item The ’simplex’ method to generate random portfolios is based on a paper by W. T. Shaw. The simplex method is useful to generate random portfolios with the full investment constraint, where the sum of the weights is equal to 1, and min box constraints. Values for min_sum and max_sum of the leverage constraint will be ignored, the sum of weights will equal 1. All other constraints such as the box constraint max, group and position limit constraints will be handled by elimination. If the constraints are very restrictive, this may result in very few feasible portfolios remaining. Another key point to note is that the solution may not be along the vertexes depending on the objective. For example, a risk budget objective will likely place the portfolio somewhere on the interior.
  \item The ’grid’ method to generate random portfolios is based on the \code{gridSearch} function in package \kbd{NMOF}. The grid search method only satisfies the min and max box constraints. The min_sum and max_sum leverage constraint will likely be violated and the weights in the random portfolios should be normalized. Normalization may cause the box constraints to be violated and will be penalized in constrained_objective.
}

\kbd{PortfolioAnalytics} leverages the \kbd{PerformanceAnalytics} package for many common objective functions. The objective types in \kbd{PortfolioAnalytics} are designed to be used with \kbd{PerformanceAnalytics} functions, but any user supplied valid R function can be used as an objective.

This summary attempts to provide an overview of how to construct a portfolio object with constraints and objectives, and then run the optimization.
}

\section{Optimization}{
The portfolio object is instantiated with the \code{\link{portfolio.spec}} function. The main argument to \code{\link{portfolio.spec}} is \code{assets}. The \code{assets} argument can be a scalar value for the number of assets, a character vector of fund names, or a named vector of initial weights.

Adding constraints to the portfolio object is done with \code{\link{add.constraint}}. The \code{\link{add.constraint}} function is the main interface for adding and/or updating constraints to the portfolio object. This function allows the user to specify the portfolio to add the constraints to, the type of constraints, arguments for the constraint, and whether or not to enable the constraint. If updating an existing constraint, the indexnum argument can be specified.

Objectives can be added to the portfolio object with \code{\link{add.objective}}. The \code{\link{add.objective}} function is the main function for adding and/or updating objectives to the portfolio object. This function allows the user to specify the portfolio to add the objectives to, the type, name of the objective function, arguments to the objective function, and whether or not to enable the objective. If updating an existing constraint, the indexnum argument can be specified.

With the constraints and objectives specified in the portfolio object, the portfolio object can be passed to \code{\link{optimize.portfolio}} or \code{\link{optimize.portfolio.rebalancing}} to run the optimization. Arguments to \code{\link{optimize.portfolio}} include asset returns, the portfolio obect specifying constraints and objectives, optimization method, and other parameters specific to the solver. \code{\link{optimize.portfolio.rebalancing}} adds support for backtesting portfolio optimization through time with rebalancing or rolling periods.
}

\section{Charts and Graphs}{
TODO
}

\section{Further Work}{
TODO
}

\section{Acknowledgements}{
TODO
}

\section{References}{
TODO
W.T. Shaw Paper
Modern Portfolio Optimization
Large-scale portfolio optimization with DEoptim 
http://cran.r-project.org/web/packages/DEoptim/vignettes/DEoptimPortfolioOptimization.pdf
}

\section{See Also}{
TODO
}