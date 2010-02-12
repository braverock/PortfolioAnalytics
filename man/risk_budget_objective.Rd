\name{risk_budget_objective}
\alias{risk_budget_objective}
\title{constructor for class risk_budget_objective...}
\usage{risk_budget_objective(assets, name, arguments, enabled=FALSE, ..., multiplier=1, target, p=0.95, min_prisk, max_prisk)}
\description{constructor for class risk_budget_objective}
\author{Brian G. Peterson}
\arguments{\item{assets}{vector of assets to use, should come from constraints object}
\item{name}{name of the objective, should correspond to a function, though we will try to make allowances}
\item{arguments}{default arguments to be passed to an objective function when executed}
\item{enabled}{TRUE/FALSE}
\item{\dots}{any other passthru parameters}
\item{multiplier}{multiplier to apply to the objective, usually 1 or -1}
\item{target}{univariate target for the objective}
\item{p}{confidence level for calculation, default p=.95}
\item{min_prisk}{minimum percentage contribution to risk}
\item{max_prisk}{maximum percentage contribution to risk}}
