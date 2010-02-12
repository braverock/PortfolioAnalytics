\name{risk_budget_objective}
\alias{risk_budget_objective}
\title{constructor for class risk_budget_objective...}
\usage{risk_budget_objective(assets, name, target, arguments, multiplier=1, enabled=FALSE, ..., min_prisk, max_prisk)}
\description{constructor for class risk_budget_objective}
\author{Brian G. Peterson}
\arguments{\item{assets}{vector of assets to use, should come from constraints object}
\item{name}{name of the objective, should correspond to a function, though we will try to make allowances}
\item{target}{univariate target for the objective}
\item{arguments}{default arguments to be passed to an objective function when executed}
\item{multiplier}{multiplier to apply to the objective, usually 1 or -1}
\item{enabled}{TRUE/FALSE}
\item{\dots}{any other passthru parameters}
\item{min_prisk}{minimum percentage contribution to risk}
\item{max_prisk}{maximum percentage contribution to risk}}
