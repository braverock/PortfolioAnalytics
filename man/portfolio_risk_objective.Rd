\name{portfolio_risk_objective}
\alias{portfolio_risk_objective}
\title{constructor for class portfolio_risk_objective...}
\usage{portfolio_risk_objective(name, enabled=FALSE, ..., multiplier=1, target, p=0.95)}
\description{constructor for class portfolio_risk_objective}
\author{bpeterson}
\arguments{\item{name}{name of the objective, should correspond to a function, though we will try to make allowances}
\item{enabled}{TRUE/FALSE}
\item{\dots}{any other passthru parameters}
\item{multiplier}{multiplier to apply to the objective, usually 1 or -1}
\item{target}{univariate target for the objective}
\item{p}{confidence level for calculation, default p=.95}}
