\name{constraint_ROI}
\alias{constraint_ROI}
\title{constructor for class constraint_ROI}
\usage{
  constraint_ROI(assets = NULL, op.problem,
    solver = c("glpk", "quadprog"), weight_seq = NULL)
}
\arguments{
  \item{assets}{number of assets, or optionally a named
  vector of assets specifying seed weights}

  \item{op.problem}{an object of type "OP" (optimization
  problem, of \code{ROI}) specifying the complete
  optimization problem, see ROI help pages for proper
  construction of OP object.}

  \item{solver}{string argument for what solver package to
  use, must have ROI plugin installed for that solver.
  Currently support is for \code{glpk} and
  \code{quadprog}.}

  \item{weight_seq}{seed sequence of weights, see
  \code{\link{generatesequence}}}
}
\description{
  constructor for class constraint_ROI
}
\author{
  Hezky Varon
}

