\name{extractGroups}
\alias{extractGroups}
\title{Extract the group and/or category weights}
\usage{
  extractGroups(object, ...)
}
\arguments{
  \item{object}{object of class \code{optimize.portfolio}}

  \item{...}{passthrough parameters. Not currently used}
}
\value{
  a list with two elements \itemize{ \item{weights:
  }{Optimal set of weights from the
  \code{optimize.portfolio} object} \item{category_weights:
  }{Weights by category if category_labels are supplied in
  the \code{portfolio} object} \item{group_weights:
  }{Weights by group if group is a constraint type} }
}
\description{
  This function extracts the weights by group and/or
  category from an object of class
  \code{optimize.portfolio}. Group constraints or
  category_labels must be specified for this to return
  group constraints.
}
\author{
  Ross Bennett
}

