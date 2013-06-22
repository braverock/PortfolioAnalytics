\name{add.constraint}
\alias{add.constraint}
\title{General interface for adding and/or updating optimization constraints, currently supports weight, box and group constraints.}
\usage{
  add.constraint(portfolio, type, enabled = FALSE, ...,
    indexnum = NULL)
}
\arguments{
  \item{portfolio}{an object of class 'portfolio' to add
  the constraint to, specifying the constraints for the
  optimization, see \code{\link{portfolio.spec}}}

  \item{type}{character type of the constraint to add or
  update, currently 'weight_sum', 'box', or 'group'}

  \item{enabled}{TRUE/FALSE}

  \item{\dots}{any other passthru parameters to specify box
  and/or group constraints}

  \item{indexnum}{if you are updating a specific
  constraint, the index number in the $objectives list to
  update}
}
\description{
  This is the main function for adding and/or updating
  constraints in an object of type \code{\link{portfolio}}.
}
\details{
  In general, you will define your constraints as one of
  three types: 'weight_sum', 'box', or 'group'.
}
\author{
  Ross Bennett
}
\seealso{
  \code{\link{constraint}}
}

