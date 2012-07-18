\name{HierarchyQuintiles}
\alias{HierarchyQuintiles}
\title{replaces numeric values in the hierarchy by quintiles}
\usage{
  HierarchyQuintiles(h, level)
}
\arguments{
  \item{h}{data.frame with portfolio hierarchy}

  \item{level}{level from the hierarchy for which there are
  numeric values}
}
\description{
  Replaces numeric values in the hierarchy by text with
  corresponding quintiles
}
\examples{
data(attrib)
HierarchyQuintiles(h = attrib.hierarchy, "MarketCap")
}
\author{
  Andrii Babii
}
\seealso{
  \code{buildHierarchy}
}
\keyword{attribution,}
\keyword{hierarchy}

