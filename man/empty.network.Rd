\name{empty.network}
\alias{empty.network}
\title{Empty Network}
\usage{
  empty.network(nodes)
}
\arguments{
  \item{nodes}{Names of nodes (character vector)}
}
\value{
  \code{mhnet} network with no edges
}
\description{
  Create a network with given nodes and no edges.
}
\examples{
empty.network(LETTERS[1:4])
}
\seealso{
  \code{\link{remove.edge}}, \code{\link{reverse.edge}},
  \code{\link{add.edge}}
}
\keyword{manip}

