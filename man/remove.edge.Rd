\name{remove.edge}
\alias{remove.edge}
\title{An Edge Removal}
\usage{
  remove.edge(nw, a, b)
}
\arguments{
  \item{nw}{\code{mhnet} network}

  \item{a}{First vertex (an arrow pointing from)}

  \item{b}{Second vertex (an arrow pointing from)}
}
\value{
  \code{mhnet} network
}
\description{
  Remove a directed edge between two vertices.
}
\examples{
nw <- empty.network(LETTERS[1:4])
nw <- add.edge(nw, "A", "D")
nw <- remove.edge(nw, "A", "D")
try(remove.edge(nw, "A", "C"))
}
\seealso{
  \code{\link{add.edge}}, \code{\link{reverse.edge}}
}
\keyword{manip}

