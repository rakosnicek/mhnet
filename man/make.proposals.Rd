\name{make.proposals}
\alias{make.proposals}
\title{Make Proposals for Graph Modification}
\usage{
  make.proposals(nw, blacklist = NULL, whitelist = NULL)
}
\arguments{
  \item{nw}{A \code{mhnet} network}

  \item{blacklist}{2-column matrix or data.frame with
  forbidden edges (vertices as numbers)}

  \item{whitelist}{2-column matrix or data.frame with
  compulsory edges (vertices as numbers)}
}
\value{
  Three column matrix. First column code for a type of
  modification (\code{1} is removal, \code{2} is addition,
  \code{3} is reversal).

  Second and Third columns define edges to be modified.
}
\description{
  Add a directed edge between two vertices and check for
  acycliclity.
}
\examples{
rg <- bnlearn::tabu(learning.test)
nw <- import.bnlearn(rg)
make.proposals(nw)
make.proposals(nw, blacklist=data.frame(a=1:5,b=6), whitelist=data.frame(a=1,b=c(2,4)))
}
\keyword{manip}

