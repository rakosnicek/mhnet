\name{matrix2strength}
\alias{matrix2strength}
\title{Matrix to Strength}
\usage{
  matrix2strength(A, nodes)
}
\arguments{
  \item{A}{Matrix with edge probability (occurence)}

  \item{nodes}{Names of nodes (ordering correspond to A's
  rows and columns)}
}
\value{
  \code{bn.strength} object
}
\description{
  Covert matrix with edge probabilities to
  \code{bn.strength} object.
}
\examples{
rg <- tabu(gaussian.test)
nw <- import.bnlearn(rg)

all.edges <- function(nw) nw$adjmat
A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))
}
\keyword{manip}

