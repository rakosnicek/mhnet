\name{log.prior}
\alias{log.prior}
\title{Prior Distribution over Graphs}
\usage{
  log.prior(nw)
}
\arguments{
  \item{nw}{\code{mhnet} network}
}
\value{
  real number
}
\description{
  Log-Prior (up to a constant) on a space of directed
  acyclic graphs
}
\examples{
nw <- empty.network(LETTERS[1:4])
exp(mhnet:::log.prior(nw))
}
\seealso{
  \code{\link{one.step}}
}
\keyword{manip}

