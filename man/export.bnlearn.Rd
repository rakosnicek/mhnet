\name{export.bnlearn}
\alias{export.bnlearn}
\title{Export Bnlearn Network}
\usage{
  export.bnlearn(nw)
}
\arguments{
  \item{nw}{\code{mhnet} network}
}
\value{
  \code{bnlearn} object
}
\description{
  Convert \code{mnnet} network into \code{bnlearn} object.
}
\examples{
nw <- empty.network(LETTERS[1:4])
export.bnlearn(nw)
}
\seealso{
  \code{\link{import.bnlearn}}
}
\keyword{manip}

