% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/plot.mhnet.R
\name{plot.mhnet}
\alias{plot.mhnet}
\title{Plot the Graph}
\usage{
\method{plot}{mhnet}(x, ...)
}
\arguments{
\item{x}{\code{mhnet} network}

\item{...}{Other graphical parameters}
}
\value{
\code{NULL}
}
\description{
Convert \code{mhnet} network to \code{bnlearn} object and plot it
}
\examples{
nw <- empty.network(LETTERS[1:4])
nw <- add.edge(nw, "A", "D")
plot(nw)
}
\seealso{
\code{\link{matrix2strength}}
}
\keyword{hplot}

