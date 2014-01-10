\name{BICcovar}
\alias{BICcovar}
\title{BIC with covariates}
\usage{
  BICcovar(nw, dataset)
}
\arguments{
  \item{nw}{\code{mhnet} network}

  \item{dataset}{A data frame to use}
}
\value{
  real number
}
\description{
  Yet to be edited and example given
}
\examples{
example <- read.csv("http://lion.img.cas.cz/user/simecek/chaos/Gm17403.csv")

data <- example[,5:7]
covs <- list(Sex = cbind(as.numeric(example$sex=="M")),
             Diet = cbind(as.numeric(example$diet=="HF")),
             Batch = cbind(as.numeric(example$batch==1)))
attr(data, "covs") <- covs
nw.start <- empty.network(c(names(attr(data, "covs")),names(data)))
BICcovar(nw.start, data)
blacklist = expand.grid(a=1:6, b=1:3)
all.edges <- function(nw) nw$adjmat
A <- mcmc(nw.start, data, all.edges, verbose=FALSE, trace=FALSE, score=BICcovar, blacklist=blacklist)
}
\keyword{manip}

