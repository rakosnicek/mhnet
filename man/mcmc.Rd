% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/mcmc.R
\name{mcmc}
\alias{mcmc}
\title{Metropolis-Hastings Simulation}
\usage{
mcmc(start.nw, dataset, reporter, Nsim = 1000, burn = 0.1, verbose = TRUE,
  trace = FALSE, score = BICbnnet, whitelist = NULL, blacklist = NULL,
  lprior = log.prior, move.score = NULL, move.lprior = NULL,
  move.proposals = NULL)
}
\arguments{
\item{start.nw}{Starting network}

\item{dataset}{Data frame with observations}

\item{reporter}{Statistic to be generated}

\item{Nsim}{Number of simulations}

\item{burn}{Proportion of simulations to be used as a burn-in}

\item{verbose}{If TRUE then report messanges and plots will be given}

\item{trace}{If TRUE then all \code{reporter} values are given, not just the average}

\item{score}{A function to calculate BIC}

\item{whitelist}{For move proposal, 2-column matrix or data.frame with compulsory edges (vertices as numbers)}

\item{blacklist}{For move proposal, 2-column matrix or data.frame with forbidden edges (vertices as numbers)}

\item{lprior}{Log of prior distribution of acyclic directed graph}

\item{move.score}{A function to adjust the score to addition / removal / reversal of an edge}

\item{move.lprior}{A function to adjust the log-prior to addition / removal / reversal of an edge}

\item{move.proposals}{A function to adjust the proposals to addition / removal / reversal of an edge}
}
\value{
Either average or all \code{reporter} values based on \code{trace} settings.
}
\description{
Perform Metropolis-Hastings simulation over space of directed acyclic graphs.
}
\examples{
rg <- tabu(gaussian.test)
nw <- import.bnlearn(rg)

number.of.edges <- function(nw) sum(nw$adjmat)
ne.dist <- mcmc(nw, gaussian.test, number.of.edges, verbose=FALSE, trace=TRUE)
table(do.call("c", ne.dist$trace)[101:1000]) / 900

all.edges <- function(nw) nw$adjmat
A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))
}
\keyword{models}

