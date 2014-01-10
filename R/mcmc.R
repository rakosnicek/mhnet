#' Metropolis-Hastings Simulation
#' 
#' Perform Metropolis-Hastings simulation over space of directed acyclic graphs. 
#'
#' @param start.nw  Starting network
#' @param dataset Data frame with observations
#' @param reporter Statistic to be generated 
#' @param Nsim Number of simulations
#' @param burn Proportion of simulations to be used as a burn-in
#' @param verbose If TRUE then report messanges and plots will be given
#' @param trace If TRUE then all \code{reporter} values are given, not just the average
#' @param score A function to calculate BIC
#' @param blacklist For move proposal, 2-column matrix or data.frame with forbidden edges (vertices as numbers)
#' @param whitelist For move proposal, 2-column matrix or data.frame with compulsory edges (vertices as numbers)
#' @param lprior Log of prior distribution of acyclic directed graph
#' @param move.score A function to adjust the score to addition / removal / reversal of an edge
#' @param move.lprior A function to adjust the log-prior to addition / removal / reversal of an edge
#' @param move.proposals A function to adjust the proposals to addition / removal / reversal of an edge
#' 
#' @return Either average or all \code{reporter} values based on \code{trace} settings.
#'
#' @keywords models
#'
#' @export 
#' 
#' @examples
#' 
#' rg <- tabu(gaussian.test)
#' nw <- import.bnlearn(rg)
#' 
#' number.of.edges <- function(nw) sum(nw$adjmat)
#' ne.dist <- mcmc(nw, gaussian.test, number.of.edges, verbose=FALSE, trace=TRUE)
#' table(do.call("c", ne.dist$trace)[101:1000]) / 900
#'  
#' all.edges <- function(nw) nw$adjmat
#' A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
#' strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))
 
mcmc <- function(start.nw, dataset, reporter, Nsim=1000, burn=0.1, verbose=TRUE, trace=FALSE, 
                 score = BICbnnet, whitelist = NULL, blacklist = NULL, 
                 lprior=log.prior, move.score=NULL, move.lprior=NULL, move.proposals=NULL) {
  nw <- start.nw
  maxburn <- round(burn*Nsim)
  
  output <- reporter(start.nw) * 0
  if (trace) collect <- list()
  
  for (i in 1:Nsim) {
    nw <- one.step(nw, dataset, verbose, score, whitelist, blacklist, lprior, move.score, move.lprior, move.proposals)
    
    if (i > maxburn) {
      output <- output + reporter(nw)
      if (verbose) Sys.sleep(1)
    }
    if (trace) collect[[i]] <- reporter(nw)    
  }
  
  if (trace) return(list(mean=output / (Nsim - maxburn), trace=collect)) else return(output / (Nsim - maxburn))
}