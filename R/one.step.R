#' Metropolis-Hastings Simulation
#' 
#' Do one step of Metropolis-Hastings algorithm: Propose a new graph and either accept or decline it.
#'
#' @param nw \code{mhnet} network  
#' @param dataset A data frame to use
#' @param score A function to calculate BIC
#' @param blacklist For move proposal, 2-column matrix or data.frame with forbidden edges (vertices as numbers)
#' @param whitelist For move proposal, 2-column matrix or data.frame with compulsory edges (vertices as numbers)
#' @param lprior Log of prior distribution of acyclic directed graph
#' @param move.score A function to adjust the score to addition / removal / reversal of an edge
#' @param move.lprior A function to adjust the log-prior to addition / removal / reversal of an edge
#' @param move.proposals A function to adjust the proposals to addition / removal / reversal of an edge
#' @param verbose If TRUE then report messanges and plots will be given
#' 
#' @return \code{mhnet} network
#'
#' @keywords models
#'
#' @export
#' 
#' @examples
#' rg <- tabu(learning.test)
#' nw <- import.bnlearn(rg)
#' one.step(nw, learning.test)

one.step <- function(nw, dataset, verbose = TRUE, score = BICbnnet, whitelist = NULL, blacklist = NULL, 
                     lprior=log.prior, move.score=NULL, move.lprior=NULL, move.proposals=NULL) {
  
  # make sure that no information is missing
  if (is.null(nw$proposals)) nw$proposals <- make.proposals(nw, whitelist, blacklist)
  if (is.null(nw$lprior)) nw$lprior <- lprior(nw)
  if (is.null(nw$bic)) nw$bic <- score(nw, dataset)
  
  # make random move
  move <- nw$proposals[sample(nrow(nw$proposals),1),]
  if (move[1] == 1) {
    if (verbose) cat(paste("Removing edge from", nw$nodes[move[2]], "to", nw$nodes[move[3]], "\n"))
    nw2 <- remove.edge(nw, move[2], move[3]) 
  }
  if (move[1] == 2) {
    if (verbose) cat(paste("Adding edge from", nw$nodes[move[2]], "to", nw$nodes[move[3]], "\n"))
    nw2 <- add.edge(nw, move[2], move[3])
  }
  if (move[1] == 3) {
    if (verbose) cat(paste("Reverting edge from", nw$nodes[move[2]], "to", nw$nodes[move[3]], "\n"))
    nw2 <- reverse.edge(nw, move[2], move[3])
  }
  
  # add missing information to the new proposal
  if (is.null(move.proposals)) nw2$proposals <- make.proposals(nw2) else nw2$proposals <- move.proposals(nw, move, whitelist, blacklist) 
  if (is.null(move.lprior)) nw2$lprior <- lprior(nw2) else nw2$lprior <- move.lprior(nw, move) 
  if (is.null(move.score)) nw2$bic <- score(nw2, dataset) else nw2$bic <- move.score(nw, move, dataset)
  
  # calculate acceptace factor
  log.mh.alpha <- (nw2$bic - nw$bic)/2 + nw2$lprior - nw$lprior + log(nrow(nw$proposals)) - log(nrow(nw2$proposals))
  if (verbose) cat(paste("Alpha =", format(exp(log.mh.alpha)), "\n"))
  
  if (log(runif(1)) < log.mh.alpha) {
    if (verbose) {
      cat(paste("Accepting", "\n"))
      plot(nw2$bnlearn)
      title(main = paste("BIC =", round(nw2$bic,2)))
    }  
    return(nw2)
  } else {
    if (verbose) cat(paste("Declining", "\n"))
    return(nw)
  }
}