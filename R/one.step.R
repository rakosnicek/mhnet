one.step <- function(nw, dataset, verbose = TRUE) {
  
  # make sure that no information is missing
  if (is.null(nw$proposals)) nw$proposals <- make.proposals(nw)
  if (is.null(nw$lprior)) nw$lprior <- log.prior(nw)
  if (is.null(nw$bnlearn)) {
    nw$bnlearn <- empty.graph(nw$nodes)
    amat(nw$bnlearn) <- nw$adjmat
  }
  if (is.null(nw$bic)) nw$bic <- myBIC(nw$bnlearn, dataset)
  
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
  if (is.null(nw2$proposals)) nw2$proposals <- make.proposals(nw2)
  if (is.null(nw2$lprior)) nw2$lprior <- log.prior(nw2)
  if (is.null(nw2$bnlearn)) {
    nw2$bnlearn <- empty.graph(nw2$nodes)
    amat(nw2$bnlearn) <- nw2$adjmat
  }
  if (is.null(nw2$bic)) nw2$bic <- myBIC(nw2$bnlearn, dataset)
  
  # calculate acceptace factor
  log.mh.alpha <- (nw2$bic - nw$bic)/2 + nw2$lprior - nw$lprior + log(nrow(nw$proposals)) - log(nrow(nw2$proposals))
  if (verbose) cat(paste("Alpha =", exp(log.mh.alpha), "\n"))
  
  if (runif(1) < exp(log.mh.alpha)) {
    if (verbose) cat(paste("Accepting", "\n"))
    if (verbose) plot(nw2$bnlearn)
    if (verbose) title(main = paste("BIC =", round(nw2$bic,2)))
    return(nw2)
  } else {
    if (verbose) cat(paste("Declining", "\n"))
    return(nw)
  }
}