import.bnlearn <- function(rg) {
  adjmat <- amat(rg)
  relmat <- diag(1, ncol(adjmat))
  dimnames(relmat) <- dimnames(adjmat)
  nsteps <- relmat
  while (!(all(nsteps==0))) {
    nsteps <- nsteps %*% adjmat
    relmat <- relmat + nsteps
  }
  max(relmat)
  
  list(nodes=names(rg$nodes), adjmat = adjmat, relmat = relmat)  
}