remove.edge <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 0) stop("The edge is not in the network")
  nw$adjmat[a,b] = 0
  nw$relmat <- nw$relmat - cbind(nw$relmat[,a]) %*% rbind(nw$relmat[b,])
  return(nw[c("nodes", "adjmat", "relmat")])
}