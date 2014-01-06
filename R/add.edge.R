add.edge <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 1) stop("The edge is already in the network")
  if (nw$relmat[b,a] >  0) stop("The edge would created a cycle")
  
  nw$adjmat[a,b] = 1
  nw$relmat <- nw$relmat + cbind(nw$relmat[,a]) %*% rbind(nw$relmat[b,])
  return(nw[c("nodes", "adjmat", "relmat")])
}