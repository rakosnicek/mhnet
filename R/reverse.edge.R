reverse.edge <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 0) stop("The edge is not in the network")
  if (nw$relmat[a,b] >  1) stop("The reversal would create a cycle")
  add.edge(remove.edge(nw, a, b), b, a)
}