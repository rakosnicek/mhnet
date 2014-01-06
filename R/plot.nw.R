plot.nw <- function(x) {
  g <- empty.graph(x$nodes)
  amat(g) <- x$adjmat
  graphviz.plot(g)
}