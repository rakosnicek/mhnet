#' Empty Network
#' 
#' Create a network with given nodes and no edges.
#'
#' @param nodes Names of nodes (character vector)
#'
#' @return \code{mhnet} network with no edges
#'
#' @keywords manip
#' 
#' @seealso \code{\link{remove.edge}}, \code{\link{reverse.edge}}, \code{\link{add.edge}}
#'
#' @export
#' 
#' @examples
#' empty.network(LETTERS[1:4])

`empty.network` <- function(nodes) {
  N <- length(nodes)
  adjmat <- matrix(0, N, N)
  relmat <- diag(N)
  
  rownames(adjmat) <- colnames(adjmat) <- nodes
  rownames(relmat) <- colnames(relmat) <- nodes
  
  nw <- list(nodes = nodes, adjmat = adjmat, relmat = relmat)
  class(nw) <- c("list", "mhnet")
  
  return(nw)
}