#' An Edge Removal
#' 
#' Remove a directed edge between two vertices.
#'
#' @param nw \code{mhnet} network 
#' @param a First vertex (an arrow pointing from) 
#' @param b Second vertex (an arrow pointing from)
#'
#' @return \code{mhnet} network
#'
#' @keywords manip
#' 
#' @seealso \code{\link{add.edge}}, \code{\link{reverse.edge}}
#'
#' @export
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' nw <- add.edge(nw, "A", "D")
#' nw <- remove.edge(nw, "A", "D")
#' try(remove.edge(nw, "A", "C"))

`remove.edge` <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 0) stop("The edge ",a," -> ",b," is not in the network")
  nw$adjmat[a,b] = 0
  nw$relmat <- nw$relmat - cbind(nw$relmat[,a]) %*% rbind(nw$relmat[b,])
  
  nw <- nw[c("nodes", "adjmat", "relmat")]
  class(nw) <- c("list", "mhnet")
  
  return(nw)
}