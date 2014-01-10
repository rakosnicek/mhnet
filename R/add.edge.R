#' An Edge Addition
#' 
#' Add a directed edge between two vertices and check for acycliclity.
#'
#' @param nw \code{mhnet} network 
#' @param a First vertex (an arrow pointing from)
#' @param b Second vertex (an arrow pointing from) 
#'
#' @return \code{mhnet} network
#'
#' @keywords manip
#' 
#' @seealso \code{\link{remove.edge}}, \code{\link{reverse.edge}}
#'
#' @export
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' ( nw <- add.edge(nw, "A", "D") )
#' try(add.edge(nw, "D", "A"))


`add.edge` <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 1) stop("The edge is already in the network")
  if (nw$relmat[b,a] >  0) stop("The edge ",a," -> ",b," would create a cycle")
  
  nw$adjmat[a,b] = 1
  nw$relmat <- nw$relmat + cbind(nw$relmat[,a]) %*% rbind(nw$relmat[b,])
  
  nw <- nw[c("nodes", "adjmat", "relmat")]
  class(nw) <- c("list", "mhnet")
  
  return(nw)
}