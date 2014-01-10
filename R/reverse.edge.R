#' An Edge Reversal
#' 
#' Reverse a directed edge between two vertices and check for acyclicity.
#'
#' @param nw \code{mhnet} network
#' @param a First vertex (an arrow pointing from)
#' @param b Second vertex (an arrow pointing from) 
#'
#' @return \code{mhnet} network
#'
#' @seealso \code{\link{add.edge}}, \code{\link{remove.edge}}
#'
#' @keywords manip
#'
#' @export
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' nw <- add.edge(nw, "A", "D")
#' reverse.edge(nw, "A", "D")
#' try(reverse.edge(nw, "D", "A"))

`reverse.edge` <- function(nw, a, b) {
  if (nw$adjmat[a,b] == 0) stop("The edge ",a," -> ",b," is not in the network")
  if (nw$relmat[a,b] >  1) stop("The reversal of ",a," -> ",b," would create a cycle")
  add.edge(remove.edge(nw, a, b), b, a)
}