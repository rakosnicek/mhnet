#' Export Bnlearn Network
#' 
#' Convert \code{mnnet} network into \code{bnlearn} object.
#'
#' @param nw \code{mhnet} network
#'
#' @return \code{bnlearn} object
#'
#' @keywords manip
#'
#' @export
#' 
#' @seealso \code{\link{import.bnlearn}}
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' export.bnlearn(nw)

`export.bnlearn` <- function(nw) {
  rg <- bnlearn::empty.graph(nw$nodes)
  amat(rg) <- nw$adjmat
  rg
}