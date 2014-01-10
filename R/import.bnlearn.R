#' Import Bnlearn Network
#' 
#' Convert \code{bnlearn} object into \code{mnnet} network.
#'
#' @param rg \code{bnlearn} object 
#'
#' @return \code{mhnet} network
#'
#' @keywords manip
#'
#' @export
#' 
#' @seealso \code{\link{export.bnlearn}}
#' 
#' @examples
#' rg <- tabu(learning.test)
#' nw <- import.bnlearn(rg)
#' plot(nw)

`import.bnlearn` <- function(rg) {
  adjmat <- amat(rg)
  relmat <- diag(1, ncol(adjmat))
  dimnames(relmat) <- dimnames(adjmat)
  nsteps <- relmat
  while (!(all(nsteps==0))) {
    nsteps <- nsteps %*% adjmat
    relmat <- relmat + nsteps
  }
  max(relmat)
  
  nw = list(nodes=names(rg$nodes), adjmat = adjmat, relmat = relmat)
  class(nw) <- c("list", "mhnet")
  
  return(nw)
}