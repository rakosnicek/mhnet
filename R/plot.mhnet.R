#' Plot the Graph
#' 
#' Convert \code{mhnet} network to \code{bnlearn} object and plot it
#'
#' @param x \code{mhnet} network
#' @param ... Other graphical parameters
#'
#' @return \code{NULL}
#'
#' @keywords hplot
#'
#' @export
#' 
#' @S3method plot mhnet
#' @method plot mhnet
#' 
#' @seealso \code{\link{matrix2strength}}
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' nw <- add.edge(nw, "A", "D")
#' plot(nw)

`plot.mhnet` <- function(x, ...) {
  require(Rgraphviz)
  rg <- export.bnlearn(x)
  bnlearn::graphviz.plot(rg, ...)
}