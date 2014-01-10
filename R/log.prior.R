#' Prior Distribution over Graphs
#' 
#' Log-Prior (up to a constant) on a space of directed acyclic graphs
#'
#' @param nw \code{mhnet} network 
#'
#' @return real number
#'
#' @keywords manip
#' 
#' @seealso \code{\link{one.step}}
#' 
#' @examples
#' nw <- empty.network(LETTERS[1:4])
#' exp(mhnet:::log.prior(nw))

`log.prior` <- function(nw) -sum(lchoose(length(nw$nodes)-1, apply(nw$adjmat,2,sum)))