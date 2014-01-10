#' Matrix to Strength
#' 
#' Covert matrix with edge probabilities to \code{bn.strength} object.
#'
#' @param A Matrix with edge probability (occurence)
#' @param nodes Names of nodes (ordering correspond to A's rows and columns) 
#'
#' @return \code{bn.strength} object
#'
#' @keywords manip
#'
#' @export
#' 
#' @examples
#' 
#' rg <- tabu(gaussian.test)
#' nw <- import.bnlearn(rg)
#' 
#' all.edges <- function(nw) nw$adjmat
#' A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
#' strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))


`matrix2strength` <- function(A, nodes) {
  output <- expand.grid(to=nodes,from=nodes)[,2:1]
  output <- subset(output, from!=to)
  output$strength <- NA
  output$direction <- NA
  for (i in 1:nrow(output)) {
    x <- which(nodes == output$from[i])
    y <- which(nodes == output$to[i])
    output$strength[i] <- A[x,y] + A[y,x]
    output$direction[i] <- A[x,y] / (A[x,y] + A[y,x])
  }
  class(output) <- c("bn.strength", "data.frame")
  attr(output,"mode") = "bootstrap"
  attr(output,"threshold") = 0.75
  output
}