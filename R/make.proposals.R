#' Make Proposals for Graph Modification
#' 
#' Add a directed edge between two vertices and check for acycliclity.
#'
#' @param nw A \code{mhnet} network
#' @param blacklist 2-column matrix or data.frame with forbidden edges (vertices as numbers)
#' @param whitelist 2-column matrix or data.frame with compulsory edges (vertices as numbers)
#'
#' @return Three column matrix. First column code for a type of modification 
#' (\code{1} is removal, \code{2} is addition, \code{3} is reversal). 
#' @return Second and Third columns define edges to be modified. 
#'
#' @keywords manip
#'
#' @export
#' 
#' @examples
#' rg <- bnlearn::tabu(learning.test)
#' nw <- import.bnlearn(rg)
#' make.proposals(nw)
#' make.proposals(nw, blacklist=data.frame(a=1:5,b=6), whitelist=data.frame(a=1,b=c(2,4)))

`make.proposals` <- function(nw, blacklist = NULL, whitelist = NULL) {

  matrix_diff <- function(x.1, x.2, ...){
  # returns rows of x.1 that are not in x.2    
    x.1p <- do.call("paste", as.data.frame(x.1))
    x.2p <- do.call("paste", as.data.frame(x.2))
    x.1[! x.1p %in% x.2p, ]
  }
  
  removals <- which(nw$adjmat == 1, arr.ind=TRUE, useNames = FALSE)
  additions <- which(nw$adjmat==0 & t(nw$relmat)==0, arr.ind=TRUE, useNames = FALSE)
  reversals <- which(nw$adjmat==1 & nw$relmat==1, arr.ind=TRUE, useNames = FALSE)
  
  if (!is.null(blacklist)) {
    additions <- matrix_diff(additions, blacklist)
    reversals <- matrix_diff(reversals, blacklist[,2:1])
  }
  if (!is.null(whitelist)) {
    removals <- matrix_diff(removals, whitelist)
    reversals <- matrix_diff(reversals, whitelist)
  }
  
  return(rbind(cbind(rep(1,nrow(removals)), removals), 
               cbind(rep(2,nrow(additions)), additions), 
               cbind(rep(3,nrow(reversals)), reversals)))
}