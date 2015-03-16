#' BIC for data represented as a list
#' 
#' Calculates BIC for the given network and data list.
#'
#' @param nw \code{mhnet} network 
#' @param dataset A list with data with elements in the same order as 'nw' nodes
#'
#' @return real number
#'
#' @keywords manip
#' 
#' @export 
#'
#' @example
#' data(Ecsit_network)
#' nw <- empty.network(c("qEcsit", "qNdufaf1", "qTmem126b", "rEcsit", 
#'         "rNdufaf1", "rTmem126b", "pEcsit", "pNdufaf1", "pTmem126b"))
#' nw <- add.edge(nw, "qEcsit", "rEcsit")
#' plot(nw)
#' 
#' BICcovar2(nw, Ecsit_network)

`BICcovar2` <- function (nw, dataset) 
{
  # log number of observations
  nobs <- NROW(dataset[[1]])
  output <- 0
  
  covs <- attr(dataset, "covs")
  if (is.null(covs)) covs <- rep(1, nobs)
  
  for (i in 1:length(nw$nodes)) {
    
    # skip multidimensional nodes
    if (NCOL(dataset[[i]])>1) {
      # check that it is parentless
      if (any(nw$adjmat[, i] == 1)) {
        warning("Multivariate distribution with parent.")
      }  
      next
    }
    
    sel <- which(nw$adjmat[, i] == 1)
    if (length(sel) == 0) {
      ll <- logLik(lm(dataset[[i]] ~ covs))
    }
    else {
      tmp.data <- do.call("cbind", dataset[sel])
      ll <- logLik(lm(dataset[[i]] ~ tmp.data + covs))
    }
    penalty <- log(nobs) * (attr(ll, "df") - 1)/2
    output <- output + as.numeric(ll) - penalty
  }
  return(output)
}