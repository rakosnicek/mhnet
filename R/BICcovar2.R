#' Yet another BIC with covariates
#' 
#' Dataset is a list, covartiates are att
#'
#' @param nw \code{mhnet} network 
#' @param dataset A list with covariates (matrix) as an attribute
#'
#' @return real number
#'
#' @keywords manip
#' 
#' @export 
#'
#' 
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