#' BIC with covariates
#' 
#' Calculates BIC for the given network and data frame.
#'
#' @param nw \code{mhnet} network 
#' @param dataset data frame with attributes given as 'covs' attribute
#'
#' @return real number
#'
#' @keywords manip
#' 
#' @export 
#'
#' @examples
#' data(Gm17403)
#' data <- Gm17403[,5:7]
#' covs <- list(Sex = cbind(as.numeric(Gm17403$sex=="M")),
#'              Diet = cbind(as.numeric(Gm17403$diet=="HF")),
#'              Batch = cbind(as.numeric(Gm17403$batch==1)))
#' attr(data, "covs") <- covs
#' 
#' nw.start <- empty.network(c(names(attr(data, "covs")), names(data)))
#' BICcovar(nw.start, data)

`BICcovar` <- function(nw, dataset) {
  lnobs <- log(nrow(dataset))
  ncovs <- length(attr(data, "covs"))
  
  output <- 0
  
  for (i in (ncovs+1):length(nw$nodes)) {
    sel <- which(nw$adjmat[,i]==1)
    
    if (length(sel) == 0) {
      ll <- logLik(lm(dataset[,i-ncovs] ~ 1))
    } else {
      tmp.data <- dataset[,(sel-ncovs)[sel>ncovs]]
      if (any(sel<=ncovs)) {
        covs <- do.call("cbind", attr(data, "covs")[sel[sel<=ncovs]])
        tmp.data <- cbind(covs,tmp.data)
      }  
      tmp.data <- as.data.frame(data)
      
      ll <- logLik(lm(I(dataset[,i-ncovs]) ~ ., data=tmp.data))
    }
    penalty <- lnobs * (attr(ll, "df")-1) / 2
    output <- output + as.numeric(ll) - penalty
  }
  
  return(output)
}