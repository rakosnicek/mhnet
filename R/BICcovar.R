#' BIC with covariates
#' 
#' Yet to be edited and example given
#'
#' @param nw \code{mhnet} network 
#' @param dataset A data frame to use
#'
#' @return real number
#'
#' @keywords manip
#' 
#' @export 
#'
#' @examples
#' example <- read.csv("http://lion.img.cas.cz/user/simecek/chaos/Gm17403.csv")
#' 
#' data <- example[,5:7]
#' covs <- list(Sex = cbind(as.numeric(example$sex=="M")),
#'              Diet = cbind(as.numeric(example$diet=="HF")),
#'              Batch = cbind(as.numeric(example$batch==1)))
#' attr(data, "covs") <- covs
#' nw.start <- empty.network(c(names(attr(data, "covs")),names(data)))
#' BICcovar(nw.start, data)
#' blacklist = expand.grid(a=1:6, b=1:3)
#' all.edges <- function(nw) nw$adjmat
#' A <- mcmc(nw.start, data, all.edges, verbose=FALSE, trace=FALSE, score=BICcovar, blacklist=blacklist)

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