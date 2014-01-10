#' BIC as implemented in bnlearn
#' 
#' Convert \code{mnnet} network into \code{bnlearn} object and use \code{BIC.bn} to calculate the BIC.
#'
#' @param nw \code{mnnet} object
#' @param dataset A data frame to use 
#'
#' @note BIC is not calculated as usual -2*log-likelihood + log(n)*npar, 
#' but as log-likelihood - log(n)*npar/2
#'
#' @return real number
#'
#' @keywords manip
#'
#' @export
#' 
#' @seealso \code{\link{one.step}}
#' 
#' @examples
#' rg <- tabu(learning.test)
#' nw <- import.bnlearn(rg)
#' BIC(rg, learning.test)
#' BICbnnet(nw, learning.test)

`BICbnnet` <- function(nw, dataset) {
  rg <- export.bnlearn(nw)
  BIC(rg, dataset)
}