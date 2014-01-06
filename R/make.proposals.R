make.proposals <- function(nw, blacklist, whitelist) {
# blacklisting and whitelisting yet to be implemented
  removals <- which(nw$adjmat == 1, arr.ind=TRUE, useNames = FALSE)
  additions <- which(nw$adjmat==0 & t(nw$relmat)==0, arr.ind=TRUE, useNames = FALSE)
  reversals <- which(nw$adjmat==1 & nw$relmat==1, arr.ind=TRUE, useNames = FALSE)
  return(rbind(cbind(rep(1,nrow(removals)), removals), 
               cbind(rep(2,nrow(additions)), additions), 
               cbind(rep(3,nrow(reversals)), reversals)))
}