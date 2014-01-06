myBIC <- function(nw, dataset) {
  lnobs <- log(nrow(dataset))
  
  output <- 0
  
  for (i in 1:length(nw$nodes)) {
    sel <- which(nw$adjmat[,i]==1)
    if (class(dataset[,i]) == "factor" & length(sel)==0) next
    if (length(sel) == 0) {
      ll <- logLik(lm(dataset[,i] ~ 1))
    } else {
      ll <- logLik(lm(dataset[,i] ~ ., data=dataset[sel]))
    }
    penalty <- lnobs * (attr(ll, "df")-1) / 2
    output <- output + as.numeric(ll) - penalty
  }
  
  return(output)
}