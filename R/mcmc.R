mcmc <- function(start.nw, dataset, reporter, Nsim=1000, burn=0.1, verbose=TRUE, trace=FALSE) {
  nw <- start.nw
  maxburn <- round(burn*Nsim)
  
  output <- reporter(start.nw) * 0
  if (trace) collect <- list()
  
  for (i in 1:Nsim) {
    nw <- one.step(nw, dataset, verbose)
    
    if (i > maxburn) {
      output <- output + reporter(nw)
      if (verbose) Sys.sleep(1)
    }
    if (trace) collect[[i]] <- reporter(nw)    
  }
  
  if (trace) return(list(mean=output / (Nsim - maxburn), trace=collect)) else return(output / (Nsim - maxburn))
}