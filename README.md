# mhnet

`mhnet` is R package for Metropolis-Hastings type simulations and inference for Bayesian Networks.

## Installation

```S
    # install.packages("devtools")
    library(devtools)
    install_github("simecek/mhnet")
```

## Example(s)

Create and manipulate directed acyclic graphs

```S
  nw <- empty.network(LETTERS[1:4])
  nw <- add.edge(nw, "A", "D")
  plot(nw)
  nw <- reverse.edge(nw, "A", "D")
  plot(nw)
  nw <- remove.edge(nw, "D", "A")
  nw
```

Inference with BIC function specified by the user (Ecsit protein network)

```S
  
  # initial network
  data(Ecsit_network)
  nw <- empty.network(c("qEcsit", "qNdufaf1", "qTmem126b", "rEcsit",
          "rNdufaf1", "rTmem126b", "pEcsit", "pNdufaf1", "pTmem126b"))
  nw <- add.edge(nw, "qEcsit", "rEcsit")
  nw <- add.edge(nw, "rEcsit", "pEcsit")
  nw <- add.edge(nw, "qNdufaf1", "rNdufaf1")
  nw <- add.edge(nw, "rNdufaf1", "pNdufaf1")
  nw <- add.edge(nw, "qTmem126b", "rTmem126b")
  nw <- add.edge(nw, "rTmem126b", "pTmem126b")
  plot(nw)

  # BIC of the initial network
  BICcovar2(nw, Ecsit_network)

  # edges that are black/white-listed
  forbidden.edges = rbind(expand.grid(a=1:9,b=1:3)) 
  necessary.edges = NULL

  # try one step
  make.proposals(nw, blacklist = forbidden.edges, whitelist = necessary.edges)
  nw2 <- nw
  nw2 <- one.step(nw2, Ecsit_network, score=BICcovar2, blacklist=forbidden.edges, 
                whitelist=necessary.edges, verbose=TRUE)

  # try 100 steps observing all edges
  all.edges <- function(nw) nw$adjmat
  mcmc(nw, dataset=Ecsit_network, score=BICcovar2, all.edges, verbose=TRUE, trace=FALSE, Nsim=100, blacklist=forbidden.edges, whitelist=necessary.edges)

  # start cluster 
  library(parallel)
  library(doParallel)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # running 1000 steps, 10 restarts
  restarts=10
  A = foreach(i=1:restarts, .combine='+') %dopar% { 
    mcmc(nw, dataset=Ecsit_network, score=BICcovar2, all.edges, verbose=FALSE, trace=FALSE, Nsim=1000, blacklist=forbidden.edges, whitelist=necessary.edges)
  }

  # averaging the results
  A = A / restarts

  # network to be plotted
  nw3 <- nw
  nw3$adjmat <- round(A)

  # to solve numeric probles with weights
  strength.mat <- matrix2strength(A/1.001, nw3$nodes)
  strength.mat$direction[!is.finite(strength.mat$direction)] <- 0.001
  strength.mat$strength[strength.mat$strength==0] <- 0.001

  # final strength plot
  strength.plot(export.bnlearn(nw3), strength.mat)
  
  # stop the cluster
  stopCluster(cl)
```

Inference with data frame and covariates (Gm17403 lincRNA network)

```S
  data(Gm17403)

  data <- Gm17403[,5:7]
  covs <- list(Sex = cbind(as.numeric(Gm17403$sex=="M")),
                  Diet = cbind(as.numeric(Gm17403$diet=="HF")),
                  Batch = cbind(as.numeric(Gm17403$batch==1)))
  attr(data, "covs") <- covs

  nw.start <- empty.network(c(names(attr(data, "covs")),names(data)))
  BICcovar(nw.start, data)
  blacklist = expand.grid(a=1:6, b=1:3)
  all.edges <- function(nw) nw$adjmat
  
  nw <- nw.start
  nw <- one.step(nw, data, verbose=TRUE, score=BICcovar, blacklist=blacklist)
  mcmc(nw, dataset=data, score=BICcovar, all.edges, verbose=TRUE, trace=FALSE, Nsim=30, blacklist=forbidden.edges)
```  


Inference on distribution of edges 

```S
  rg <- tabu(gaussian.test)
  nw <- import.bnlearn(rg)
  plot(nw)

  number.of.edges <- function(nw) sum(nw$adjmat)
  ne.dist <- mcmc(nw, gaussian.test, number.of.edges, verbose=FALSE, trace=TRUE)
  table(do.call("c", ne.dist$trace)[101:1000]) / 900

  all.edges <- function(nw) nw$adjmat
  A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
  strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))
```

