# mhnet

`mhnet` is R package for Metropolis-Hastings type simulations and inference for Bayesian Networks.

## Installation

```S
    # install.packages("devtools")
    library(devtools)
    install_github("rakosnicek/mhnet")
```

## Example(s)

Create and manipulate directed acyclic graphs

```S
  nw <- empty.network(LETTERS[1:4])
  nw <- add.edge(nw, "A", "D")
  nw <- reverse.edge(nw, "A", "D")
  nw <- remove.edge(nw, "D", "A")
  nw
```

Inference 

```S
  rg <- tabu(gaussian.test)
  nw <- import.bnlearn(rg)

  number.of.edges <- function(nw) sum(nw$adjmat)
  ne.dist <- mcmc(nw, gaussian.test, number.of.edges, verbose=FALSE, trace=TRUE)
  table(do.call("c", ne.dist$trace)[101:1000]) / 900

  all.edges <- function(nw) nw$adjmat
  A <- mcmc(nw, gaussian.test, all.edges, verbose=FALSE, trace=FALSE)
  strength.plot(export.bnlearn(nw), matrix2strength(A, nw$nodes))
```

Implementing own BIC function

```S
  example <- read.csv("H:\\Projects\\1309 lincRNA\\compilation\\sets\\Gm17403.csv")

  data <- example[,5:7]
  covs <- list(Sex = cbind(as.numeric(example$sex=="M")),
                  Diet = cbind(as.numeric(example$diet=="HF")),
                  Batch = cbind(as.numeric(example$batch==1)))
  attr(data, "covs") <- covs

  nw.start <- empty.network(c(names(attr(data, "covs")),names(data)))
  BICcovar(nw.start, data)
  blacklist = expand.grid(a=1:6, b=1:3)
  all.edges <- function(nw) nw$adjmat
  
  nw <- nw.start
  nw <- one.step(nw, data, verbose=TRUE, score=BICcovar, blacklist=blacklist)
```  
