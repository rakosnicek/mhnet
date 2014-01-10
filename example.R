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

options(error=recover)
A <- mcmc(nw.start, data, all.edges, verbose=FALSE, trace=FALSE, score=BICcovar, blacklist=blacklist)

nw <- nw.start
nw <- one.step(nw, data, verbose=TRUE, score=BICcovar, blacklist=blacklist)