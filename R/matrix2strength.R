matrix2strength <- function(A, nodes) {
  output <- expand.grid(to=nodes,from=nodes)[,2:1]
  output <- subset(output, from!=to)
  output$strength <- NA
  output$direction <- NA
  for (i in 1:nrow(output)) {
    x <- which(nodes == output$from[i])
    y <- which(nodes == output$to[i])
    output$strength[i] <- A[x,y] + A[y,x]
    output$direction[i] <- A[x,y] / (A[x,y] + A[y,x])
  }
  class(output) <- c("bn.strength", "data.frame")
  attr(output,"mode") = "bootstrap"
  attr(output,"threshold") = 0.75
  output
}