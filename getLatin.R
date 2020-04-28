library("magic")

getLatin <- function(n) {
  if(n <= 3){
    red=1
  }
  if(n == 4){
    red=4
  }
  if(n==5){
    red=56
  }
  size <- factorial(n) * factorial(n-1)*red
  l <- list()
  l[[1]] <- rlatin(n)
  for(k in 2:size) {
    new <- rlatin(n)
    while(sum(sapply(l, function(x) any(identical(x, new)))) > 0) {
      new <- rlatin(n)
    }
    l[[k]] <- new
  }
  l
}