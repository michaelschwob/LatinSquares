constructLatin <- function(n){
  ls <- matrix(0,n,n)
  ls[1,] <- 1:n
  for(i in 2:n){ #i is the row
    ls[i,]<-ls[i-1,]+1
    for(j in 1:n){
      if(ls[i,j]>n){
        ls[i,j] <- ls[i,j]-n
      }
    }
  }
  ls
}