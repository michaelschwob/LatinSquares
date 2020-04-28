library("polynom")

gf <- function(q,p){
  
  ### First we need to create an irreducible polynomial of degree p
  poly <- polynomial(coef=c(floor(runif(p,min=0,max=q)),1)) #This generates the first polynomial of degree p with coefficients ranging between the integer values of 0,1,...,q
  for(i in 1:(q^5*p)){ #we generate/check our polynomial a sufficient amount of times to ensure that we get an irreducible polynomial
    poly.x <- as.function(poly) #we coerce the generated polynomial into a function
    for(j in 0:q){ #we check if the generated polynomial is irreducible
      if(poly.x(j) %% q == 0){ #if we find that a polynomial is reducible, then we generate a new polynomial
        poly <- polynomial(coef=c(floor(runif(p,min=0,max=q)),1)) #...and go through the loop again
      }
    }
  }
  list(poly.x=poly.x,poly=poly)
  
  ### Now, we need to construct the cyclic group F[x] given the irreducible polynomial "poly"
  F <- c(rep(0,q^p)) #initialize the vector F
  for(j in 0:(q^p-1)){
    #F[j] <- polynomial(coef = c(rep(j,p)))
    F[j] <- c(rep(0,3))  
  }
  F
}