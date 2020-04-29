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
  F <- matrix(ncol=p,nrow=q^p) #initialize the vector F
  
  
  seqs = lapply(1:p, function(x) {rep((1:q) - 1, each = q^(x - 1))} )
  F <- do.call(cbind, seqs)
  
  ### Constructs an unordered F[x], where each row contains the coefficients of the polynomial
  #F <- as.matrix(expand.grid(lapply(1:p, function(i) 1:q-1L)))
  
  F
}
