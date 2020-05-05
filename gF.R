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
  
  
  ### Now, we need to construct the cyclic group F[x] given the irreducible polynomial "poly"
  F <- matrix(ncol=p,nrow=q^p) #initialize the vector F
  
  
  seqs = lapply(1:p, function(x) {rep((1:q) - 1, each = q^(x - 1))} )
  F <- do.call(cbind, seqs)
  
  ### Constructs an F[x], where each row contains the coefficients of the polynomial
  F <- as.matrix(expand.grid(lapply(1:p, function(i) 1:q-1L)))
  
  ### Here's another way to accomplish the same thing
  # seqs = lapply(1:p, function(x) {rep((1:q) - 1, each = q^(x - 1))} )
  # F <- do.call(cbind, seqs)
  
  #We need to save a list of coefficients (loc) to coerce into a polynomial
  #loc <- polynomial(coef=F[7,])
  
  ### We need to solve for x^p using the irreducible polynomial above 
  ### https://rosettacode.org/wiki/Polynomial_long_division#R
  polylongdiv <- function(n,d) {
    gd <- length(d)
    pv <- vector("numeric", length(n))
    pv[1:gd] <- d
    if ( length(n) >= gd ) {
      q <- c()
      while ( length(n) >= gd ) {
        q <- c(q, n[1]/pv[1])
        n <- n - pv * (n[1]/pv[1])
        n <- n[2:length(n)]
        pv <- pv[1:(length(pv)-1)]
      }
      list(q=q, r=n)
    } else {
      list(q=c(0), r=n)
    }
  }
  
  ### This is a utility function to print the polynomial.
  # print.polynomial <- function(p) {
  #   i <- length(p)-1
  #   for(a in p) {
  #     if ( i == 0 ) {
  #       cat(a, "\n")
  #     } else {
  #       cat(a, "x^", i, " + ", sep="")
  #     }
  #     i <- i - 1
  #   }
  # }
  
  
  ### Now we need to check for a generator
  for(i in (q+1):(q^p/2)){ #we can skip the first 1:q, since those are constants, and we don't need to calculate the second half of the set
    posgen <- F[i,] 
  }
  
  ### Now we construct the orthogonal mate Latin Square, which must be a List, since it's a 3-dimensional matrix
  gen <- c(0,1)
  LS <- list()
  sub <- 1 #this will change later
  
  for(i in 1:(q^p)){ #this is working well
    LS[[i]] <- F[i,]%%q
  }
  
  for(k in (q^p+1):q^(2*p)){ #y
    for(i in 1:q^p){
      for(j in 0:(q-2)){
        LS[[k]] <- (F[i,] + gen^(j+sub))%%q
      }
    }
  }
  
  # for(i in (q^p+1):((q^p)*p)){ #row indexing
  #   for(j in 1:(q^p)){ #column indexing
  #     LS[[i]] <- (F[j,] + gen^(sub+(i-1)))%%q
  #   }
  # }
  
  list(poly.x=poly.x,poly=poly,F=F,posgen=posgen,LS=LS)
}

