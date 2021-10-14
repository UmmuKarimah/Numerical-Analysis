A <- matrix(c(1, 2, 3,
              2, 5, 3, 
              3, 1, 5), ncol = 3, nrow = 3, byrow = T)
b <- c(19, 28, 31)

x <- solve(A)%*%b
x

#######################################################################
gauss_jordan <- function(A, b){
  n <- nrow(A)
  c <- matrix(c(A, b), nrow = n)
  
  for(i in 1:(n-1)){
    for(j in (i+1) : n){
      pivot <- c[j,i]/c[i,i]
      for(k in 1 : (n+1)){
        c[j,k] <- c[j,k] - pivot*c[i,k]
      }
    }
  }
  
  for(i in n:2){
    for(k in (i-1):1){
      pivot <- c[k,i]/c[i,i]
      c[k,i] <- c[k,i] - pivot*c[i,i]
      c[k,(n+1)] <- c[k,(n+1)] - pivot*c[i,(n+1)]
    }
  }
  
  for(i in 1:n){
    x[i] <- c[i,(n+1)]/c[i,i]
    print(x[i])
  }
}
gauss_jordan(A, b)

############################################
library(matlib)

showEqn(A, b)
echelon(A, b)
echelon(A, b, verbose = T)
echelon(A, b, verbose = T, fraction = T)

#########################################
-4x + 4y = -1 
-2x + 2y -3z = -3
3x + y -3z = -3




A <- matrix(c(-4, 4, 0,
              -2, 2, -3,
              3, 1, -3), 3, 3, byrow = T)
b <- c(-1, -3, -3)
gauss_jordan(A,b)
echelon(C,d)
