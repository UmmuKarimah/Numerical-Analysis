jacobi <- function(a, b, tol=1e-7, maxiter=100){
  n <- length(b)
  iter <- 0
  Dinv <- diag(1/diag(a))
  R <- a-diag(diag(a))
  x <- rep(0,n)
  x_new <- rep(tol, n)
  while(sqrt(sum(x_new-x)^2)>tol){
    if(iter>maxiter){
      warning("iterasi maksimum tercapai")
      break
    }
    
    x <- x_new
    x_new <- Dinv %*% (b - R %*% x)
    iter <- iter+1
  }
  return(list(X = x_new, iter=iter))
}
A <- matrix(c(27,6,1,6,15,1,-1,2,54), 3)
b <- c(85,72,110)
jacobi(A,b)
#install.packages("pracma","Rlinsolve")
library(pracma)
A<-matrix(c(10,-1,2,0,
            -1,11,-1,3,
            2,-1,10,-1,
            0,3,-1,8),ncol = 4,nrow = 4)
b<-c(6,25,-11,15)
x0<-c(0,0,0,0)
itersolve(A, b, x0 = x0, tol = 1e-8, method = "Jacobi")

AA<-matrix(c(2,5,1,7),ncol = 2,nrow = 2)
bb<-c(11,13)
x0<-c(1,1)
itersolve(AA, bb, x0 = x0, tol = 1e-8, method = "Jacobi")

A1<-matrix(c(1,0,0,
             -2,1,0,
             3,3,1),ncol = 3,nrow = 3)
b1<-c(9,-4,17)
x0<-c(1,1,1)
itersolve(A1, b1, x0 = x0, tol = 1e-8, method = "Jacobi")

A2<-matrix(c(1,2,3,
             -1,3,-1,
             1,-1,9),ncol = 3,nrow = 3)
b2<-c(8,-2,9)
itersolve(A2, b2, x0 = x0, tol = 1e-8, method = "Jacobi")

library(Rlinsolve)
set.seed(100)
A1 = matrix(rnorm(10*5),nrow=10)
x = rnorm(5)
b = A1%*%x

out1 = lsolve.jacobi(A1,b,weight=1,verbose=FALSE)   # unweighted
out2 = lsolve.jacobi(A1,b,verbose=FALSE)            # weight of 0.66
out3 = lsolve.jacobi(A1,b,weight=0.5,verbose=FALSE) # weight of 0.50
print("* lsolve.jacobi : overdetermined case example")
print(paste("*   error for unweighted    Jacobi case : ",norm(out1$x-x)))
print(paste("*   error for 0.66 weighted Jacobi case : ",norm(out2$x-x)))
print(paste("*   error for 0.50 weighted Jacobi case : ",norm(out3$x-x)))
