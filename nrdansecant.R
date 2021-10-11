#metode newton rhapson
root_newton <- function(f, fp, x0, tol=1e-7, N=100){
  iter <- 0
  xold<-x0
  xnew <- xold + 10*tol
  while(abs(xnew-xold)>tol){
    iter <- iter+1
    if(iter>N){
      stop("No solutions found")
    }
    xold<-xnew
    xnew <- xold - f(xold)/fp(xold)
  }
  root<-xnew
  return(list(`function`=f, root=root, iter=iter))
}

root_newton(function(x){x-exp(-x)},
            function(x){1+exp(-x)},
            x0=0)

#metode secant
root_secant <- function(f, x, tol=1e-7, N=100){
  iter <- 0
  xold <- x
  fxold <- f(x)
  x <- xold+10*tol
  while(abs(x-xold)>tol){
    iter <- iter+1
    if(iter>N)
      stop("No solutions found")
    fx <- f(x)
    xnew <- x - fx*((x-xold)/(fx-fxold))
    xold <- x
    fxold <- fx
    x <- xnew
  }
  root<-xnew
  return(list(`function`=f, root=root, iter=iter))
}

root_secant(function(x){x-exp(-x)}, x=0)



#jawaban
f <- function(h){
  (h^3) + ((0.075-((1.2^2)/(2*9.81*(1.8^2)*(0.6^2))))*h^2)+ (1.2^2/(2*9.81*(1.8^2)))
}
root_secant(f, 0.6)
